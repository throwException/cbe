/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <base/allocator_avl.h>
#include <base/attached_ram_dataspace.h>
#include <base/attached_rom_dataspace.h>
#include <base/component.h>
#include <base/heap.h>
#include <base/thread.h>
#include <block/request_stream.h>
#include <root/root.h>
#include <terminal_session/connection.h>
#include <util/bit_allocator.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/util.h>
#include <cbe/external_crypto.h>

/* cbe-check includes */
#include <cbe_check/library.h>

/* cbe-dump includes */
#include <cbe_dump/library.h>
#include <cbe_dump/configuration.h>

/* cbe-init includes */
#include <cbe_init/library.h>
#include <cbe_init/configuration.h>

/* repo includes */
#include <util/sha256_4k.h>

/* local includes */
#include <util.h>

using namespace Genode;

static char const *to_string(Block::Operation::Type type)
{
	struct Unknown_operation_type : Exception { };
	switch (type) {
	case Block::Operation::Type::INVALID: return "invalid";
	case Block::Operation::Type::READ: return "read";
	case Block::Operation::Type::WRITE: return "write";
	case Block::Operation::Type::SYNC: return "sync";
	case Block::Operation::Type::TRIM: return "trim";
	}
	throw Unknown_operation_type();
}

static char const *to_string(Cbe::Request::Operation op)
{
	struct Unknown_operation_type : Exception { };
	switch (op) {
	case Cbe::Request::Operation::INVALID: return "invalid";
	case Cbe::Request::Operation::READ: return "read";
	case Cbe::Request::Operation::WRITE: return "write";
	case Cbe::Request::Operation::SYNC: return "sync";
	}
	throw Unknown_operation_type();
}

namespace Cbe {

	struct Block_session_component;
	struct Main;

	/**
	 * Convert CBE primitive to CBE request
	 *
	 * \param p refrence to primitive
	 *
	 * \return Cbe::Request object
	 */
	static inline Cbe::Request convert_from(Cbe::Primitive const &p)
	{
		auto convert_op = [&] (Cbe::Primitive::Operation o) {
			switch (o) {
			case Cbe::Primitive::Operation::INVALID: return Cbe::Request::Operation::INVALID;
			case Cbe::Primitive::Operation::READ:    return Cbe::Request::Operation::READ;
			case Cbe::Primitive::Operation::WRITE:   return Cbe::Request::Operation::WRITE;
			case Cbe::Primitive::Operation::SYNC:    return Cbe::Request::Operation::SYNC;
			}
			return Cbe::Request::Operation::INVALID;
		};
		return Cbe::Request {
			.operation    = convert_op(p.operation),
			.success      = Cbe::Request::Success::FALSE,
			.block_number = p.block_number,
			.offset       = 0,
			.count        = 1,
			.tag          = 0,
		};
	}

	/**
	 * Convert CBE request
	 *
	 * \param r  reference to CBE request object
	 *
	 * \return  Block request object
	 */
	static inline Block::Request convert_from(Cbe::Request const &r)
	{
		auto convert_op = [&] (Cbe::Request::Operation o) {
			switch (o) {
			case Cbe::Request::Operation::INVALID: return Block::Operation::Type::INVALID;
			case Cbe::Request::Operation::READ:    return Block::Operation::Type::READ;
			case Cbe::Request::Operation::WRITE:   return Block::Operation::Type::WRITE;
			case Cbe::Request::Operation::SYNC:    return Block::Operation::Type::SYNC;
			}
			return Block::Operation::Type::INVALID;
		};
		auto convert_success = [&] (Cbe::Request::Success s) {
			return s == Cbe::Request::Success::TRUE ? true : false;
		};
		return Block::Request {
			.operation = {
				.type         = convert_op(r.operation),
				.block_number = r.block_number,
				.count        = r.count,
			},
			.success   = convert_success(r.success),
			.offset    = (Block::off_t)r.offset,
			.tag       = { .value = r.tag },
		};
	}

	/**
	 * Convert Block request
	 *
	 * \param r  reference to Block request object
	 *
	 * \return  CBE request object
	 */
	static inline Cbe::Request convert_to(Block::Request const &r)
	{
		auto convert_op = [&] (Block::Operation::Type t) {
			switch (t) {
			case Block::Operation::Type::INVALID: return Cbe::Request::Operation::INVALID;
			case Block::Operation::Type::READ:    return Cbe::Request::Operation::READ;
			case Block::Operation::Type::WRITE:   return Cbe::Request::Operation::WRITE;
			case Block::Operation::Type::SYNC:    return Cbe::Request::Operation::SYNC;
			case Block::Operation::Type::TRIM:    return Cbe::Request::Operation::INVALID; // XXX fix
			}
			return Cbe::Request::Operation::INVALID;
		};
		auto convert_success = [&] (bool success) {
			return success ? Cbe::Request::Success::TRUE : Cbe::Request::Success::FALSE;
		};

		return Cbe::Request {
			.operation    = convert_op(r.operation.type),
			.success      = convert_success(r.success),
			.block_number = r.operation.block_number,
			.offset       = (Genode::uint64_t)r.offset,
			.count        = (Genode::uint32_t)r.operation.count,
			.tag          = (Genode::uint32_t)r.tag.value,
		};
	}

} /* namespace Cbe */


struct Create_snapshot
{
	Cbe::Token token;
	bool       quarantine;

	Create_snapshot(Cbe::Token token,
	                bool       quarantine)
	:
		token      { token },
		quarantine { quarantine }
	{ }
};

struct Discard_snapshot
{
	uint64_t id;

	Discard_snapshot(uint64_t id)
	:
		id { id }
	{ }
};


struct Cbe::Block_session_component
{
	enum class Response { ACCEPTED, REJECTED, RETRY };

	class Payload : Noncopyable
	{
		private:

			friend class Block_session_component;

			Genode::addr_t const _base;

			Payload(Genode::addr_t base)
			:
				_base(base)
			{ }

		public:

			template <typename FN>
			void with_content(Block::Request , FN const &fn) const
			{
				fn((void *)_base, BLOCK_SIZE);
			}
	};

	struct Test : Fifo<Test>::Element
	{
		enum Type
		{
			INVALID,
			REQUEST,
			CREATE_SNAPSHOT,
			DISCARD_SNAPSHOT,
			INITIALIZE,
			CHECK,
			DUMP
		};

		Type                                   type             { INVALID };
		Constructible<Block::Request>          request          { };
		Constructible<Create_snapshot>         create_snapshot  { };
		Constructible<Discard_snapshot>        discard_snapshot { };
		Constructible<Cbe_init::Configuration> initialize       { };
		Constructible<Cbe_dump::Configuration> dump             { };

		Test () { }

		Test (Test &other)
		:
			type { other.type }
		{
			if (other.request.constructed()) {
				request.construct(*other.request);
			}
			if (other.create_snapshot.constructed()) {
				create_snapshot.construct(*other.create_snapshot);
			}
			if (other.discard_snapshot.constructed()) {
				discard_snapshot.construct(*other.discard_snapshot);
			}
			if (other.initialize.constructed()) {
				initialize.construct(*other.initialize);
			}
			if (other.dump.constructed()) {
				dump.construct(*other.dump);
			}
		}
	};

	class Ack : Noncopyable
	{
		private:

			Block_session_component &_component;

		public:

			void submit(Block::Request request)
			{
				_component.submit_ack(request);
			}

			Ack(Block_session_component &component)
			:
				_component { component }
			{ }
	};

	Env                    &_env;
	Attached_rom_dataspace &_config_rom;
	Allocator              &_alloc;
	Fifo<Test>              _test_queue { };
	Constructible<Test>     _test_in_progress { };
	unsigned long           _nr_of_failed_tests { 0 };
	Block_data              _blk_data { };

	void _read_request_node (Xml_node const &node)
	{
		struct Bad_request_node : Exception { };
		try {
			Block::Operation op;
			if (node.attribute("type").has_value("read")) {
				op.type = Block::Operation::Type::READ;
			} else if (node.attribute("type").has_value("write")) {
				op.type = Block::Operation::Type::WRITE;
			} else if (node.attribute("type").has_value("sync")) {
				op.type = Block::Operation::Type::SYNC;
			} else {
				error("request node has bad type attribute");
				throw Bad_request_node();
			}
			if (!node.attribute("lba").value(op.block_number)) {
				error("request node has bad lba attribute");
				throw Bad_request_node();
			}
			if (!node.attribute("count").value(op.count)) {
				error("request node has bad count attribute");
				throw Bad_request_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::REQUEST;
			test.request.construct();
			test.request->operation = op;
			test.request->success = false;
			test.request->offset = 0;
			test.request->tag.value = 0;
			_test_queue.enqueue(test);
		} catch (Xml_node::Nonexistent_attribute) {
			error("request node misses attribute");
			throw Bad_request_node();
		}
	}

	void _read_create_snapshot_node(Xml_node const &node)
	{
		struct Bad_create_snapshot_node : Exception { };
		try {
			Cbe::Token token { 0 };
			bool quarantine { false };
			if (!node.attribute("id").value(token.value)) {
				error("create-snapshot node has bad id attribute");
				throw Bad_create_snapshot_node();
			}
			if (!node.attribute("quarantine").value(quarantine)) {
				error("create-snapshot node has bad quarantine attribute");
				throw Bad_create_snapshot_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::CREATE_SNAPSHOT;
			test.create_snapshot.construct(token, quarantine);
			_test_queue.enqueue(test);
		} catch (Xml_node::Nonexistent_attribute) {
			error("create-snapshot node misses attribute");
			throw Bad_create_snapshot_node();
		}
	}

	void _read_discard_snapshot_node(Xml_node const &node)
	{
		struct Bad_discard_snapshot_node : Exception { };
		try {
			uint64_t id { 0 };
			if (!node.attribute("id").value(id)) {
				error("discard-snapshot node has bad id attribute");
				throw Bad_discard_snapshot_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::DISCARD_SNAPSHOT;
			test.discard_snapshot.construct(id);
			_test_queue.enqueue(test);
		} catch (Xml_node::Nonexistent_attribute) {
			error("discard-snapshot node misses attribute");
			throw Bad_discard_snapshot_node();
		}
	}

	void _read_replay_node (Xml_node const &node)
	{
		struct Bad_replay_sub_node : Exception { };
		try {
			Xml_node sub_node = node.sub_node();
			while (1) {
				if (sub_node.has_type("request")) {
					_read_request_node(sub_node);
				} else if (sub_node.has_type("create-snapshot")) {
					_read_create_snapshot_node(sub_node);
				} else if (sub_node.has_type("discard-snapshot")) {
					_read_discard_snapshot_node(sub_node);
				} else {
					error("replay sub-node has bad type");
					throw Bad_replay_sub_node();
				}
				sub_node = sub_node.next();
			}
		} catch (Xml_node::Nonexistent_sub_node) { }
	}

	void _read_check_node ()
	{
		Test &test = *new (_alloc) Test;
		test.type = Test::CHECK;
		_test_queue.enqueue(test);
	}

	void _read_dump_node (Xml_node const &node)
	{
		Test &test = *new (_alloc) Test;
		test.type = Test::DUMP;
		test.dump.construct(node);
		_test_queue.enqueue(test);
	}

	void _read_initialize_node (Xml_node const &node)
	{
		try {
			Test &test = *new (_alloc) Test;
			test.type = Test::INITIALIZE;
			test.initialize.construct(node);
			_test_queue.enqueue(test);
		} catch (Cbe_init::Configuration::Invalid) {
			error("bad initialize node");
			throw;
		}
	}

	void _read_tests_node (Xml_node const &node)
	{
		try {
			Xml_node sub_node = node.sub_node();
			while (1) {
				if (sub_node.has_type("replay")) {
					_read_replay_node(sub_node);
				}
				else if (sub_node.has_type("initialize")) {
					_read_initialize_node(sub_node);
				}
				else if (sub_node.has_type("check")) {
					_read_check_node();
				}
				else if (sub_node.has_type("dump")) {
					_read_dump_node(sub_node);
				}
				sub_node = sub_node.next();
			}
		} catch (Xml_node::Nonexistent_sub_node) { }
	}

	void submit_ack(Block::Request request)
	{
		if (!request.success) {
			_nr_of_failed_tests++;
			log("request failed: op ",
				to_string(request.operation.type), ", vba ",
				request.operation.block_number, ", cnt ",
				request.operation.count);
		} else {
			log("request succeeded: op ",
				to_string(request.operation.type), ", vba ",
				request.operation.block_number, ", cnt ",
				request.operation.count);
		}
		_test_in_progress.destruct();
	}

	Block_session_component(Env                    &env,
	                        Attached_rom_dataspace &config_rom,
	                        Allocator              &alloc)
	:
		_env        { env },
		_config_rom { config_rom },
		_alloc      { alloc }
	{
		_config_rom.xml().with_sub_node("tests", [&] (Xml_node const &node) {
			_read_tests_node(node);
		});
	}

	~Block_session_component() { }

	template <typename FN>
	void with_requests(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_is_request { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::REQUEST) {
				head_is_request = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_is_request) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});

		log("request started: op ",
			to_string(_test_in_progress->request->operation.type), ", vba ",
			_test_in_progress->request->operation.block_number, ", cnt ",
			_test_in_progress->request->operation.count);

		fn(*_test_in_progress->request);
	}

	template <typename FN>
	void try_acknowledge(FN const &fn)
	{
		Ack ack { *this };
		fn(ack);
	}

	template <typename FN>
	void with_payload(FN const &fn) const
	{
		Payload payload { (addr_t)&_blk_data };
		fn(payload);
	}

	void wakeup_client_if_needed() { }

	template <typename FN>
	void with_create_snapshot(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_is_snap_creation { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::CREATE_SNAPSHOT) {
				head_is_snap_creation = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_is_snap_creation) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("create snapshot started: token ",
		    _test_in_progress->create_snapshot->token,
		    ", quarantine ",
		    _test_in_progress->create_snapshot->quarantine);

		fn(*_test_in_progress->create_snapshot);
	}

	void create_snapshot_failed()
	{
		_nr_of_failed_tests++;
		log("create snapshot failed: token ",
		    _test_in_progress->create_snapshot->token,
		    ", quarantine ",
		    _test_in_progress->create_snapshot->quarantine);
		_test_in_progress.destruct();
	}

	void create_snapshot_done(Cbe::Token token, Cbe::Snapshot_ID id)
	{
		bool const success =
			token.value == _test_in_progress->create_snapshot->token.value;

		if (success) {
			log("create snapshot succeeded: token ",
			    _test_in_progress->create_snapshot->token,
			    ", id ", id,
			    ", quarantine ",
			    _test_in_progress->create_snapshot->quarantine);
		} else {
			_nr_of_failed_tests++;
			log("create snapshot failed: token ",
			    _test_in_progress->create_snapshot->token,
			    ", quarantine ",
			    _test_in_progress->create_snapshot->quarantine);
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_discard_snapshot(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_is_snap_discard { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::DISCARD_SNAPSHOT) {
				head_is_snap_discard = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_is_snap_discard) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("discard snapshot started: id ",
		    _test_in_progress->discard_snapshot->id);

		fn(*_test_in_progress->discard_snapshot);
	}

	void discard_snapshot_done(bool success)
	{
		if (success) {
			log("discard snapshot succeeded: id ",
		    _test_in_progress->discard_snapshot->id);
		} else {
			_nr_of_failed_tests++;
			log("discard snapshot failed: id ",
		    _test_in_progress->discard_snapshot->id);
		}
		_test_in_progress.destruct();
	}

	bool cbe_request_next() const
	{
		bool result = false;
		_test_queue.head([&] (Test &test) {
			result |= test.type == Test::REQUEST;
			result |= test.type == Test::CREATE_SNAPSHOT;
			result |= test.type == Test::DISCARD_SNAPSHOT;
		});
		return result;
	}

	template <typename FN>
	void with_check(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_type_fits { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::CHECK) {
				head_type_fits = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_type_fits) {
			return;
		}

		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});

		log("check started");
		fn();
	}

	void check_done(bool success)
	{
		if (success) {
			log("check succeeded");
		} else {
			_nr_of_failed_tests++;
			log("check failed");
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_dump(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_type_fits { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::DUMP) {
				head_type_fits = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_type_fits) {
			return;
		}

		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});

		log("dump started",
		    ": unused_nodes ",           _test_in_progress->dump->unused_nodes(),
		    ", max_superblocks ",        _test_in_progress->dump->max_superblocks(),
		    ", max_snapshots ",          _test_in_progress->dump->max_snapshots(),
		    ", vbd ",                    _test_in_progress->dump->vbd(),
		    ", vbd_pba_filter_enabled ", _test_in_progress->dump->vbd_pba_filter_enabled(),
		    ", vbd_pba_filter ",         _test_in_progress->dump->vbd_pba_filter(),
		    ", vbd_vba_filter_enabled ", _test_in_progress->dump->vbd_vba_filter_enabled(),
		    ", vbd_vba_filter ",         _test_in_progress->dump->vbd_vba_filter(),
		    ", free_tree ",              _test_in_progress->dump->free_tree(),
		    ", hashes ",                 _test_in_progress->dump->hashes());

		fn(*_test_in_progress->dump);
	}

	void dump_done(bool success)
	{
		if (success) {
			log("dump succeeded");
		} else {
			_nr_of_failed_tests++;
			log("dump failed");
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_initialize(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_type_fits { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::INITIALIZE) {
				head_type_fits = true;
			}
		});
		if (!head_available) {
			log("all tests finished (", _nr_of_failed_tests, " tests failed)");
			if (_nr_of_failed_tests > 0) {
				_env.parent().exit(-1);
			} else {
				_env.parent().exit(0);
			}
		}
		if (!head_type_fits) {
			return;
		}

		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});

		log("initialize started:  vbd:",
		    " lvls ", _test_in_progress->initialize->vbd_nr_of_lvls(),
		    ", degr ", _test_in_progress->initialize->vbd_nr_of_children(),
		    ", leafs ", _test_in_progress->initialize->vbd_nr_of_leafs(),
		    "  ft:",
		    " lvls ", _test_in_progress->initialize->ft_nr_of_lvls(),
		    ", degr ", _test_in_progress->initialize->ft_nr_of_children(),
		    ", leafs ", _test_in_progress->initialize->ft_nr_of_leafs());

		fn(*_test_in_progress->initialize);
	}

	void initialize_done(bool success)
	{
		if (success) {
			log("initialize succeeded");
		} else {
			_nr_of_failed_tests++;
			log("initialize failed");
		}
		_test_in_progress.destruct();
	}
};

struct A1 { A1 () {Genode::log(__func__,__LINE__);}};
struct A2 { A2 () {Genode::log(__func__,__LINE__);}};
struct A3 { A3 () {Genode::log(__func__,__LINE__);}};
struct A4 { A4 () {Genode::log(__func__,__LINE__);}};

class Cbe::Main
{
	private:

		enum State { INVALID, CBE, CBE_INIT, CBE_CHECK, CBE_DUMP };

		static char const *state_to_string(State s)
		{
			switch (s) {
			case CBE: return "CBE";
			case CBE_INIT: return "INIT";
			case CBE_CHECK: return "CHECK";
			case CBE_DUMP: return "DUMP";
			case INVALID: return "INVALID";
			default: throw -1;
			}
		}

		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };

		Env                                    &_env;
		Attached_rom_dataspace                  _config_rom              { _env, "config" };
		bool                                    _verbose_back_end_io     { false };
		bool                                    _verbose_back_end_crypto { false };
		Constructible<Block_session_component>  _block_session           { };
		Heap                                    _heap                    { _env.ram(), _env.rm() };
		Allocator_avl                           _blk_alloc               { &_heap };
		Block::Connection<>                     _blk                     { _env, &_blk_alloc, TX_BUF_SIZE };
		Timer::Connection                       _timer                   { _env };
		Constructible<Cbe::Library>             _cbe                     { };
		Cbe_check::Library                      _cbe_check               { };
		Cbe_dump::Library                       _cbe_dump                { };
		Cbe_init::Library                       _cbe_init                { };
		Cbe::Request                            _blk_req                 { };
		Io_buffer                               _blk_buf                 { };
		Crypto_plain_buffer                     _crypto_plain_buf        { };
		Crypto_cipher_buffer                    _crypto_cipher_buf       { };
		External::Crypto                        _crypto                  { };
		State                                   _state                   { INVALID };
		Cbe::Superblocks                        _super_blocks            { };
		uint64_t                                _nr_of_sbs_requested     { 0 };
		uint64_t                                _nr_of_sbs_available     { 0 };
		Signal_handler<Main>                    _request_handler         { _env.ep(), *this, &Main::_execute };
		Cbe::Snapshot_ID                        _creating_snapshot_id    { 0, false };
		bool                                    _creating_snapshot       { false };
		Cbe::Snapshot_ID                        _discarding_snapshot_id  { 0, false };

		void _execute_cbe_check (bool &progress)
		{
			_cbe_check.execute(_blk_buf);
			if (_cbe_check.execute_progress()) {
				progress = true;
			}

			struct Invalid_io_request : Exception { };

			while (_blk.tx()->ready_to_submit()) {

				Cbe::Io_buffer::Index data_index { 0 };
				Cbe::Request request { };
				_cbe_check.has_io_request(request, data_index);

				if (!request.valid()) {
					break;
				}
				if (_blk_req.valid()) {
					break;
				}
				try {
					request.tag = data_index.value;
					Block::Packet_descriptor::Opcode op;
					switch (request.operation) {
					case Cbe::Request::Operation::READ:
						op = Block::Packet_descriptor::READ;
						break;
					case Cbe::Request::Operation::WRITE:
						op = Block::Packet_descriptor::WRITE;
						break;
					default:
						throw Invalid_io_request();
					}
					Block::Packet_descriptor packet {
						_blk.alloc_packet(Cbe::BLOCK_SIZE), op,
						request.block_number, request.count };

					if (request.operation == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation), ": pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
					}
					_blk_req = request;
					_cbe_check.io_request_in_progress(data_index);
					progress = true;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) {
					break;
				}
			}

			while (_blk.tx()->ack_avail()) {

				Block::Packet_descriptor packet =
					_blk.tx()->try_get_acked_packet();

				if (!_blk_req.valid()) {
					break;
				}

				bool const read  =
					packet.operation() == Block::Packet_descriptor::READ;

				bool const write =
					packet.operation() == Block::Packet_descriptor::WRITE;

				bool const op_match =
					(read && _blk_req.read()) ||
					(write && _blk_req.write());

				bool const bn_match =
					packet.block_number() == _blk_req.block_number;

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success =
					packet.succeeded() ? Cbe::Request::Success::TRUE
					                   : Cbe::Request::Success::FALSE;

				Cbe::Io_buffer::Index const data_index { _blk_req.tag };
				bool                  const success    { _blk_req.success == Cbe::Request::Success::TRUE };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_check.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request { };
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_check.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_check.drop_completed_client_request(req);
					_block_session->check_done(
						req.success == Cbe::Request::Success::TRUE ?
							true : false);
					_state = INVALID;
				}
			}
		}

		void _execute_cbe_dump (bool &progress)
		{
			static unsigned x = 0;
			x++;
			_cbe_dump.execute(_blk_buf);
			if (_cbe_dump.execute_progress()) {
				progress = true;
			}

			struct Invalid_io_request : Exception { };

			while (_blk.tx()->ready_to_submit()) {
				Cbe::Io_buffer::Index data_index { 0 };
				Cbe::Request request { };
				_cbe_dump.has_io_request(request, data_index);

				if (!request.valid()) {
					break;
				}
				if (_blk_req.valid()) {
					break;
				}
				try {
					request.tag = data_index.value;
					Block::Packet_descriptor::Opcode op;
					switch (request.operation) {
					case Cbe::Request::Operation::READ:
						op = Block::Packet_descriptor::READ;
						break;
					case Cbe::Request::Operation::WRITE:
						op = Block::Packet_descriptor::WRITE;
						break;
					default:
						throw Invalid_io_request();
					}
					Block::Packet_descriptor packet {
						_blk.alloc_packet(Cbe::BLOCK_SIZE), op,
						request.block_number, request.count };

					if (request.operation == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation), ": pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
					}
					_blk_req = request;
					_cbe_dump.io_request_in_progress(data_index);
					progress = true;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) {
					break;
				}
			}

			while (_blk.tx()->ack_avail()) {

				Block::Packet_descriptor packet =
					_blk.tx()->try_get_acked_packet();

				if (!_blk_req.valid()) {
					break;
				}

				bool const read  =
					packet.operation() == Block::Packet_descriptor::READ;

				bool const write =
					packet.operation() == Block::Packet_descriptor::WRITE;

				bool const op_match =
					(read && _blk_req.read()) ||
					(write && _blk_req.write());

				bool const bn_match =
					packet.block_number() == _blk_req.block_number;

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success =
					packet.succeeded() ? Cbe::Request::Success::TRUE
					                   : Cbe::Request::Success::FALSE;

				Cbe::Io_buffer::Index const data_index { _blk_req.tag };
				bool                  const success    { _blk_req.success == Cbe::Request::Success::TRUE };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_dump.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request { };
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_dump.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_dump.drop_completed_client_request(req);
					_block_session->dump_done(
						req.success == Cbe::Request::Success::TRUE ?
							true : false);
					_state = INVALID;
				}
			}
		}

		void _execute_cbe_init (bool &progress)
		{
			_cbe_init.execute(_blk_buf);
			if (_cbe_init.execute_progress()) {
				progress = true;
			}

			struct Invalid_io_request : Exception { };

			while (_blk.tx()->ready_to_submit()) {

				Cbe::Io_buffer::Index data_index { 0 };
				Cbe::Request request { };
				_cbe_init.has_io_request(request, data_index);

				if (!request.valid()) {
					break;
				}
				if (_blk_req.valid()) {
					break;
				}
				try {
					request.tag = data_index.value;
					Block::Packet_descriptor::Opcode op;
					switch (request.operation) {
					case Cbe::Request::Operation::READ:
						op = Block::Packet_descriptor::READ;
						break;
					case Cbe::Request::Operation::WRITE:
						op = Block::Packet_descriptor::WRITE;
						break;
					default:
						throw Invalid_io_request();
					}
					Block::Packet_descriptor packet {
						_blk.alloc_packet(Cbe::BLOCK_SIZE), op,
						request.block_number, request.count };

					if (request.operation == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation), ": pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
					}
					_blk_req = request;
					_cbe_init.io_request_in_progress(data_index);
					progress = true;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) {
					break;
				}
			}

			while (_blk.tx()->ack_avail()) {

				Block::Packet_descriptor packet =
					_blk.tx()->try_get_acked_packet();

				if (!_blk_req.valid()) {
					break;
				}

				bool const read  =
					packet.operation() == Block::Packet_descriptor::READ;

				bool const write =
					packet.operation() == Block::Packet_descriptor::WRITE;

				bool const op_match =
					(read && _blk_req.read()) ||
					(write && _blk_req.write());

				bool const bn_match =
					packet.block_number() == _blk_req.block_number;

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success =
					packet.succeeded() ? Cbe::Request::Success::TRUE
					                   : Cbe::Request::Success::FALSE;

				Cbe::Io_buffer::Index const data_index { _blk_req.tag };
				bool                  const success    { _blk_req.success == Cbe::Request::Success::TRUE };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_init.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request { };
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_init.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_init.drop_completed_client_request(req);
					_block_session->initialize_done(
						req.success == Cbe::Request::Success::TRUE ?
							true : false);
					_state = INVALID;
				}
			}
		}

		void _execute_cbe (bool &progress)
		{
			if (_state == INVALID || _state == CBE)
			_block_session->with_requests([&] (Block::Request request) {
				using namespace Genode;

				Cbe::Virtual_block_address const vba = request.operation.block_number;

				if (vba > _cbe->max_vba()) {
					warning("reject request with out-of-range virtual block address ", vba);
					return Block_session_component::Response::REJECTED;
				}

				if (!request.operation.valid()) {
					warning("reject invalid request for virtual block address ", vba);
					return Block_session_component::Response::REJECTED;
				}

				if (!_cbe->client_request_acceptable()) {
					return Block_session_component::Response::RETRY;
				}

				Cbe::Request req = convert_to(request);
				_cbe->submit_client_request(req, 0);

				_state = CBE;
				progress |= true;
				return Block_session_component::Response::ACCEPTED;
			});

			/*
			 * Acknowledge finished Block session requests.
			 */

			_block_session->try_acknowledge([&] (Block_session_component::Ack &ack) {

				Cbe::Request const &req = _cbe->peek_completed_client_request();
				if (!req.valid()) { return; }

				_cbe->drop_completed_client_request(req);

				Block::Request request = convert_from(req);

				ack.submit(request);

				if (_block_session->cbe_request_next()) {
					_state = CBE;
				} else {
					_state = INVALID;
				}

				progress |= true;
			});

			if (!_creating_snapshot) {
				_block_session->with_create_snapshot([&] (Create_snapshot const cs) {
					if (_cbe->create_snapshot(cs.token, cs.quarantine)) {
						_creating_snapshot = true;
						// XXX state change?
					} else {
						_block_session->create_snapshot_failed();
					}
					progress |= true;
				});
			}

			if (_creating_snapshot) {
				Cbe::Token       token   { 0 };
				Cbe::Snapshot_ID snap_id { 0, false };

				if (_cbe->snapshot_creation_complete(token, snap_id)) {
					_block_session->create_snapshot_done(token, snap_id);
					_creating_snapshot = false;

					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			if (!_discarding_snapshot_id.valid) {
				_block_session->with_discard_snapshot([&] (Discard_snapshot const ds) {
					_discarding_snapshot_id = { ds.id, true };
					if (_cbe->discard_snapshot(_discarding_snapshot_id)) {
						_block_session->discard_snapshot_done(true);
					} else {
						_block_session->discard_snapshot_done(false);
					}
					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					_discarding_snapshot_id = { 0, false };
					progress |= true;
				});
			}

			_cbe->execute(_blk_buf, _crypto_plain_buf, _crypto_cipher_buf, _timer.curr_time().trunc_to_plain_ms().value);
			progress |= _cbe->execute_progress();

			using Payload = Block_session_component::Payload;

			/*
			 * Poll the CBE for pending data requests. This always happens
			 * whenever we need to read from the Block::Request_stream in case
			 * it is a write requests or write to it when it is read request.
			 */
			_block_session->with_payload([&] (Payload const &payload) {

				/* read */
				{
					Cbe::Request const cbe_request = _cbe->client_data_ready();
					if (!cbe_request.valid()) { return; }

					uint64_t const prim_index = _cbe->client_data_index(cbe_request);
					if (prim_index == ~0ull) {
						Genode::error("prim_index invalid: ", cbe_request);
						return;
					}

					Block::Request request { };
					request.offset = cbe_request.offset + (prim_index * BLOCK_SIZE);
					request.operation.count = 1;

					payload.with_content(request, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);
						Crypto_plain_buffer::Index data_index(~0);
						bool const data_index_valid =
							_cbe->obtain_client_data(cbe_request, data_index);

						if (data_index_valid) {
							progress |= true;
							data = _crypto_plain_buf.item(data_index);
						}
					});
				}
			});

			_block_session->with_payload([&] (Payload const &payload) {
				/* write */
				{
					Cbe::Request const cbe_request = _cbe->client_data_required();
					if (!cbe_request.valid()) { return; }

					uint64_t const prim_index = _cbe->client_data_index(cbe_request);
					if (prim_index == ~0ull) {
						Genode::error("prim_index invalid: ", cbe_request);
						return;
					}

					Block::Request request { };
					request.offset = cbe_request.offset + (prim_index * BLOCK_SIZE);
					request.operation.count = 1;

					payload.with_content(request, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(addr);
						progress |= _cbe->supply_client_data(_timer.curr_time().trunc_to_plain_ms().value, cbe_request, data);
					});
				}
			});

			/*
			 * Backend I/O
			 */

			bool io_progress = false;
			struct Invalid_io_request : Exception { };

			/*
			 * Handle backend I/O requests
			 */
			while (_blk.tx()->ready_to_submit()) {

				Io_buffer::Index data_index { 0 };
				Cbe::Request request = _cbe->has_io_request(data_index);

				if (!request.valid()) {
					break;
				}
				if (_blk_req.valid()) {
					break;
				}
				try {
					request.tag = data_index.value;
					Block::Packet_descriptor::Opcode op;
					switch (request.operation) {
					case Request::Operation::READ:
						op = Block::Packet_descriptor::READ;
						break;
					case Request::Operation::WRITE:
						op = Block::Packet_descriptor::WRITE;
						break;
					case Cbe::Request::Operation::SYNC:
						op = Block::Packet_descriptor::SYNC;
						break;
					default:
						throw Invalid_io_request();
					}
					Block::Packet_descriptor packet {
						_blk.alloc_packet(Cbe::BLOCK_SIZE), op,
						request.block_number, request.count };

					if (request.operation == Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation), ": pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
					}
					_blk_req = request;

					_cbe->io_request_in_progress(data_index);

					progress |= true;
					io_progress |= true;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) { break; }
			}

			while (_blk.tx()->ack_avail()) {
				Block::Packet_descriptor packet = _blk.tx()->try_get_acked_packet();

				if (!_blk_req.valid()) { break; }

				bool const read  = packet.operation() == Block::Packet_descriptor::READ;
				bool const write = packet.operation() == Block::Packet_descriptor::WRITE;
				bool const sync  = packet.operation() == Block::Packet_descriptor::SYNC;

				bool const op_match =
					(read && _blk_req.read()) ||
					(write && _blk_req.write()) ||
					(sync && _blk_req.sync());

				bool const bn_match = packet.block_number() == _blk_req.block_number;
				// assert packet descriptor belongs to stored backend request
				if (!bn_match || !op_match) { break; }

				_blk_req.success =
					packet.succeeded() ? Cbe::Request::Success::TRUE
									   : Cbe::Request::Success::FALSE;

				Io_buffer::Index const data_index { _blk_req.tag };
				bool             const success    { _blk_req.success == Request::Success::TRUE };
				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe->io_request_completed(data_index, success);
				progress |= true;

				_blk.tx()->release_packet(packet);

				_blk_req = Cbe::Request { };
				io_progress |= true;
			}

			progress |= io_progress;

			/*********************
			 ** Crypto handling **
			 *********************/

			progress |= _crypto.execute();

			/* encrypt */
			while (true) {
				Crypto_plain_buffer::Index data_index(0);
				Cbe::Request request = _cbe->crypto_cipher_data_required(data_index);
				if (!request.valid()) {
					break;
				}
				if (!_crypto.encryption_request_acceptable()) {
					break;
				}
				request.tag = data_index.value;
				_crypto.submit_encryption_request(request, _crypto_plain_buf.item(data_index), 0);
				_cbe->crypto_cipher_data_requested(data_index);
				if (_verbose_back_end_crypto) {
					log ("   encrypt: pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
				}
				progress |= true;
			}
			while (true) {
				Cbe::Request const request = _crypto.peek_completed_encryption_request();
				if (!request.valid()) {
					break;
				}
				Crypto_cipher_buffer::Index const data_index(request.tag);
				if (!_crypto.supply_cipher_data(request, _crypto_cipher_buf.item(data_index))) {
					break;
				}
				_cbe->supply_crypto_cipher_data(data_index, request.success == Request::Success::TRUE);
				progress |= true;
			}

			/* decrypt */
			while (true) {
				Crypto_cipher_buffer::Index data_index(0);
				Cbe::Request request = _cbe->crypto_plain_data_required(data_index);
				if (!request.valid()) {
					break;
				}
				if (!_crypto.decryption_request_acceptable()) {
					break;
				}
				request.tag = data_index.value;
				_crypto.submit_decryption_request(request, _crypto_cipher_buf.item(data_index), 0);
				_cbe->crypto_plain_data_requested(data_index);
				if (_verbose_back_end_crypto) {
					log ("   decrypt: pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
				}
				progress |= true;
			}
			while (true) {
				Cbe::Request const request = _crypto.peek_completed_decryption_request();
				if (!request.valid()) {
					break;
				}
				Crypto_plain_buffer::Index const data_index(request.tag);
				if (!_crypto.supply_plain_data(request, _crypto_plain_buf.item(data_index))) {
					break;
				}
				_cbe->supply_crypto_plain_data(data_index, request.success == Request::Success::TRUE);
				progress |= true;
			}

			if (!_blk_req.valid()) {
				if (_block_session->cbe_request_next()) {
					_state = CBE;
				} else {
					_state = INVALID;
				}
			}
		}

		void _execute_cbe_construction(bool &progress)
		{
			while (_nr_of_sbs_requested < NUM_SUPER_BLOCKS &&
			       _blk.tx()->ready_to_submit())
			{
				Cbe::Request request {
					Cbe::Request::Operation::READ,
					Cbe::Request::Success::FALSE,
					_nr_of_sbs_requested,
					0,
					1,
					0
				};
				try {
					Block::Packet_descriptor packet {
						_blk.alloc_packet(Cbe::BLOCK_SIZE),
						Block::Packet_descriptor::READ,
						request.block_number, request.count };

					_blk.tx()->try_submit_packet(packet);
					if (_nr_of_sbs_requested == 0) {
						log("cbe construction started");
					}
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation), ": pba ", (unsigned long)request.block_number, ", cnt ", (unsigned long)request.count);
					}
					_nr_of_sbs_requested++;
					progress = true;
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) {
					break;
				}
			}

			while (_nr_of_sbs_available < NUM_SUPER_BLOCKS &&
			       _blk.tx()->ack_avail())
			{
				Block::Packet_descriptor packet =
					_blk.tx()->try_get_acked_packet();

				if (packet.block_number() >= NUM_SUPER_BLOCKS ||
				    packet.operation() != Block::Packet_descriptor::READ)
				{
					_blk.tx()->release_packet(packet);
					continue;
				}

				if (!packet.succeeded()) {
					error("failed to read superblock");
					_env.parent().exit(-1);
				}

				_super_blocks.block[_nr_of_sbs_available] =
					*reinterpret_cast<Cbe::Superblock*>(
						_blk.tx()->packet_content(packet));

				_blk.tx()->release_packet(packet);
				_nr_of_sbs_available++;
				progress = true;
			}

			if (_nr_of_sbs_available == NUM_SUPER_BLOCKS) {

				Cbe::Generation        last_gen = 0;
				Cbe::Superblocks_index most_recent_sb { 0 };
				bool                   most_recent_sb_valid { false };
				for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {

					Cbe::Superblock &dst = _super_blocks.block[i];
					if (dst.valid() && dst.last_secured_generation >= last_gen) {
						most_recent_sb.value = i;
						most_recent_sb_valid = true;
						last_gen = dst.last_secured_generation;
					}
				}
				struct Failed : Exception { };
				if (!most_recent_sb_valid) {
					error("failed to find current superblock");
					_env.parent().exit(-1);
				}
				_cbe.construct(_super_blocks, most_recent_sb);
				log("cbe construction succeeded: vba range is 0 .. ", _cbe->max_vba());
				progress = true;
			}
		}

		void _execute_cbe_destruction(bool &progress)
		{
			_nr_of_sbs_available = 0;
			_nr_of_sbs_requested = 0;
			_cbe.destruct();
			progress = true;
		}

		void _execute()
		{
			if (!_block_session.constructed()) { return; }

			for (bool progress = true; progress; ) {

				progress = false;

				if (_state == INVALID)
				_block_session->with_initialize([&] (Cbe_init::Configuration const &cfg) {

					if (!_cbe_init.client_request_acceptable()) {
						error("failed to submit request");
						_env.parent().exit(-1);
					}
					_cbe_init.submit_client_request(
						Cbe::Request {
							Cbe::Request::Operation::READ,
							Cbe::Request::Success::FALSE, 0, 0, 0, 0 },
						cfg.vbd_nr_of_lvls() - 1,
						cfg.vbd_nr_of_children(),
						cfg.vbd_nr_of_leafs(),
						cfg.ft_nr_of_lvls() - 1,
						cfg.ft_nr_of_children(),
						cfg.ft_nr_of_leafs());

					_state = CBE_INIT;
					progress = true;
				});

				if (_state == INVALID)
				_block_session->with_check([&] () {

					if (!_cbe_check.client_request_acceptable()) {
						error("failed to submit request");
						_env.parent().exit(-1);
					}
					_cbe_check.submit_client_request(
						Cbe::Request {
							Cbe::Request::Operation::READ,
							Cbe::Request::Success::FALSE, 0, 0, 0, 0 });

					_state = CBE_CHECK;
					progress = true;
				});

				if (_state == INVALID)
				_block_session->with_dump([&] (Cbe_dump::Configuration const &cfg) {

					if (!_cbe_dump.client_request_acceptable()) {
						error("failed to submit request");
						_env.parent().exit(-1);
					}
					_cbe_dump.submit_client_request(
						Cbe::Request {
							Cbe::Request::Operation::READ,
							Cbe::Request::Success::FALSE, 0, 0, 0, 0 },
						cfg);

					_state = CBE_DUMP;
					progress = true;
				});

				if (_state == CBE_INIT) {
					if (_cbe.constructed()) {
						_execute_cbe_destruction(progress);
					} else {
						_execute_cbe_init(progress);
					}
				} else if (_state == CBE_CHECK) {
					if (_cbe.constructed()) {
						_execute_cbe_destruction(progress);
					} else {
						_execute_cbe_check(progress);
					}
				} else if (_state == CBE_DUMP) {
					if (_cbe.constructed()) {
						_execute_cbe_destruction(progress);
					} else {
						_execute_cbe_dump(progress);
					}
				} else if (_state == CBE) {
					if (!_cbe.constructed()) {
						_execute_cbe_construction(progress);
					} else {
						_execute_cbe(progress);
					}
				} else if (_state == INVALID) {
					// XXX
					if (!_cbe.constructed()) {
						_execute_cbe_construction(progress);
					} else {
						_execute_cbe(progress);
					}
				}
			}
			/* notify I/O backend */
			_blk.tx()->wakeup();

			/* notify client */
			_block_session->wakeup_client_if_needed();
		}

		/**
		 *  Read super-blocks
		 *
		 *  The super-blocks are read one by one using blocking I/O and are
		 *  stored in the super-block array. While reading all blocks, the
		 *  most recent one is determined
		 *
		 *  \param  sb  array where the super-blocks are stored
		 *
		 *  \return  index of the most recent super-block or an INVALID
		 *           index in case the super-block could not be found
		 */
		Cbe::Superblocks_index _read_superblocks(Cbe::Superblocks &sbs)
		{
			Cbe::Generation        last_gen = 0;
			Cbe::Superblocks_index most_recent_sb { 0 };
			bool                   most_recent_sb_valid { false };

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Util::Block_io io(_blk, sizeof (Cbe::Superblock), i, 1);
				void const       *src = io.addr<void*>();
				Cbe::Superblock &dst = sbs.block[i];
				Genode::memcpy(&dst, src, sizeof (Cbe::Superblock));

				/*
				 * For now this always selects the last SB if the generation
				 * is the same and is mostly used for finding the initial SB
				 * with generation == 0.
				 */
				if (dst.valid() && dst.last_secured_generation >= last_gen) {
					most_recent_sb.value = i;
					most_recent_sb_valid = true;
					last_gen = dst.last_secured_generation;
				}

				Sha256_4k::Hash hash { };
				Sha256_4k::Data const &data = *reinterpret_cast<Sha256_4k::Data const*>(&dst);
				Sha256_4k::hash(data, hash);
			}
			struct Failed : Exception { };
			if (!most_recent_sb_valid) {
				throw Failed();
			}
			return most_recent_sb;
		}

	public:

		/*
		 * Constructor
		 *
		 * \param env   reference to Genode environment
		 */
		Main(Env &env)
		:
			_env { env }
		{
			External::Crypto::Key_data key { };
			Genode::memcpy(key.value, "All your base are belong to us  ", 32);
			_crypto.set_key(0, 0, key);

			/*
			 * Install signal handler for the backend Block connection.
			 *
			 * (Hopefully outstanding Block requests will not create problems when
			 *  the frontend session is already gone.)
			 */
			_blk.tx_channel()->sigh_ack_avail(_request_handler);
			_blk.tx_channel()->sigh_ready_to_submit(_request_handler);

			_block_session.construct(_env, _config_rom, _heap);
			_execute();
		}

		~Main()
		{
			_blk.tx_channel()->sigh_ack_avail(Signal_context_capability());
			_blk.tx_channel()->sigh_ready_to_submit(Signal_context_capability());
		}
};


extern "C" void print_size(Genode::size_t sz) {
	Genode::log(sz);
}


extern "C" void print_u64(unsigned long long const u) { Genode::log(u); }
extern "C" void print_u32(unsigned int const u) { Genode::log(u); }
extern "C" void print_u16(unsigned short const u) { Genode::log(u); }
extern "C" void print_u8(unsigned char const u) { Genode::log(u); }


extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	Genode::log(Genode::Cstring(s, len));
}

extern "C" void print_cstring_buffered(char const     *input,
                                       Genode::size_t  input_len)
{
	enum { BUF_SIZE = 256 };

	static char   buf[BUF_SIZE];
	static size_t buf_left { BUF_SIZE };

	char const *input_curr = input;
	for (size_t input_left = input_len; input_left; input_left--) {
		if (*input_curr == '\n' || buf_left == 0) {
			Genode::log(Genode::Cstring(buf, BUF_SIZE - buf_left));
			buf_left = BUF_SIZE;
		} else {
			buf[BUF_SIZE - buf_left] = *input_curr;
			buf_left--;
		}
		input_curr++;
	}

}

extern "C" void adainit();

void Component::construct(Genode::Env &env)
{
	env.exec_static_constructors();

	/**
	 * We have to call adainit, so, the secondary stack of SPARK
	 * for, e.g., variable-sized return values gets initialized.
	 */
	adainit();

	Cbe::assert_valid_object_size<Cbe::Library>();
	Cbe::assert_valid_object_size<Cbe_init::Library>();
	Cbe::assert_valid_object_size<Cbe_check::Library>();
	Cbe::assert_valid_object_size<Cbe_dump::Library>();
	Cbe::assert_valid_object_size<External::Crypto>();

	static Cbe::Main inst(env);
}

extern "C" int memcmp(const void *p0, const void *p1, Genode::size_t size)
{
	return Genode::memcmp(p0, p1, size);
}
