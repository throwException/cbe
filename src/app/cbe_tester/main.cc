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
#include <util/bit_allocator.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/external_crypto.h>

/* cbe-check includes */
#include <cbe_check/library.h>

/* cbe-dump includes */
#include <cbe_dump/library.h>
#include <cbe_dump/configuration.h>

/* cbe-init includes */
#include <cbe_init/library.h>
#include <cbe_init/configuration.h>

/* local includes */
#include <util.h>

/* CBE external trust anchor */
#include <cbe/external_ta.h>

using namespace Genode;

namespace Cbe {

	struct Block_session_component;
	struct Main;

	static inline Block::Request convert_from(Cbe::Request const &r)
	{
		struct Operation_type_not_convertable : Genode::Exception { };
		auto convert_op = [&] (Cbe::Request::Operation o) {
			switch (o) {
			case Cbe::Request::Operation::INVALID:          return Block::Operation::Type::INVALID;
			case Cbe::Request::Operation::READ:             return Block::Operation::Type::READ;
			case Cbe::Request::Operation::WRITE:            return Block::Operation::Type::WRITE;
			case Cbe::Request::Operation::SYNC:             return Block::Operation::Type::SYNC;
			case Cbe::Request::Operation::CREATE_SNAPSHOT:  throw Operation_type_not_convertable();
			case Cbe::Request::Operation::DISCARD_SNAPSHOT: throw Operation_type_not_convertable();
			case Cbe::Request::Operation::REKEY:            throw Operation_type_not_convertable();
			case Cbe::Request::Operation::EXTEND_VBD:       throw Operation_type_not_convertable();
			case Cbe::Request::Operation::EXTEND_FT:        throw Operation_type_not_convertable();
			case Cbe::Request::Operation::RESUME_REKEYING:  throw Operation_type_not_convertable();
			case Cbe::Request::Operation::DEINITIALIZE:     throw Operation_type_not_convertable();
			case Cbe::Request::Operation::INITIALIZE:       throw Operation_type_not_convertable();
			}
			return Block::Operation::Type::INVALID;
		};
		return Block::Request {
			.operation = {
				.type         = convert_op(r.operation()),
				.block_number = r.block_number(),
				.count        = r.count(),
			},
			.success   = r.success(),
			.offset    = (Block::off_t)r.offset(),
			.tag       = { .value = r.tag() },
		};
	}

	static inline Cbe::Request convert_to(Block::Request const &r)
	{
		auto convert_op = [&] (Block::Operation::Type t) {
			switch (t) {
			case Block::Operation::Type::INVALID: return Cbe::Request::Operation::INVALID;
			case Block::Operation::Type::READ:    return Cbe::Request::Operation::READ;
			case Block::Operation::Type::WRITE:   return Cbe::Request::Operation::WRITE;
			case Block::Operation::Type::SYNC:    return Cbe::Request::Operation::SYNC;
			case Block::Operation::Type::TRIM:    return Cbe::Request::Operation::INVALID;
			}
			return Cbe::Request::Operation::INVALID;
		};
		return Cbe::Request(
			convert_op(r.operation.type),
			r.success,
			r.operation.block_number,
			(Genode::uint64_t)r.offset,
			(Number_of_blocks)r.operation.count,
			0,
			(Genode::uint32_t)r.tag.value);
	}
} /* namespace Cbe */


char const *to_string(Block::Operation::Type type)
{
	struct Unknown_operation_type : Genode::Exception { };
	switch (type) {
	case Block::Operation::Type::INVALID: return "invalid";
	case Block::Operation::Type::READ: return "read";
	case Block::Operation::Type::WRITE: return "write";
	case Block::Operation::Type::SYNC: return "sync";
	case Block::Operation::Type::TRIM: return "trim";
	}
	throw Unknown_operation_type();
}


struct Create_snapshot_id
{
	uint64_t value;

	Create_snapshot_id (uint64_t value) : value { value } { }
};


struct Create_snapshot
{
	Create_snapshot_id id;

	Create_snapshot(Create_snapshot_id id) : id { id } { }
};


struct Failed_to_find_created_snapshot : Genode::Exception { };


class Created_snapshot : public Avl_node<Created_snapshot>
{
	private:

		Create_snapshot_id _id;
		Cbe::Generation    _generation;

	public:

		Created_snapshot(Create_snapshot_id id,
		                 Cbe::Generation    generation)
		:
			_id         { id },
			_generation { generation }
		{ }

		Created_snapshot &find_by_id(Create_snapshot_id const &id)
		{
			if (id.value == _id.value) {
				return *this;
			}
			bool const side =  _id.value > id.value;
			Created_snapshot *const snap = child(side);
			if (!snap) {
				throw Failed_to_find_created_snapshot();
			}
			return snap->find_by_id(id);
		}

		Cbe::Generation generation() const { return _generation; }


		/**************
		 ** Avl_node **
		 **************/

		bool higher(Created_snapshot *other)
		{
			return _id.value > other->_id.value;
		}
};


class Created_snapshots_tree : public Avl_tree<Created_snapshot>
{
	public:

		Created_snapshot &find_by_id(Create_snapshot_id const &id)
		{
			if (!first()) {
				throw Failed_to_find_created_snapshot();
			}
			return first()->find_by_id(id);
		}
};


struct Discard_snapshot
{
	Create_snapshot_id id;

	Discard_snapshot(Create_snapshot_id id) : id { id } { }
};


struct Extend_vbd
{
	uint32_t nr_of_phys_blocks;

	Extend_vbd(uint32_t nr_of_phys_blocks)
	:
		nr_of_phys_blocks { nr_of_phys_blocks }
	{ }
};


struct Extend_ft
{
	uint32_t nr_of_phys_blocks;

	Extend_ft(uint32_t nr_of_phys_blocks)
	:
		nr_of_phys_blocks { nr_of_phys_blocks }
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
			REKEY,
			DEINITIALIZE,
			EXTEND_VBD,
			EXTEND_FT,
			INITIALIZE,
			CHECK,
			DUMP,
			LIST_SNAPSHOTS
		};

		Type                                   type             { INVALID };
		Constructible<Block::Request>          request          { };
		Constructible<Create_snapshot>         create_snapshot  { };
		Constructible<Discard_snapshot>        discard_snapshot { };
		Constructible<Extend_vbd>              extend_vbd       { };
		Constructible<Extend_ft>               extend_ft        { };
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
			if (other.extend_vbd.constructed()) {
				extend_vbd.construct(*other.extend_vbd);
			}
			if (other.extend_ft.constructed()) {
				extend_ft.construct(*other.extend_ft);
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
	unsigned long           _with_payload_cnt { 0 };
	Created_snapshots_tree  _created_snapshots { };

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
			Create_snapshot_id id { 0 };
			if (!node.attribute("id").value(id.value)) {
				error("create-snapshot node has bad id attribute");
				throw Bad_create_snapshot_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::CREATE_SNAPSHOT;
			test.create_snapshot.construct(id);
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
			Create_snapshot_id id { 0 };
			if (!node.attribute("id").value(id.value)) {
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

	void _read_rekey_node()
	{
		Test &test = *new (_alloc) Test;
		test.type = Test::REKEY;
		_test_queue.enqueue(test);
	}

	void _read_deinitialize_node()
	{
		Test &test = *new (_alloc) Test;
		test.type = Test::DEINITIALIZE;
		_test_queue.enqueue(test);
	}

	void _read_list_snapshots_node()
	{
		Test &test = *new (_alloc) Test;
		test.type = Test::LIST_SNAPSHOTS;
		_test_queue.enqueue(test);
	}

	void _read_extend_vbd_node(Xml_node const &node)
	{
		struct Bad_extend_vbd_node : Exception { };
		try {
			uint32_t nr_of_phys_blocks { 0 };
			if (!node.attribute("nr_of_phys_blocks").value(nr_of_phys_blocks)) {
				error("extend-vbd node has bad nr_of_phys_blocks attribute");
				throw Bad_extend_vbd_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::EXTEND_VBD;
			test.extend_vbd.construct(nr_of_phys_blocks);
			_test_queue.enqueue(test);
		} catch (Xml_node::Nonexistent_attribute) {
			error("extend-vbd node misses attribute");
			throw Bad_extend_vbd_node();
		}
	}

	void _read_extend_ft_node(Xml_node const &node)
	{
		struct Bad_extend_ft_node : Exception { };
		try {
			uint32_t nr_of_phys_blocks { 0 };
			if (!node.attribute("nr_of_phys_blocks").value(nr_of_phys_blocks)) {
				error("extend-ft node has bad nr_of_phys_blocks attribute");
				throw Bad_extend_ft_node();
			}
			Test &test = *new (_alloc) Test;
			test.type = Test::EXTEND_FT;
			test.extend_ft.construct(nr_of_phys_blocks);
			_test_queue.enqueue(test);
		} catch (Xml_node::Nonexistent_attribute) {
			error("extend-ft node misses attribute");
			throw Bad_extend_ft_node();
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
				} else if (sub_node.has_type("rekey")) {
					_read_rekey_node();
				} else if (sub_node.has_type("deinitialize")) {
					_read_deinitialize_node();
				} else if (sub_node.has_type("list-snapshots")) {
					_read_list_snapshots_node();
				} else if (sub_node.has_type("extend-vbd")) {
					_read_extend_vbd_node(sub_node);
				} else if (sub_node.has_type("extend-ft")) {
					_read_extend_ft_node(sub_node);
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
		for (unsigned idx = 0;
		     idx < sizeof(_blk_data.values)/sizeof(_blk_data.values[0]);
		     idx++)
		{
			_blk_data.values[idx] = _with_payload_cnt + idx + 1;
		}
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
	void with_payload(FN const &fn)
	{
		Payload payload { (addr_t)&_blk_data };
		if  (fn(payload)) {

			_with_payload_cnt++;
			for (unsigned idx = 0;
			     idx < sizeof(_blk_data.values)/sizeof(_blk_data.values[0]);
			     idx++)
			{
				_blk_data.values[idx] = _with_payload_cnt + idx + 1;
			}
		}
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
		log("create snapshot started: id ",
		    _test_in_progress->create_snapshot->id.value);

		fn(*_test_in_progress->create_snapshot);
	}

	void create_snapshot_done(Cbe::Request const &req,
	                          Genode::Allocator  &alloc)
	{
		if (req.success()) {

			log("create snapshot succeeded:",
			    " id ", _test_in_progress->create_snapshot->id.value,
			    " generation ", (Generation)req.offset());

			_created_snapshots.insert(
				new (alloc)
					Created_snapshot(
						_test_in_progress->create_snapshot->id,
						(Generation)req.offset()));

		} else {
			_nr_of_failed_tests++;
			log("create snapshot failed:"
			    " id ",
			    _test_in_progress->create_snapshot->id.value);
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

		try {
			Created_snapshot const &snap {
				_created_snapshots.find_by_id(
					_test_in_progress->discard_snapshot->id) };

			log("discard snapshot started: id ",
				_test_in_progress->discard_snapshot->id.value,
				" generation ",
				snap.generation());

			fn(snap.generation());
		}
		catch (Failed_to_find_created_snapshot) {
			_nr_of_failed_tests++;
			log("discard snapshot failed (unknown snapshot): id ",
			    _test_in_progress->discard_snapshot->id.value);
			_test_in_progress.destruct();
		}
	}

	void discard_snapshot_done(Cbe::Request const &req,
	                           Genode::Allocator &alloc)
	{
		if (req.success()) {

			Created_snapshot &snap {
				_created_snapshots.find_by_id(
					_test_in_progress->discard_snapshot->id) };

			_created_snapshots.remove(&snap);
			destroy(alloc, &snap);

			log("discard snapshot succeeded: id ",
			    _test_in_progress->discard_snapshot->id.value);

		} else {
			_nr_of_failed_tests++;
			log("discard snapshot failed: id ",
			    _test_in_progress->discard_snapshot->id.value);
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_deinitialize(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_has_correct_type { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::DEINITIALIZE) {
				head_has_correct_type = true;
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
		if (!head_has_correct_type) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("deinitialize started");

		fn();
	}

	template <typename FN>
	void with_list_snapshots(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_has_correct_type { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::LIST_SNAPSHOTS) {
				head_has_correct_type = true;
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
		if (!head_has_correct_type) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("list snapshots:");

		fn();
		_test_in_progress.destruct();
	}

	void deinitialize_done(bool success)
	{
		if (success) {
			log("deinitialize succeeded");
		} else {
			_nr_of_failed_tests++;
			log("deinitialize failed");
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_rekey(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_has_correct_type { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::REKEY) {
				head_has_correct_type = true;
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
		if (!head_has_correct_type) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("rekey started");

		fn();
	}

	void rekey_done(bool success)
	{
		if (success) {
			log("rekey succeeded");
		} else {
			_nr_of_failed_tests++;
			log("rekey failed");
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_extend_vbd(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_has_correct_type { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::EXTEND_VBD) {
				head_has_correct_type = true;
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
		if (!head_has_correct_type) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("extend-vbd started: nr_of_phys_blocks ",
		    _test_in_progress->extend_vbd->nr_of_phys_blocks);

		fn(*_test_in_progress->extend_vbd);
	}

	void extend_vbd_done(bool success)
	{
		if (success) {
			log("extend_vbd succeeded: nr_of_phys_blocks ",
			    _test_in_progress->extend_vbd->nr_of_phys_blocks);
		} else {
			_nr_of_failed_tests++;
			log("extend_vbd failed: nr_of_phys_blocks ",
			    _test_in_progress->extend_vbd->nr_of_phys_blocks);
		}
		_test_in_progress.destruct();
	}

	template <typename FN>
	void with_extend_ft(FN const &fn)
	{
		if (_test_in_progress.constructed()) {
			return;
		}
		bool head_available { false };
		bool head_has_correct_type { false };
		_test_queue.head([&] (Test &test) {
			head_available = true;
			if (test.type == Test::EXTEND_FT) {
				head_has_correct_type = true;
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
		if (!head_has_correct_type) {
			return;
		}
		_test_queue.dequeue([&] (Test &test) {
			_test_in_progress.construct(test);
			destroy(_alloc, &test);
		});
		log("extend-ft started: nr_of_phys_blocks ",
		    _test_in_progress->extend_ft->nr_of_phys_blocks);

		fn(*_test_in_progress->extend_ft);
	}

	void extend_ft_done(bool success)
	{
		if (success) {
			log("extend_ft succeeded: nr_of_phys_blocks ",
			    _test_in_progress->extend_ft->nr_of_phys_blocks);
		} else {
			_nr_of_failed_tests++;
			log("extend_ft failed: nr_of_phys_blocks ",
			    _test_in_progress->extend_ft->nr_of_phys_blocks);
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
			result |= test.type == Test::REKEY;
			result |= test.type == Test::DEINITIALIZE;
			result |= test.type == Test::EXTEND_VBD;
			result |= test.type == Test::EXTEND_FT;
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
		    ", lvls ", _test_in_progress->initialize->vbd_nr_of_lvls(),
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


class Cbe::Main
{
	private:

		enum State { INVALID, CBE, CBE_INIT, CBE_CHECK, CBE_DUMP };

		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };

		Env                                    &_env;
		Attached_rom_dataspace                  _config_rom              { _env, "config" };
		bool                                    _verbose_back_end_io     { false };
		bool                                    _verbose_back_end_crypto { false };
		Constructible<Block_session_component>  _block_session           { };
		Heap                                    _heap                    { _env.ram(), _env.rm() };
		Allocator_avl                           _blk_alloc               { &_heap };
		Block::Connection<>                     _blk                     { _env, &_blk_alloc, TX_BUF_SIZE };
		Constructible<Cbe::Library>             _cbe                     { };
		Cbe_check::Library                      _cbe_check               { };
		Cbe_dump::Library                       _cbe_dump                { };
		Cbe_init::Library                       _cbe_init                { };
		External::Trust_anchor                  _trust_anchor            { };
		Cbe::Hash                               _last_sb_hash            { };
		Cbe::Request                            _blk_req                 { };
		Io_buffer                               _blk_buf                 { };
		Crypto_plain_buffer                     _crypto_plain_buf        { };
		Crypto_cipher_buffer                    _crypto_cipher_buf       { };
		External::Crypto                        _crypto                  { };
		State                                   _state                   { INVALID };
		Signal_handler<Main>                    _request_handler         { _env.ep(), *this, &Main::_execute };
		bool                                    _creating_snapshot       { false };
		bool                                    _discard_snapshot        { false };
		bool                                    _rekey                   { false };
		bool                                    _deinitialize            { false };
		bool                                    _extend_vbd              { false };
		Extend_vbd                              _extend_vbd_obj          { 0 };
		bool                                    _extend_ft               { false };
		Extend_ft                               _extend_ft_obj           { 0 };

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
					request.tag(data_index.value);
					Block::Packet_descriptor::Opcode op;
					switch (request.operation()) {
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
						request.block_number(), request.count() };

					if (request.operation() == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation()), ": pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
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
					packet.block_number() == _blk_req.block_number();

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success(packet.succeeded());

				Cbe::Io_buffer::Index const data_index { _blk_req.tag() };
				bool                  const success    { _blk_req.success() };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_check.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request();
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_check.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_check.drop_completed_client_request(req);
					_block_session->check_done(req.success());
					_state = INVALID;
				}
			}
		}

		void _execute_cbe_dump (bool &progress)
		{
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
					request.tag(data_index.value);
					Block::Packet_descriptor::Opcode op;
					switch (request.operation()) {
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
						request.block_number(), request.count() };

					if (request.operation() == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation()), ": pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
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
					packet.block_number() == _blk_req.block_number();

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success(packet.succeeded());

				Cbe::Io_buffer::Index const data_index { _blk_req.tag() };
				bool                  const success    { _blk_req.success() };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_dump.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request();
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_dump.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_dump.drop_completed_client_request(req);
					_block_session->dump_done(req.success());
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
					request.tag(data_index.value);
					Block::Packet_descriptor::Opcode op;
					switch (request.operation()) {
					case Cbe::Request::Operation::READ:
						op = Block::Packet_descriptor::READ;
						break;
					case Cbe::Request::Operation::WRITE:
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
						request.block_number(), request.count() };

					if (request.operation() == Cbe::Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation()), ": pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
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

				bool const sync =
					packet.operation() == Block::Packet_descriptor::SYNC;

				bool const op_match =
					(read && _blk_req.read()) ||
					(sync && _blk_req.sync()) ||
					(write && _blk_req.write());

				bool const bn_match =
					packet.block_number() == _blk_req.block_number();

				if (!bn_match || !op_match) {
					break;
				}

				_blk_req.success(packet.succeeded());

				Cbe::Io_buffer::Index const data_index { _blk_req.tag() };
				bool                  const success    { _blk_req.success() };

				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe_init.io_request_completed(data_index, success);
				_blk.tx()->release_packet(packet);
				_blk_req = Cbe::Request();
				progress = true;
			}

			if (!_blk_req.valid()) {
				Cbe::Request const req {
					_cbe_init.peek_completed_client_request() };

				if (req.valid()) {
					_cbe_init.drop_completed_client_request(req);
					_block_session->initialize_done(req.success());
					_state = INVALID;
				}
			}

			/* handle requests to the trust anchor */
			{
				progress |= _trust_anchor.execute();

				using Op = Cbe::Trust_anchor_request::Operation;

				while (true) {

					Cbe::Trust_anchor_request const request =
						_cbe_init.peek_generated_ta_request();

					if (!request.valid()) { break; }
					if (!_trust_anchor.request_acceptable()) { break; }

					switch (request.operation()) {
					case Op::CREATE_KEY:
						_trust_anchor.submit_create_key_request(request);
						break;
					case Op::SECURE_SUPERBLOCK:
					{
						_last_sb_hash = _cbe_init.peek_generated_ta_sb_hash(request);
						_trust_anchor.submit_secure_superblock_request(request, _last_sb_hash);
						break;
					}
					case Op::ENCRYPT_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_cbe_init.peek_generated_ta_key_value_plaintext(request);

						_trust_anchor.submit_encrypt_key_request(request, pk);
						break;
					}
					case Op::DECRYPT_KEY:
					{
						Cbe::Key_ciphertext_value const ck =
							_cbe_init.peek_generated_ta_key_value_ciphertext(request);

						_trust_anchor.submit_decrypt_key_request(request, ck);
						break;
					}
					case Op::LAST_SB_HASH:

						struct Cbe_init_requested_ta_last_sb_hash : Exception { };
						throw Cbe_init_requested_ta_last_sb_hash();

					case Op::INVALID:
						/* never reached */
						break;
					}
					_cbe_init.drop_generated_ta_request(request);
					progress |= true;
				}

				while (true) {

					Cbe::Trust_anchor_request const request =
						_trust_anchor.peek_completed_request();

					if (!request.valid()) { break; }

					switch (request.operation()) {
					case Op::CREATE_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_trust_anchor.peek_completed_key_value_plaintext(request);

						_cbe_init.mark_generated_ta_create_key_request_complete(request, pk);
						break;
					}
					case Op::SECURE_SUPERBLOCK:
					{
						_cbe_init.mark_generated_ta_secure_sb_request_complete(request);
						break;
					}
					case Op::ENCRYPT_KEY:
					{
						Cbe::Key_ciphertext_value const ck =
							_trust_anchor.peek_completed_key_value_ciphertext(request);

						_cbe_init.mark_generated_ta_encrypt_key_request_complete(request, ck);
						break;
					}
					case Op::DECRYPT_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_trust_anchor.peek_completed_key_value_plaintext(request);

						_cbe_init.mark_generated_ta_decrypt_key_request_complete(request, pk);
						break;
					}
					case Op::LAST_SB_HASH:

						struct Ta_completed_last_sb_hash_request_for_cbe_init : Exception { };
						throw Ta_completed_last_sb_hash_request_for_cbe_init();

					case Op::INVALID:
						/* never reached */
						break;
					}
					_trust_anchor.drop_completed_request(request);
					progress |= true;
				}
			}
		}

		void _execute_cbe (bool &progress)
		{
			/* handle requests to the trust anchor */
			{
				progress |= _trust_anchor.execute();

				using Op = Cbe::Trust_anchor_request::Operation;

				while (true) {

					Cbe::Trust_anchor_request const request =
						_cbe->peek_generated_ta_request();

					if (!request.valid()) { break; }
					if (!_trust_anchor.request_acceptable()) { break; }

					switch (request.operation()) {
					case Op::CREATE_KEY:
						_trust_anchor.submit_create_key_request(request);
						_cbe->drop_generated_ta_request(request);
						break;
					case Op::SECURE_SUPERBLOCK:
					{
						_last_sb_hash = _cbe->peek_generated_ta_sb_hash(request);
						_trust_anchor.submit_secure_superblock_request(request, _last_sb_hash);
						_cbe->drop_generated_ta_request(request);
						break;
					}
					case Op::ENCRYPT_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_cbe->peek_generated_ta_key_value_plaintext(request);

						_trust_anchor.submit_encrypt_key_request(request, pk);
						_cbe->drop_generated_ta_request(request);
						break;
					}
					case Op::DECRYPT_KEY:
					{
						Cbe::Key_ciphertext_value const ck =
							_cbe->peek_generated_ta_key_value_ciphertext(request);

						_trust_anchor.submit_decrypt_key_request(request, ck);
						_cbe->drop_generated_ta_request(request);
						break;
					}
					case Op::LAST_SB_HASH:

						_cbe->drop_generated_ta_request(request);
						_cbe->mark_generated_ta_last_sb_hash_request_complete(request, _last_sb_hash);
						break;

					case Op::INVALID:
						break;
					}
					progress |= true;
				}

				while (true) {

					Cbe::Trust_anchor_request const request =
						_trust_anchor.peek_completed_request();

					if (!request.valid()) { break; }

					switch (request.operation()) {
					case Op::CREATE_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_trust_anchor.peek_completed_key_value_plaintext(request);

						_cbe->mark_generated_ta_create_key_request_complete(request, pk);
						break;
					}
					case Op::SECURE_SUPERBLOCK:
					{
						_cbe->mark_generated_ta_secure_sb_request_complete(request);
						break;
					}
					case Op::ENCRYPT_KEY:
					{
						Cbe::Key_ciphertext_value const ck =
							_trust_anchor.peek_completed_key_value_ciphertext(request);

						_cbe->mark_generated_ta_encrypt_key_request_complete(request, ck);
						break;
					}
					case Op::DECRYPT_KEY:
					{
						Cbe::Key_plaintext_value const pk =
							_trust_anchor.peek_completed_key_value_plaintext(request);

						_cbe->mark_generated_ta_decrypt_key_request_complete(request, pk);
						break;
					}
					case Op::LAST_SB_HASH:

						struct Ta_completed_last_sb_hash_request_for_cbe : Exception { };
						throw Ta_completed_last_sb_hash_request_for_cbe();

					case Op::INVALID:
						/* never reached */
						break;
					}
					_trust_anchor.drop_completed_request(request);
					progress |= true;
				}
			}

			_block_session->with_requests([&] (Block::Request request) {
				using namespace Genode;

				Cbe::Virtual_block_address const vba = request.operation.block_number;

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

			if (!_rekey && !_deinitialize && !_extend_vbd && !_extend_ft &&
			    !_creating_snapshot && !_discard_snapshot)
			{

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
			}

			if (!_creating_snapshot) {
				_block_session->with_create_snapshot([&] (Create_snapshot const) {

					if (!_cbe->client_request_acceptable()) {
						return;
					}

					Cbe::Request req {
						Cbe::Request::Operation::CREATE_SNAPSHOT,
						false,
						0,
						0,
						1,
						0,
						0 };

					_cbe->submit_client_request(req, 0);

					_creating_snapshot = true;
					progress |= true;
				});
			}

			if (_creating_snapshot) {

				Cbe::Request const &req {
					_cbe->peek_completed_client_request() };

				if (req.valid() &&
				    req.operation() == Cbe::Request::Operation::CREATE_SNAPSHOT)
				{
					
					_block_session->create_snapshot_done(req, _heap);

					_creating_snapshot = false;

					_cbe->drop_completed_client_request(req);
					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			if (!_discard_snapshot) {
				_block_session->with_discard_snapshot([&] (Generation gen)
				{
					if (!_cbe->client_request_acceptable()) {
						return;
					}

					Cbe::Request req {
						Cbe::Request::Operation::DISCARD_SNAPSHOT,
						false,
						0,
						0,
						1,
						0,
						0 };

					_cbe->submit_client_request(req, gen);
					_discard_snapshot = true;
					progress |= true;
				});
			}

			if (_discard_snapshot) {

				Cbe::Request const &req {
					_cbe->peek_completed_client_request() };

				if (req.valid() &&
				    req.operation() == Cbe::Request::Operation::DISCARD_SNAPSHOT)
				{
					_block_session->discard_snapshot_done(req, _heap);
					_discard_snapshot = false;

					_cbe->drop_completed_client_request(req);
					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			if (!_deinitialize) {
				_block_session->with_deinitialize([&] () {

					struct Deinitialize_request_not_acceptable { };
					if (!_cbe->client_request_acceptable()) {
						throw Deinitialize_request_not_acceptable();
					}

					Cbe::Request req(
						Cbe::Request::Operation::DEINITIALIZE,
						false,
						0,
						0,
						0,
						0,
						0);

					_cbe->submit_client_request(req, 0);
					_deinitialize = true;
					progress |= true;
				});
			}

			if (_deinitialize) {

				Cbe::Request const &req = _cbe->peek_completed_client_request();
				if (req.valid()) {

					struct Unexpected_request : Genode::Exception { };
					if (req.operation() != Cbe::Request::Operation::DEINITIALIZE)
					{
						throw Unexpected_request();
					}
					_block_session->deinitialize_done(req.success());

					_deinitialize = false;
					_cbe->drop_completed_client_request(req);

					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			_block_session->with_list_snapshots([&] () {
				Active_snapshot_ids ids;
				_cbe->active_snapshot_ids(ids);
				unsigned snap_nr { 0 };
				for (unsigned idx { 0 }; idx < sizeof(ids.values) / sizeof(ids.values[0]); idx++) {
					if (ids.values[idx] != 0) {
						log("   snap ", snap_nr, " generation ", ids.values[idx]);
						snap_nr++;
					}
				}
			});

			if (!_rekey) {
				_block_session->with_rekey([&] () {

					struct Rekey_request_not_acceptable { };
					if (!_cbe->client_request_acceptable()) {
						throw Rekey_request_not_acceptable();
					}

					Cbe::Request req(
						Cbe::Request::Operation::REKEY,
						false,
						0,
						0,
						0,
						0,
						0);

					_cbe->submit_client_request(req, 0);
					_rekey = true;
					progress |= true;
				});
			}

			if (_rekey) {

				Cbe::Request const &req = _cbe->peek_completed_client_request();
				if (req.valid()) {

					struct Unexpected_request : Genode::Exception { };
					if (req.operation() != Cbe::Request::Operation::REKEY)
					{
						throw Unexpected_request();
					}
					_block_session->rekey_done(req.success());

					_rekey = false;
					_cbe->drop_completed_client_request(req);

					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			if (!_extend_vbd) {
				_block_session->with_extend_vbd([&] (Extend_vbd const extend_vbd) {

					struct Extend_vbd_request_not_acceptable { };
					if (!_cbe->client_request_acceptable()) {
						throw Extend_vbd_request_not_acceptable();
					}

					Cbe::Request req(
						Cbe::Request::Operation::EXTEND_VBD,
						false,
						0,
						0,
						extend_vbd.nr_of_phys_blocks,
						0,
						0);

					_cbe->submit_client_request(req, 0);
					_extend_vbd_obj = {
						extend_vbd.nr_of_phys_blocks
					};
					_extend_vbd = true;
					progress |= true;
				});
			}

			if (_extend_vbd) {

				Cbe::Request const &req = _cbe->peek_completed_client_request();
				if (req.valid()) {

					struct Unexpected_request : Genode::Exception { };
					if (req.operation() != Cbe::Request::Operation::EXTEND_VBD ||
					    req.count() != _extend_vbd_obj.nr_of_phys_blocks)
					{
						throw Unexpected_request();
					}
					_block_session->extend_vbd_done(req.success());
					_extend_vbd_obj = { 0 };
					_extend_vbd = false;
					_cbe->drop_completed_client_request(req);

					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			if (!_extend_ft) {
				_block_session->with_extend_ft([&] (Extend_ft const extend_ft) {

					struct Extend_ft_request_not_acceptable { };
					if (!_cbe->client_request_acceptable()) {
						throw Extend_ft_request_not_acceptable();
					}

					Cbe::Request req(
						Cbe::Request::Operation::EXTEND_FT,
						false,
						0,
						0,
						extend_ft.nr_of_phys_blocks,
						0,
						0);

					_cbe->submit_client_request(req, 0);
					_extend_ft_obj = {
						extend_ft.nr_of_phys_blocks
					};
					_extend_ft = true;
					progress |= true;
				});
			}

			if (_extend_ft) {

				Cbe::Request const &req = _cbe->peek_completed_client_request();
				if (req.valid()) {

					struct Unexpected_request : Genode::Exception { };
					if (req.operation() != Cbe::Request::Operation::EXTEND_FT ||
					    req.count() != _extend_ft_obj.nr_of_phys_blocks)
					{
						throw Unexpected_request();
					}
					_block_session->extend_ft_done(req.success());

					_extend_ft_obj = { 0 };
					_extend_ft = false;
					_cbe->drop_completed_client_request(req);

					if (_block_session->cbe_request_next()) {
						_state = CBE;
					} else {
						_state = INVALID;
					}
					progress |= true;
				}
			}

			_cbe->execute(_blk_buf, _crypto_plain_buf, _crypto_cipher_buf);
			progress |= _cbe->execute_progress();

			using Payload = Block_session_component::Payload;

			/*
			 * Transfer read data from the CBE to the block buffer
			 */
			_block_session->with_payload([&] (Payload const &payload) {
				{
					Cbe::Request cbe_req { };
					uint64_t vba { 0 };
					Crypto_plain_buffer::Index plain_buf_idx { 0 };

					_cbe->client_transfer_read_data_required(
						cbe_req, vba, plain_buf_idx);

					if (!cbe_req.valid()) {
						return false;
					}
					Block::Request blk_req { };
					uint64_t buf_base { cbe_req.offset() };
					uint64_t blk_off { vba - cbe_req.block_number() };
					blk_req.offset = buf_base + (blk_off * BLOCK_SIZE);
					blk_req.operation.count = 1;

					payload.with_content(blk_req, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data {
							*reinterpret_cast<Cbe::Block_data*>(addr) };

						data = _crypto_plain_buf.item(plain_buf_idx);
					});
					_cbe->client_transfer_read_data_in_progress(
						plain_buf_idx);

					_cbe->client_transfer_read_data_completed(
						plain_buf_idx, true);

					progress |= true;
					return true;
				}
			});

			/*
			 * Transfer write data from the block buffer to the CBE
			 */
			_block_session->with_payload([&] (Payload const &payload) {
				{
					Cbe::Request cbe_req { };
					uint64_t vba { 0 };
					Crypto_plain_buffer::Index plain_buf_idx { 0 };

					_cbe->client_transfer_write_data_required(
						cbe_req, vba, plain_buf_idx);

					if (!cbe_req.valid()) {
						return false;
					}
					Block::Request blk_req { };
					uint64_t buf_base { cbe_req.offset() };
					uint64_t blk_off { vba - cbe_req.block_number() };
					blk_req.offset = buf_base + (blk_off * BLOCK_SIZE);
					blk_req.operation.count = 1;

					payload.with_content(blk_req, [&] (void *addr, Genode::size_t) {

						Cbe::Block_data &data {
							*reinterpret_cast<Cbe::Block_data*>(addr) };

						_crypto_plain_buf.item(plain_buf_idx) = data;
					});
					_cbe->client_transfer_write_data_in_progress(
						plain_buf_idx);

					_cbe->client_transfer_write_data_completed(
						plain_buf_idx, true);

					progress |= true;
					return true;
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
					request.tag(data_index.value);
					Block::Packet_descriptor::Opcode op;
					switch (request.operation()) {
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
						request.block_number(), request.count() };

					if (request.operation() == Request::Operation::WRITE) {
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet)) =
								_blk_buf.item(data_index);
					}
					_blk.tx()->try_submit_packet(packet);
					if (_verbose_back_end_io) {
						log ("   ", to_string(request.operation()), ": pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
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

				bool const bn_match = packet.block_number() == _blk_req.block_number();
				// assert packet descriptor belongs to stored backend request
				if (!bn_match || !op_match) { break; }

				_blk_req.success(packet.succeeded());

				Io_buffer::Index const data_index { _blk_req.tag() };
				bool             const success    { _blk_req.success() };
				if (read && success) {
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				_cbe->io_request_completed(data_index, success);
				progress |= true;

				_blk.tx()->release_packet(packet);

				_blk_req = Cbe::Request();
				io_progress |= true;
			}

			progress |= io_progress;

			/*********************
			 ** Crypto handling **
			 *********************/

			progress |= _crypto.execute();

			/* add keys */
			while (true) {
				Key key;
				Cbe::Request request = _cbe->crypto_add_key_required(key);
				if (!request.valid()) {
					break;
				}
				_cbe->crypto_add_key_requested(request);
				External::Crypto::Key_data data { };
				Genode::memcpy(
					data.value, key.value,
					sizeof(data.value) / sizeof(data.value[0]));

				_crypto.add_key(key.id, data);
				request.success (true);
				if (_verbose_back_end_crypto) {
					log("    add key: id " , (unsigned)key.id.value);
				}
				_cbe->crypto_add_key_completed(request);
				progress |= true;
			}

			/* remove keys */
			while (true) {
				Key::Id key_id;
				Cbe::Request request =
					_cbe->crypto_remove_key_required(key_id);

				if (!request.valid()) {
					break;
				}
				_cbe->crypto_remove_key_requested(request);
				_crypto.remove_key(key_id);
				request.success (true);
				if (_verbose_back_end_crypto) {
					log("    remove key: id " , (unsigned)key_id.value);
				}
				_cbe->crypto_remove_key_completed(request);
				progress |= true;
			}

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
				request.tag(data_index.value);
				_crypto.submit_encryption_request(request, _crypto_plain_buf.item(data_index), 0);
				_cbe->crypto_cipher_data_requested(data_index);
				if (_verbose_back_end_crypto) {
					log ("   encrypt: pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
				}
				progress |= true;
			}
			while (true) {
				Cbe::Request const request = _crypto.peek_completed_encryption_request();
				if (!request.valid()) {
					break;
				}
				Crypto_cipher_buffer::Index const data_index(request.tag());
				if (!_crypto.supply_cipher_data(request, _crypto_cipher_buf.item(data_index))) {
					break;
				}
				_cbe->supply_crypto_cipher_data(data_index, request.success());
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
				request.tag(data_index.value);
				_crypto.submit_decryption_request(request, _crypto_cipher_buf.item(data_index), 0);
				_cbe->crypto_plain_data_requested(data_index);
				if (_verbose_back_end_crypto) {
					log ("   decrypt: pba ", (unsigned long)request.block_number(), ", cnt ", (unsigned long)request.count());
				}
				progress |= true;
			}
			while (true) {
				Cbe::Request const request = _crypto.peek_completed_decryption_request();
				if (!request.valid()) {
					break;
				}
				Crypto_plain_buffer::Index const data_index(request.tag());
				if (!_crypto.supply_plain_data(request, _crypto_plain_buf.item(data_index))) {
					break;
				}
				_cbe->supply_crypto_plain_data(data_index, request.success());
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
						Cbe::Request(
							Cbe::Request::Operation::READ,
							false, 0, 0, 0, 0, 0),
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
						Cbe::Request(
							Cbe::Request::Operation::READ,
							false, 0, 0, 0, 0, 0));

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
						Cbe::Request(
							Cbe::Request::Operation::READ,
							false, 0, 0, 0, 0, 0),
						cfg);

					_state = CBE_DUMP;
					progress = true;
				});

				if (_state == CBE_INIT) {
					if (_cbe.constructed()) {
						_cbe.destruct();
					}
					_execute_cbe_init(progress);
				} else if (_state == CBE_CHECK) {
					if (_cbe.constructed()) {
						_cbe.destruct();
					}
					_execute_cbe_check(progress);
				} else if (_state == CBE_DUMP) {
					if (_cbe.constructed()) {
						_cbe.destruct();
					}
					_execute_cbe_dump(progress);
				} else if (_state == CBE) {
					if (!_cbe.constructed()) {
						_cbe.construct();
					}
					_execute_cbe(progress);
				} else if (_state == INVALID) {
					if (!_cbe.constructed()) {
						_cbe.construct();
					}
					_execute_cbe(progress);
				}
			}
			/* notify I/O backend */
			_blk.tx()->wakeup();

			/* notify client */
			_block_session->wakeup_client_if_needed();
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

void Component::construct(Genode::Env &env)
{
	env.exec_static_constructors();

	Cbe::assert_valid_object_size<Cbe::Library>();
	cbe_cxx_init();

	Cbe::assert_valid_object_size<Cbe_init::Library>();
	cbe_init_cxx_init();

	Cbe::assert_valid_object_size<Cbe_check::Library>();
	cbe_check_cxx_init();

	Cbe::assert_valid_object_size<Cbe_dump::Library>();
	cbe_dump_cxx_init();

	Cbe::assert_valid_object_size<External::Trust_anchor>();
	external_trust_anchor_cxx_init();

	Cbe::assert_valid_object_size<External::Crypto>();
	external_crypto_cxx_init();

	static Cbe::Main inst(env);
}

extern "C" int memcmp(const void *p0, const void *p1, Genode::size_t size)
{
	return Genode::memcmp(p0, p1, size);
}
