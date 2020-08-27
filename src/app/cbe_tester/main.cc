/*
 * \brief  Tool for running tests and benchmarks on the CBE
 * \author Martin Stein
 * \date   2020-08-26
 */

/*
 * Copyright (C) 2020 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

/* Genode includes */
#include <base/attached_rom_dataspace.h>
#include <base/component.h>
#include <base/heap.h>
#include <timer_session/connection.h>

/* CBE includes */
#include <cbe/library.h>
#include <cbe/external_crypto.h>
#include <cbe_check/library.h>
#include <cbe_dump/library.h>
#include <cbe_dump/configuration.h>
#include <cbe_init/library.h>
#include <cbe_init/configuration.h>
#include <cbe/external_ta.h>
#include <util.h>

using namespace Genode;
using namespace Cbe;


enum class Module_type : uint8_t
{
	CBE_INIT,
	CBE_DUMP,
	CBE_CHECK,
	CBE,
};


template <typename T>
T read_attribute(Xml_node const &node,
                 char     const *attr)
{
	T value { };

	if (!node.has_attribute(attr)) {

		error("<", node.type(), "> node misses attribute '", attr, "'");
		class Attribute_missing { };
		throw Attribute_missing();
	}
	if (!node.attribute(attr).value(value)) {

		error("<", node.type(), "> node has malformed '", attr, "' attribute");
		class Malformed_attribute { };
		throw Malformed_attribute();
	}
	return value;
}


static char const *blk_pkt_op_to_string(Block::Packet_descriptor::Opcode op)
{
	switch (op) {
	case Block::Packet_descriptor::READ: return "read";
	case Block::Packet_descriptor::WRITE: return "write";
	case Block::Packet_descriptor::SYNC: return "sync";
	case Block::Packet_descriptor::TRIM: return "trim";
	case Block::Packet_descriptor::END: return "end";
	};
	return "?";
}


static void print_blk_data(Block_data const &blk_data)
{
	for(unsigned long idx = 0; idx < Cbe::BLOCK_SIZE; idx += 64) {
		log(
			"  ", idx, ": ",
			Hex(blk_data.values[idx + 0], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 1], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 2], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 3], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 4], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 5], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 6], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 7], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 8], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 9], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 10], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 11], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 12], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 13], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 14], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 15], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 16], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 17], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 18], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 19], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 20], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 21], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 22], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 23], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 24], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 25], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 26], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 27], Hex::OMIT_PREFIX, Hex::PAD), " ",
			Hex(blk_data.values[idx + 28], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 29], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 30], Hex::OMIT_PREFIX, Hex::PAD),
			Hex(blk_data.values[idx + 31], Hex::OMIT_PREFIX, Hex::PAD));
	}
}


static String<128> blk_pkt_to_string(Block::Packet_descriptor const &packet)
{
	return
		String<128>(
			"op=", blk_pkt_op_to_string(packet.operation()),
			" vba=", packet.block_number(),
			" cnt=", packet.block_count(),
			" succ=", packet.succeeded(),
			" tag=", Hex(packet.tag().value));
}


class Config_node
{
	private:

		bool const _verbose_cmd_pool_cmd_pending    ;
		bool const _verbose_cmd_pool_cmd_in_progress;
		bool const _verbose_cmd_pool_cmd_completed  ;
		bool const _verbose_blk_pkt_in_progress     ;
		bool const _verbose_blk_pkt_completed       ;
		bool const _verbose_ta_req_in_progress      ;
		bool const _verbose_ta_req_completed        ;
		bool const _verbose_crypto_req_completed    ;
		bool const _verbose_crypto_req_in_progress  ;
		bool const _verbose_client_data_mismatch    ;
		bool const _verbose_client_data_transferred ;

	public:

		Config_node(Xml_node const &node)
		:
			_verbose_cmd_pool_cmd_pending     { node.attribute_value("verbose_cmd_pool_cmd_pending"    , false) },
			_verbose_cmd_pool_cmd_in_progress { node.attribute_value("verbose_cmd_pool_cmd_in_progress", false) },
			_verbose_cmd_pool_cmd_completed   { node.attribute_value("verbose_cmd_pool_cmd_completed"  , false) },
			_verbose_blk_pkt_in_progress      { node.attribute_value("verbose_blk_pkt_in_progress"     , false) },
			_verbose_blk_pkt_completed        { node.attribute_value("verbose_blk_pkt_completed"       , false) },
			_verbose_ta_req_in_progress       { node.attribute_value("verbose_ta_req_in_progress"      , false) },
			_verbose_ta_req_completed         { node.attribute_value("verbose_ta_req_completed"        , false) },
			_verbose_crypto_req_completed     { node.attribute_value("verbose_crypto_req_completed"    , false) },
			_verbose_crypto_req_in_progress   { node.attribute_value("verbose_crypto_req_in_progress"  , false) },
			_verbose_client_data_mismatch     { node.attribute_value("verbose_client_data_mismatch"    , false) },
			_verbose_client_data_transferred  { node.attribute_value("verbose_client_data_transferred" , false) }
		{ }

		bool verbose_cmd_pool_cmd_pending    () const { return _verbose_cmd_pool_cmd_pending    ; }
		bool verbose_cmd_pool_cmd_in_progress() const { return _verbose_cmd_pool_cmd_in_progress; }
		bool verbose_cmd_pool_cmd_completed  () const { return _verbose_cmd_pool_cmd_completed  ; }
		bool verbose_blk_pkt_in_progress     () const { return _verbose_blk_pkt_in_progress     ; }
		bool verbose_blk_pkt_completed       () const { return _verbose_blk_pkt_completed       ; }
		bool verbose_ta_req_in_progress      () const { return _verbose_ta_req_in_progress      ; }
		bool verbose_ta_req_completed        () const { return _verbose_ta_req_completed        ; }
		bool verbose_crypto_req_completed    () const { return _verbose_crypto_req_completed    ; }
		bool verbose_crypto_req_in_progress  () const { return _verbose_crypto_req_in_progress  ; }
		bool verbose_client_data_mismatch    () const { return _verbose_client_data_mismatch    ; }
		bool verbose_client_data_transferred () const { return _verbose_client_data_transferred ; }
};


class Benchmark_node
{
	public:

		using Label = String<128>;

		enum Operation { START, STOP };

	private:

		Operation const _op;
		bool      const _label_avail;
		Label     const _label;

		Operation _read_op_attr(Xml_node const &node)
		{
			class Attribute_missing { };
			if (!node.has_attribute("op")) {
				throw Attribute_missing();
			}
			if (node.attribute("op").has_value("start")) {
				return Operation::START;
			}
			if (node.attribute("op").has_value("stop")) {
				return Operation::STOP;
			}
			class Malformed_attribute { };
			throw Malformed_attribute();
		}

		static char const *_op_to_string(Operation op)
		{
			switch (op) {
			case START: return "start";
			case STOP: return "stop";
			}
			return "?";
		}

	public:

		bool has_attr_label() const
		{
			return _op == Operation::START;
		}

		Benchmark_node(Xml_node const &node)
		:
			_op          { _read_op_attr(node) },
			_label_avail { has_attr_label() && node.has_attribute("label") },
			_label       { _label_avail ?
			               node.attribute_value("label", Label { }) :
			               Label { } }
		{ }

		Operation op() const { return _op; }
		bool label_avail() const { return _label_avail; }
		Label const &label() const { return _label; }

		void print(Genode::Output &out) const
		{
			Genode::print(out, "op=", _op_to_string(_op));
		}
};


class Benchmark
{
	private:

		enum State { STARTED, STOPPED };

		Env                           &_env;
		Timer::Connection              _timer                   { _env };
		State                          _state                   { STOPPED };
		Microseconds                   _start_time              { 0 };
		uint64_t                       _nr_of_virt_blks_read    { 0 };
		uint64_t                       _nr_of_virt_blks_written { 0 };
		Constructible<Benchmark_node>  _start_node              { };
		uint64_t                       _id                      { 0 };

	public:

		Benchmark(Env &env) : _env { env } { }

		void submit_request(Benchmark_node const &node)
		{
			class Bad_state { };
			switch (node.op()) {
			case Benchmark_node::START:

				if (_state != STOPPED) {
					throw Bad_state { };
				}
				_id++;
				_nr_of_virt_blks_read = 0;
				_nr_of_virt_blks_written = 0;
				_state = STARTED;
				_start_node.construct(node);
				_start_time = _timer.curr_time().trunc_to_plain_us();
				break;

			case Benchmark_node::STOP:

				if (_state != STARTED) {
					throw Bad_state { };
				}
				uint64_t const stop_time_us {
					_timer.curr_time().trunc_to_plain_us().value };

				double const passed_time_sec {
					(double)(stop_time_us - _start_time.value) /
					(double)(1000 * 1000) };

				double const bytes_per_sec_read {
					((double)(_nr_of_virt_blks_read * Cbe::BLOCK_SIZE)) /
					passed_time_sec };

				double const bytes_per_sec_written {
					((double)(_nr_of_virt_blks_written * Cbe::BLOCK_SIZE)) /
					passed_time_sec };

				log("benchmark completed: id=", _id,
				    _start_node->label_avail() ? " label=(" : "",
				    _start_node->label_avail() ? _start_node->label() : "",
				    _start_node->label_avail() ? ")" : "",
				    " duration=(", passed_time_sec, " sec)",
				    " read=(", bytes_per_sec_read / (double)(1024 * 1024), " MiB/s)",
				    " write=(", bytes_per_sec_written / (double)(1024 * 1024), " MiB/s)");

				_state = STOPPED;
				break;
			}
		}

		void raise_nr_of_virt_blks_read()    { _nr_of_virt_blks_read++;    }
		void raise_nr_of_virt_blks_written() { _nr_of_virt_blks_written++; }
};


class Request_node
{
	private:

		using Operation = Cbe::Request::Operation;

		Operation             const _op;
		Virtual_block_address const _vba;
		Number_of_blocks      const _count;
		bool                  const _sync;
		bool                  const _salt_avail;
		uint64_t              const _salt;

		Operation _read_op_attr(Xml_node const &node)
		{
			class Attribute_missing { };
			if (!node.has_attribute("op")) {
				throw Attribute_missing();
			}
			if (node.attribute("op").has_value("read")) {
				return Operation::READ;
			}
			if (node.attribute("op").has_value("write")) {
				return Operation::WRITE;
			}
			if (node.attribute("op").has_value("sync")) {
				return Operation::SYNC;
			}
			if (node.attribute("op").has_value("create_snapshot")) {
				return Operation::CREATE_SNAPSHOT;
			}
			if (node.attribute("op").has_value("extend_ft")) {
				return Operation::EXTEND_FT;
			}
			if (node.attribute("op").has_value("extend_vbd")) {
				return Operation::EXTEND_VBD;
			}
			if (node.attribute("op").has_value("rekey")) {
				return Operation::REKEY;
			}
			if (node.attribute("op").has_value("deinitialize")) {
				return Operation::DEINITIALIZE;
			}
			class Malformed_attribute { };
			throw Malformed_attribute();
		}

	public:

		Request_node(Xml_node const &node)
		:
			_op         { _read_op_attr(node) },
			_vba        { has_attr_vba() ?
			              read_attribute<uint64_t>(node, "vba") : 0 },
			_count      { has_attr_count() ?
			              read_attribute<uint64_t>(node, "count") : 0 },
			_sync       { read_attribute<bool>(node, "sync") },
			_salt_avail { has_attr_salt() ?
			              node.has_attribute("salt") : false },
			_salt       { has_attr_salt() && _salt_avail ?
			              read_attribute<uint64_t>(node, "salt") : 0 }
		{ }

		Operation               op()         const { return _op; }
		Virtual_block_address   vba()        const { return _vba; }
		Number_of_blocks        count()      const { return _count; }
		bool                    sync()       const { return _sync; }
		bool                    salt_avail() const { return _salt_avail; }
		uint64_t                salt()       const { return _salt; }

		bool has_attr_vba() const
		{
			return _op == Operation::READ ||
			       _op == Operation::WRITE ||
			       _op == Operation::SYNC;
		}

		bool has_attr_salt() const
		{
			return _op == Operation::READ ||
			       _op == Operation::WRITE;
		}

		bool has_attr_count() const
		{
			return _op == Operation::READ ||
			       _op == Operation::WRITE ||
			       _op == Operation::SYNC ||
			       _op == Operation::EXTEND_FT ||
			       _op == Operation::EXTEND_VBD;
		}

		void print(Genode::Output &out) const
		{
			Genode::print(out, "op=", to_string(_op));
			if (has_attr_vba()) {
				Genode::print(out, " vba=", _vba);
			}
			if (has_attr_count()) {
				Genode::print(out, " count=", _count);
			}
			Genode::print(out, " sync=", _sync);
			if (_salt_avail) {
				Genode::print(out, " salt=", _salt);
			}
		}
};


class Command : public Fifo<Command>::Element
{
	public:

		enum Type
		{
			INVALID,
			REQUEST,
			BENCHMARK,
			CONSTRUCT,
			DESTRUCT,
			INITIALIZE,
			CHECK,
			DUMP,
			LIST_SNAPSHOTS
		};

		enum State
		{
			PENDING,
			IN_PROGRESS,
			COMPLETED
		};

	private:

		Type                                   _type           { INVALID };
		unsigned long                          _id             { 0 };
		State                                  _state          { PENDING };
		bool                                   _success        { false };
		bool                                   _data_mismatch  { false };
		Constructible<Request_node>            _request_node   { };
		Constructible<Benchmark_node>          _benchmark_node { };
		Constructible<Cbe_init::Configuration> _initialize     { };
		Constructible<Cbe_dump::Configuration> _dump           { };

		char const *_state_to_string() const
		{
			switch (_state) {
			case PENDING: return "pending";
			case IN_PROGRESS: return "in_progress";
			case COMPLETED: return "completed";
			}
			return "?";
		}

		char const *_type_to_string() const
		{
			switch (_type) {
			case INITIALIZE: return "initialize";
			case INVALID: return "invalid";
			case DUMP: return "dump";
			case REQUEST: return "request";
			case BENCHMARK: return "benchmark";
			case CONSTRUCT: return "construct";
			case DESTRUCT: return "destruct";
			case CHECK: return "check";
			case LIST_SNAPSHOTS: return "list_snapshots";
			}
			return "?";
		}

	public:

		Command() { }

		Command(Type            type,
		        Xml_node const &node,
		        unsigned long   id)
		:
			_type { type },
			_id   { id }
		{
			switch (_type) {
			case INITIALIZE: _initialize.construct(node);     break;
			case DUMP:       _dump.construct(node);           break;
			case REQUEST:    _request_node.construct(node);   break;
			case BENCHMARK:  _benchmark_node.construct(node); break;
			default:                                          break;
			}
		}

		Command(Command &other)
		:
			_type    { other._type },
			_id      { other._id },
			_state   { other._state },
			_success { other._success }
		{
			switch (_type) {
			case INITIALIZE: _initialize.construct(*other._initialize);         break;
			case DUMP:       _dump.construct(*other._dump);                     break;
			case REQUEST:    _request_node.construct(*other._request_node);     break;
			case BENCHMARK:  _benchmark_node.construct(*other._benchmark_node); break;
			default:                                                            break;
			}
		}

		bool has_attr_data_mismatch() const
		{
			return
				_type == REQUEST &&
				_request_node->op() == Cbe::Request::Operation::READ &&
				_request_node->salt_avail();
		}

		bool synchronize() const
		{
			class Bad_type { };
			switch (_type) {
			case INITIALIZE:     return true;
			case BENCHMARK:      return true;
			case CONSTRUCT:      return true;
			case DESTRUCT:       return true;
			case DUMP:           return true;
			case CHECK:          return true;
			case LIST_SNAPSHOTS: return true;
			case REQUEST:        return _request_node->sync();
			case INVALID:        throw Bad_type();
			}
			throw Bad_type();
		}

		static Type type_from_string(String<64> str)
		{
			if (str == "initialize")     { return INITIALIZE; }
			if (str == "request")        { return REQUEST; }
			if (str == "benchmark")      { return BENCHMARK; }
			if (str == "construct")      { return CONSTRUCT; }
			if (str == "destruct")       { return DESTRUCT; }
			if (str == "check")          { return CHECK; }
			if (str == "dump")           { return DUMP; }
			if (str == "list-snapshots") { return LIST_SNAPSHOTS; }
			class Bad_string { };
			throw Bad_string();
		}

		void print(Genode::Output &out) const
		{
			Genode::print(out, "id=", _id, " type=", _type_to_string());
			class Bad_type { };
			switch (_type) {
			case INITIALIZE:     Genode::print(out, " cfg=(", *_initialize, ")"); break;
			case REQUEST:        Genode::print(out, " cfg=(", *_request_node, ")"); break;
			case BENCHMARK:      Genode::print(out, " cfg=(", *_benchmark_node, ")"); break;
			case DUMP:           Genode::print(out, " cfg=(", *_dump, ")"); break;
			case INVALID:        break;
			case CHECK:          break;
			case CONSTRUCT:      break;
			case DESTRUCT:       break;
			case LIST_SNAPSHOTS: break;
			}
			Genode::print(out, ") succ=", _success);
			if (has_attr_data_mismatch()) {
				Genode::print(out, " bad_data=", _data_mismatch);
			}
			Genode::print(out, " state=", _state_to_string());
		}

		Type                           type          () const { return _type           ; }
		State                          state         () const { return _state          ; }
		unsigned long                  id            () const { return _id             ; }
		bool                           success       () const { return _success        ; }
		bool                           data_mismatch () const { return _data_mismatch  ; }
		Request_node            const &request_node  () const { return *_request_node  ; }
		Benchmark_node          const &benchmark_node() const { return *_benchmark_node; }
		Cbe_init::Configuration const &initialize    () const { return *_initialize    ; }
		Cbe_dump::Configuration const &dump          () const { return *_dump          ; }

		void state         (State state)        { _state = state; }
		void success       (bool success)       { _success = success; }
		void data_mismatch (bool data_mismatch) { _data_mismatch = data_mismatch; }

};


class Command_pool {

	private:

		Allocator         &_alloc;
		Config_node const &_config_node;
		Fifo<Command>      _cmd_queue              { };
		unsigned long      _next_command_id        { 0 };
		unsigned long      _nr_of_uncompleted_cmds { 0 };
		unsigned long      _nr_of_errors           { 0 };
		Block_data         _blk_data               { };

		void _read_cmd_node(Xml_node const &node,
		                    Command::Type   cmd_type)
		{
			Command &cmd {
				*new (_alloc) Command(cmd_type, node, _next_command_id++) };

			_nr_of_uncompleted_cmds++;
			_cmd_queue.enqueue(cmd);

			if (_config_node.verbose_cmd_pool_cmd_pending()) {
				log("cmd pending: ", &cmd, " ", cmd);
			}
		}

		static void _generate_blk_data(Block_data            &blk_data,
		                               Virtual_block_address  vba,
		                               uint64_t               salt)
		{
			for (uint64_t idx { 0 };
			     idx + sizeof(vba) + sizeof(salt) <=
			        sizeof(blk_data.values) / sizeof(blk_data.values[0]); )
			{
				memcpy(&blk_data.values[idx], &vba, sizeof(vba));
				idx += sizeof(vba);
				memcpy(&blk_data.values[idx], &salt, sizeof(salt));
				idx += sizeof(salt);
				vba += idx + salt;
				salt += idx + vba;
			}
		}

	public:

		Command_pool(Allocator         &alloc,
		             Xml_node    const &config_xml,
		             Config_node const &config_node)
		:
			_alloc       { alloc },
			_config_node { config_node }
		{
			config_xml.for_each_sub_node([&] (Xml_node const &node) {
				_read_cmd_node(node, Command::type_from_string(node.type()));
			});
		}

		Command peek_pending_command(Command::Type type) const
		{
			Reconstructible<Command> resulting_cmd { };
			bool first_uncompleted_cmd { true };
			bool exit_loop { false };
			_cmd_queue.for_each([&] (Command &curr_cmd)
			{
				if (exit_loop) {
					return;
				}
				switch (curr_cmd.state()) {
				case Command::PENDING:

					/*
					 * Stop iterating at the first uncompleted command
					 * that needs to be synchronized.
					 */
					if (curr_cmd.synchronize()) {
						if (curr_cmd.type() == type && first_uncompleted_cmd) {
							resulting_cmd.construct(curr_cmd);
						}
						exit_loop = true;
						return;
					}
					/*
					 * Select command and stop iterating if the command is of
					 * the desired type.
					 */
					if (curr_cmd.type() == type) {
						resulting_cmd.construct(curr_cmd);
						exit_loop = true;
					}
					first_uncompleted_cmd = false;
					return;

				case Command::IN_PROGRESS:

					/*
					 * Stop iterating at the first uncompleted command
					 * that needs to be synchronized.
					 */
					if (curr_cmd.synchronize()) {
						exit_loop = true;
						return;
					}
					first_uncompleted_cmd = false;
					return;

				case Command::COMPLETED:

					return;
				}
			});
			return *resulting_cmd;
		}

		void mark_command_in_progress(unsigned long cmd_id)
		{
			bool exit_loop { false };
			_cmd_queue.for_each([&] (Command &cmd)
			{
				if (exit_loop) {
					return;
				}
				if (cmd.id() == cmd_id) {
					if (cmd.state() != Command::PENDING) {
						class Bad_state { };
						throw Bad_state();
					}
					cmd.state(Command::IN_PROGRESS);
					exit_loop = true;

					if (_config_node.verbose_cmd_pool_cmd_in_progress()) {
						log("cmd in progress: ", cmd);
					}
				}
			});
		}

		void mark_command_completed(unsigned long cmd_id,
		                            bool          success)
		{
			bool exit_loop { false };
			_cmd_queue.for_each([&] (Command &cmd)
			{
				if (exit_loop) {
					return;
				}
				if (cmd.id() == cmd_id) {
					if (cmd.state() != Command::IN_PROGRESS) {
						class Bad_state { };
						throw Bad_state();
					}
					cmd.state(Command::COMPLETED);
					_nr_of_uncompleted_cmds--;
					cmd.success(success);
					if (!cmd.success()) {
						_nr_of_errors++;
					}
					exit_loop = true;

					if (_config_node.verbose_cmd_pool_cmd_completed()) {
						log("cmd completed: ", cmd);
					}
				}
			});
		}

		void generate_blk_data(Cbe::Request           cbe_req,
		                       Virtual_block_address  vba,
		                       Block_data            &blk_data) const
		{
			bool exit_loop { false };
			_cmd_queue.for_each([&] (Command &cmd)
			{
				if (exit_loop) {
					return;
				}
				if (cmd.id() != cbe_req.tag()) {
					return;
				}
				if (cmd.type() != Command::REQUEST) {
					class Bad_command_type { };
					throw Bad_command_type();
				}
				Request_node const &req_node { cmd.request_node() };
				if (req_node.salt_avail()) {

					_generate_blk_data(blk_data, vba, req_node.salt());
				}
				exit_loop = true;
			});
		}

		void verify_blk_data(Cbe::Request           cbe_req,
		                     Virtual_block_address  vba,
		                     Block_data      const &blk_data)
		{
			bool exit_loop { false };
			_cmd_queue.for_each([&] (Command &cmd)
			{
				if (exit_loop) {
					return;
				}
				if (cmd.id() != cbe_req.tag()) {
					return;
				}
				if (cmd.type() != Command::REQUEST) {
					class Bad_command_type { };
					throw Bad_command_type();
				}
				Request_node const &req_node { cmd.request_node() };
				if (req_node.salt_avail()) {

					Block_data gen_blk_data { };
					_generate_blk_data(gen_blk_data, vba, req_node.salt());

					if (memcmp(blk_data.values, gen_blk_data.values,
					           sizeof(blk_data.values) /
					           sizeof(blk_data.values[0]))) {

						cmd.data_mismatch(true);
						_nr_of_errors++;

						if (_config_node.verbose_client_data_mismatch()) {
							log("client data mismatch: vba=", vba,
							    " req=(", cbe_req, ")");
							log("client data should be:");
							print_blk_data(gen_blk_data);
							log("client data is:");
							print_blk_data(blk_data);
							class Client_data_mismatch { };
							throw Client_data_mismatch();
						}
					}
				}
				exit_loop = true;
			});
		}

		void print_failed_cmds() const
		{
			_cmd_queue.for_each([&] (Command &cmd)
			{
				if (cmd.state() != Command::COMPLETED) {
					return;
				}
				if (cmd.success() &&
				    (!cmd.has_attr_data_mismatch() || !cmd.data_mismatch())) {

					return;
				}
				log("cmd failed: ", cmd);
			});
		}

		unsigned long nr_of_uncompleted_cmds() { return _nr_of_uncompleted_cmds; }
		unsigned long nr_of_errors()           { return _nr_of_errors; }
};


class Main
{
	private:

		enum { TX_BUF_SIZE = Block::Session::TX_QUEUE_SIZE * BLOCK_SIZE, };

		Env                         &_env;
		Attached_rom_dataspace       _config_rom              { _env, "config" };
		Config_node                  _config_node             { _config_rom.xml() };
		Heap                         _heap                    { _env.ram(), _env.rm() };
		Allocator_avl                _blk_alloc               { &_heap };
		Block::Connection<>          _blk                     { _env, &_blk_alloc, TX_BUF_SIZE };
		Signal_handler<Main>         _blk_sigh                { _env.ep(), *this, &Main::_execute };
		Io_buffer                    _blk_buf                 { };
		Command_pool                 _cmd_pool                { _heap, _config_rom.xml(), _config_node };
		Constructible<Cbe::Library>  _cbe                     { };
		Cbe_check::Library           _cbe_check               { };
		Cbe_dump::Library            _cbe_dump                { };
		Cbe_init::Library            _cbe_init                { };
		Benchmark                    _benchmark               { _env };
		Cbe::Hash                    _trust_anchor_sb_hash    { };
		External::Trust_anchor       _trust_anchor            { };
		Crypto_plain_buffer          _crypto_plain_buf        { };
		Crypto_cipher_buffer         _crypto_cipher_buf       { };
		External::Crypto             _crypto                  { };

		Module_type _packet_module_type(Block::Packet_descriptor const & pkt)
		{
			class Bad_tag { };
			switch (((uint32_t)pkt.tag().value & 0xff000000) >> 24) {
			case 1: return Module_type::CBE_INIT;
			case 2: return Module_type::CBE;
			case 3: return Module_type::CBE_DUMP;
			case 4: return Module_type::CBE_CHECK;
			default: throw Bad_tag();
			}
		}

		Cbe::Io_buffer::Index _packet_io_buf_idx(Block::Packet_descriptor const & pkt)
		{
			return
				Cbe::Io_buffer::Index {
					(uint32_t)pkt.tag().value & 0xffffff };
		}

		uint32_t _module_type_to_uint32(Module_type type)
		{
			class Bad_type { };
			switch (type) {
			case Module_type::CBE_INIT : return 1;
			case Module_type::CBE      : return 2;
			case Module_type::CBE_DUMP : return 3;
			case Module_type::CBE_CHECK: return 4;
			}
			throw Bad_type();
		}

		Module_type _module_type_from_uint32(uint32_t type)
		{
			class Bad_type { };
			switch (type) {
			case 1: return Module_type::CBE_INIT ;
			case 2: return Module_type::CBE      ;
			case 3: return Module_type::CBE_DUMP ;
			case 4: return Module_type::CBE_CHECK;
			default: ;
			}
			throw Bad_type();
		}

		Module_type
		_ta_request_get_module_type(Trust_anchor_request const &ta_req)
		{
			return
				_module_type_from_uint32(
					((uint32_t)ta_req.tag() >> 24) & 0xff);
		}

		void _ta_request_unset_module_type(Trust_anchor_request &ta_req)
		{
			ta_req.tag((uint32_t)ta_req.tag() & ~((uint32_t)0xff << 24));
		}

		void _ta_request_set_module_type(Trust_anchor_request &ta_req,
		                                 Module_type           type)
		{
			class Bad_tag { };
			if (ta_req.tag() & 0xff000000) {
				throw Bad_tag();
			}
			ta_req.tag((uint32_t)ta_req.tag() |
			           (_module_type_to_uint32(type) << 24));
		}

		template <typename MODULE>
		void _handle_pending_blk_io_requests_of_module(MODULE      &module,
		                                               Module_type  module_type,
		                                               bool        &progress)
		{
			while (true) {

				if (!_blk.tx()->ready_to_submit()) {
					break;
				}
				Cbe::Io_buffer::Index data_index { 0 };
				Cbe::Request cbe_req { };
				module.has_io_request(cbe_req, data_index);

				if (!cbe_req.valid()) {
					break;
				}
				Block::Packet_descriptor::Opcode blk_op {
					Block::Packet_descriptor::END };

				if (!_cbe_op_to_block_op(cbe_req.operation(), blk_op)) {
					break;
				};
				Block::Packet_descriptor packet;
				class Bad_data_index { };
				if (data_index.value & 0xff000000) {
					throw Bad_data_index();
				}
				
				try {
					packet = {
						_blk.alloc_packet(Cbe::BLOCK_SIZE),
						blk_op,
						cbe_req.block_number(),
						cbe_req.count(),
						Block::Packet_descriptor::Tag {
							(uint32_t)data_index.value |
							(_module_type_to_uint32(module_type) << 24)
						} };
				}
				catch (Block::Session::Tx::Source::Packet_alloc_failed) {
					break;
				}
				if (cbe_req.operation() == Cbe::Request::Operation::WRITE) {

					*reinterpret_cast<Cbe::Block_data*>(
						_blk.tx()->packet_content(packet)) =
					_blk_buf.item(data_index);
				}
				if (_config_node.verbose_blk_pkt_in_progress()) {
					log("blk pkt in progress: ", blk_pkt_to_string(packet));
				}
				_blk.tx()->try_submit_packet(packet);
				module.io_request_in_progress(data_index);

				progress = true;
			}
		}

		template <typename MODULE>
		void _handle_completed_client_requests_of_module(MODULE &module,
		                                                 bool   &progress)
		{
			while (true) {

				Cbe::Request const cbe_req {
					module.peek_completed_client_request() };

				if (!cbe_req.valid()) {
					break;
				}
				_cmd_pool.mark_command_completed(cbe_req.tag(),
				                                 cbe_req.success());

				module.drop_completed_client_request(cbe_req);
				progress = true;
			}
		}

		void _execute_cbe_dump (bool &progress)
		{
			_cbe_dump.execute(_blk_buf);
			if (_cbe_dump.execute_progress()) {
				progress = true;
			}
			_handle_pending_blk_io_requests_of_module(
				_cbe_dump, Module_type::CBE_DUMP, progress);

			_handle_completed_client_requests_of_module(_cbe_dump, progress);
		}

		bool _cbe_op_to_block_op(Cbe::Request::Operation           cbe_op,
		                         Block::Packet_descriptor::Opcode &blk_op)
		{
			switch (cbe_op) {
			case Cbe::Request::Operation::READ:
				blk_op = Block::Packet_descriptor::READ;
				return true;
			case Cbe::Request::Operation::WRITE:
				blk_op = Block::Packet_descriptor::WRITE;
				return true;
				break;
			case Cbe::Request::Operation::SYNC:
				blk_op = Block::Packet_descriptor::SYNC;
				return true;
				break;
			default:
				error("failed to convert CBE request operation to block ",
				      "packet opcode");
				return true;
			}
		}

		template <typename MODULE>
		void _handle_pending_ta_requests_of_module(MODULE      &module,
		                                           Module_type  module_type,
		                                           bool        &progress)
		{
			using Ta_operation = Cbe::Trust_anchor_request::Operation;
			while (true) {

				if (!_trust_anchor.request_acceptable()) {
					break;
				}
				Cbe::Trust_anchor_request ta_req =
					module.peek_generated_ta_request();

				if (ta_req.operation() == Ta_operation::INVALID) {
					return;
				}
				Cbe::Trust_anchor_request typed_ta_req { ta_req };
				_ta_request_set_module_type(typed_ta_req, module_type);

				if (_config_node.verbose_ta_req_in_progress()) {
					log("ta req in progress: ", typed_ta_req);
				}
				switch (ta_req.operation()) {
				case Ta_operation::CREATE_KEY:

					_trust_anchor.submit_create_key_request(typed_ta_req);
					module.drop_generated_ta_request(ta_req);
					progress = true;
					break;

				case Ta_operation::SECURE_SUPERBLOCK:

					_trust_anchor_sb_hash =
						module.peek_generated_ta_sb_hash(ta_req);

					_trust_anchor.submit_secure_superblock_request(
						typed_ta_req, _trust_anchor_sb_hash);

					module.drop_generated_ta_request(ta_req);
					progress = true;
					break;

				case Ta_operation::ENCRYPT_KEY:

					_trust_anchor.submit_encrypt_key_request(
						typed_ta_req,
						module.peek_generated_ta_key_value_plaintext(ta_req));

					module.drop_generated_ta_request(ta_req);
					progress = true;
					break;

				case Ta_operation::DECRYPT_KEY:

					_trust_anchor.submit_decrypt_key_request(
						typed_ta_req,
						module.peek_generated_ta_key_value_ciphertext(ta_req));

					module.drop_generated_ta_request(ta_req);
					progress = true;
					break;

				case Ta_operation::LAST_SB_HASH:

					module.drop_generated_ta_request(ta_req);
					module.mark_generated_ta_last_sb_hash_request_complete(
						ta_req, _trust_anchor_sb_hash);

					progress = true;
					break;

				case Ta_operation::INVALID:

					return;
				}
			}
		}

		void _execute_cbe_init(bool &progress)
		{
			_cbe_init.execute(_blk_buf);
			if (_cbe_init.execute_progress()) {
				progress = true;
			}
			_handle_pending_blk_io_requests_of_module(
				_cbe_init, Module_type::CBE_INIT, progress);

			_handle_pending_ta_requests_of_module(
				_cbe_init, Module_type::CBE_INIT, progress);

			_handle_completed_client_requests_of_module(_cbe_init, progress);
		}

		void _cbe_transfer_client_data_that_was_read(bool &progress)
		{
			while (true) {

				Cbe::Request request { };
				uint64_t vba { 0 };
				Crypto_plain_buffer::Index plain_buf_idx { 0 };
				_cbe->client_transfer_read_data_required(
					request, vba, plain_buf_idx);

				if (!request.valid()) {
					break;
				}
				_cmd_pool.verify_blk_data(
					request, vba, _crypto_plain_buf.item(plain_buf_idx));

				_cbe->client_transfer_read_data_in_progress(plain_buf_idx);
				_cbe->client_transfer_read_data_completed(plain_buf_idx, true);
				_benchmark.raise_nr_of_virt_blks_read();
				progress = true;

				if (_config_node.verbose_client_data_transferred()) {
					log("client data: vba=", vba, " req=(", request, ")");
				}
			}
		}

		void _cbe_transfer_client_data_that_will_be_written(bool &progress)
		{
			while (true) {

				Cbe::Request request { };
				uint64_t vba { 0 };
				Crypto_plain_buffer::Index plain_buf_idx { 0 };
				_cbe->client_transfer_write_data_required(
					request, vba, plain_buf_idx);

				if (!request.valid()) {
					return;
				}
				_cmd_pool.generate_blk_data(
					request, vba, _crypto_plain_buf.item(plain_buf_idx));

				_cbe->client_transfer_write_data_in_progress(plain_buf_idx);
				_cbe->client_transfer_write_data_completed(
					plain_buf_idx, true);

				_benchmark.raise_nr_of_virt_blks_written();
				progress = true;

				if (_config_node.verbose_client_data_transferred()) {
					log("client data: vba=", vba, " req=(", request, ")");
				}
			}
		}

		void _cbe_handle_crypto_add_key_requests(bool &progress)
		{
			while (true) {

				Key key;
				Cbe::Request request { _cbe->crypto_add_key_required(key) };
				if (!request.valid()) {
					break;
				}
				_cbe->crypto_add_key_requested(request);
				External::Crypto::Key_data data { };
				memcpy(
					data.value, key.value,
					sizeof(data.value) / sizeof(data.value[0]));

				_crypto.add_key(key.id, data);
				request.success (true);
				_cbe->crypto_add_key_completed(request);
				progress = true;

				if (_config_node.verbose_crypto_req_in_progress()) {
					log("crypto req in progress: ", request);
				}
			}
		}

		void _cbe_handle_crypto_remove_key_requests(bool &progress)
		{
			while (true) {

				Key::Id key_id;
				Cbe::Request request {
					_cbe->crypto_remove_key_required(key_id) };

				if (!request.valid()) {
					break;
				}
				_cbe->crypto_remove_key_requested(request);
				_crypto.remove_key(key_id);
				request.success (true);
				_cbe->crypto_remove_key_completed(request);
				progress = true;

				if (_config_node.verbose_crypto_req_in_progress()) {
					log("crypto req in progress: ", request);
				}
			}
		}

		void _cbe_handle_crypto_encrypt_requests(bool &progress)
		{
			while (true) {

				if (!_crypto.encryption_request_acceptable()) {
					break;
				}
				Crypto_plain_buffer::Index data_index { 0 };
				Cbe::Request request {
					_cbe->crypto_cipher_data_required(data_index) };

				if (!request.valid()) {
					break;
				}
				request.tag(data_index.value);
				_crypto.submit_encryption_request(
					request,
					_crypto_plain_buf.item(data_index),
					0);

				_cbe->crypto_cipher_data_requested(data_index);
				progress = true;

				if (_config_node.verbose_crypto_req_in_progress()) {
					log("crypto req in progress: ", request);
				}
			}
		}

		void _cbe_handle_crypto_decrypt_requests(bool &progress)
		{
			while (true) {

				if (!_crypto.decryption_request_acceptable()) {
					break;
				}
				Crypto_cipher_buffer::Index data_index { 0 };
				Cbe::Request request {
					_cbe->crypto_plain_data_required(data_index) };

				if (!request.valid()) {
					break;
				}
				request.tag(data_index.value);
				_crypto.submit_decryption_request(
					request,
					_crypto_cipher_buf.item(data_index),
					0);

				_cbe->crypto_plain_data_requested(data_index);
				progress = true;

				if (_config_node.verbose_crypto_req_in_progress()) {
					log("crypto req in progress: ", request);
				}
			}
		}

		void _cbe_handle_crypto_requests(bool &progress)
		{
			_cbe_handle_crypto_add_key_requests(progress);
			_cbe_handle_crypto_remove_key_requests(progress);
			_cbe_handle_crypto_encrypt_requests(progress);
			_cbe_handle_crypto_decrypt_requests(progress);
		}

		void _execute_cbe(bool &progress)
		{
			_cbe->execute(_blk_buf, _crypto_plain_buf, _crypto_cipher_buf);
			if (_cbe->execute_progress()) {
				progress = true;
			}
			_handle_pending_blk_io_requests_of_module(
				*_cbe, Module_type::CBE, progress);

			_handle_pending_ta_requests_of_module(
				*_cbe, Module_type::CBE, progress);

			_cbe_handle_crypto_requests(progress);
			_cbe_transfer_client_data_that_was_read(progress);
			_cbe_transfer_client_data_that_will_be_written(progress);
			_handle_completed_client_requests_of_module(*_cbe, progress);
		}

		void _cmd_pool_handle_pending_cbe_init_cmds(bool &progress)
		{
			while (true) {

				if (!_cbe_init.client_request_acceptable()) {
					break;
				}
				Command const cmd {
					_cmd_pool.peek_pending_command(Command::INITIALIZE) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				Cbe_init::Configuration const &cfg { cmd.initialize() };

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

				_cmd_pool.mark_command_in_progress(cmd.id());
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_check_cmds(bool &progress)
		{
			while (true) {

				if (!_cbe_check.client_request_acceptable()) {
					break;
				}
				Command const cmd {
					_cmd_pool.peek_pending_command(Command::CHECK) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				_cbe_check.submit_client_request(
					Cbe::Request {
						Cbe::Request::Operation::READ,
						false,
						0,
						0,
						0,
						0,
						(uint32_t)cmd.id()
					}
				);
				_cmd_pool.mark_command_in_progress(cmd.id());
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_cbe_cmds(bool &progress)
		{
			while (true) {

				if (!_cbe->client_request_acceptable()) {
					break;
				}
				Command const cmd {
					_cmd_pool.peek_pending_command(Command::REQUEST) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				Request_node const &req_node { cmd.request_node() };
				Cbe::Request const &cbe_req {
					cmd.request_node().op(),
					false,
					req_node.has_attr_vba() ? req_node.vba() : 0,
					0,
					req_node.has_attr_count() ? req_node.count() : 0,
					0,
					(uint32_t)cmd.id() };

				_cbe->submit_client_request(cbe_req, 0);
				_cmd_pool.mark_command_in_progress(cmd.id());
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_dump_cmds(bool &progress)
		{
			while (true) {

				if (!_cbe_dump.client_request_acceptable()) {
					break;
				}
				Command const cmd {
					_cmd_pool.peek_pending_command(Command::DUMP) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				Cbe_dump::Configuration const &cfg { cmd.dump() };
				_cbe_dump.submit_client_request(
					Cbe::Request(
						Cbe::Request::Operation::READ,
						false, 0, 0, 0, 0, (uint32_t)cmd.id()),
					cfg);

				_cmd_pool.mark_command_in_progress(cmd.id());
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_construct_cmds(bool &progress)
		{
			while (true) {

				Command const cmd {
					_cmd_pool.peek_pending_command(Command::CONSTRUCT) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				_cbe.construct();
				_cmd_pool.mark_command_in_progress(cmd.id());
				_cmd_pool.mark_command_completed(cmd.id(), true);
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_destruct_cmds(bool &progress)
		{
			while (true) {

				Command const cmd {
					_cmd_pool.peek_pending_command(Command::DESTRUCT) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				_cbe.destruct();
				_cmd_pool.mark_command_in_progress(cmd.id());
				_cmd_pool.mark_command_completed(cmd.id(), true);
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_list_snapshots_cmds(bool &progress)
		{
			while (true) {

				Command const cmd {
					_cmd_pool.peek_pending_command(Command::LIST_SNAPSHOTS) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				Active_snapshot_ids ids;
				_cbe->active_snapshot_ids(ids);
				unsigned snap_nr { 0 };
				for (unsigned idx { 0 }; idx < sizeof(ids.values) / sizeof(ids.values[0]); idx++) {
					if (ids.values[idx] != 0) {
						log("list snapshots: cmd=", cmd.id(), " snap=", snap_nr, " gen=", ids.values[idx]);
						snap_nr++;
					}
				}
				_cmd_pool.mark_command_in_progress(cmd.id());
				_cmd_pool.mark_command_completed(cmd.id(), true);
				progress = true;
			}
		}

		void _cmd_pool_handle_pending_benchmark_cmds(bool &progress)
		{
			while (true) {

				Command const cmd {
					_cmd_pool.peek_pending_command(Command::BENCHMARK) };

				if (cmd.type() == Command::INVALID) {
					break;
				}
				_benchmark.submit_request(cmd.benchmark_node());
				_cmd_pool.mark_command_in_progress(cmd.id());
				_cmd_pool.mark_command_completed(cmd.id(), true);
				progress = true;
			}
		}

		void _execute_cbe_check (bool &progress)
		{
			_cbe_check.execute(_blk_buf);
			if (_cbe_check.execute_progress()) {
				progress = true;
			}
			_handle_pending_blk_io_requests_of_module(
				_cbe_check, Module_type::CBE_CHECK, progress);

			_handle_completed_client_requests_of_module(_cbe_check, progress);
		}

		void _execute_command_pool(bool &progress)
		{
			if (_cbe.constructed()) {
				_cmd_pool_handle_pending_cbe_cmds(progress);
				_cmd_pool_handle_pending_list_snapshots_cmds(progress);
			}
			_cmd_pool_handle_pending_cbe_init_cmds(progress);
			_cmd_pool_handle_pending_benchmark_cmds(progress);
			_cmd_pool_handle_pending_construct_cmds(progress);
			_cmd_pool_handle_pending_destruct_cmds(progress);
			_cmd_pool_handle_pending_dump_cmds(progress);
			_cmd_pool_handle_pending_check_cmds(progress);

			if (_cmd_pool.nr_of_uncompleted_cmds() == 0) {

				if (_cmd_pool.nr_of_errors() > 0) {

					_cmd_pool.print_failed_cmds();
					_env.parent().exit(-1);

				} else {

					_env.parent().exit(0);
				}
			}
		}

		template <typename MODULE>
		void _trust_anchor_handle_completed_requests_of_module(MODULE                     &module,
		                                                       Trust_anchor_request const &typed_ta_req,
		                                                       bool                       &progress)
		{
			using Ta_operation = Cbe::Trust_anchor_request::Operation;

			Trust_anchor_request ta_req { typed_ta_req };
			_ta_request_unset_module_type(ta_req);

			if (_config_node.verbose_ta_req_completed()) {
				log("ta req completed: ", typed_ta_req);
			}
			switch (ta_req.operation()) {
			case Ta_operation::CREATE_KEY:

				module.mark_generated_ta_create_key_request_complete(
					ta_req,
					_trust_anchor.peek_completed_key_value_plaintext(
						typed_ta_req));

				_trust_anchor.drop_completed_request(typed_ta_req);
				progress = true;
				break;

			case Ta_operation::SECURE_SUPERBLOCK:

				module.mark_generated_ta_secure_sb_request_complete(
					ta_req);

				_trust_anchor.drop_completed_request(typed_ta_req);
				progress = true;
				break;

			case Ta_operation::ENCRYPT_KEY:

				module.mark_generated_ta_encrypt_key_request_complete(
					ta_req,
					_trust_anchor.peek_completed_key_value_ciphertext(
						typed_ta_req));

				_trust_anchor.drop_completed_request(typed_ta_req);
				progress = true;
				break;

			case Ta_operation::DECRYPT_KEY:

				module.mark_generated_ta_decrypt_key_request_complete(
					ta_req,
					_trust_anchor.peek_completed_key_value_plaintext(
						typed_ta_req));

				_trust_anchor.drop_completed_request(typed_ta_req);
				progress = true;
				break;

			default:

				class Bad_ta_operation { };
				throw Bad_ta_operation();
			}
		}

		void _trust_anchor_handle_completed_requests(bool &progress)
		{
			while (true) {

				Cbe::Trust_anchor_request const typed_ta_req =
					_trust_anchor.peek_completed_request();

				if (!typed_ta_req.valid()) {
					break;
				}
				switch (_ta_request_get_module_type(typed_ta_req)) {
				case Module_type::CBE_INIT:

					_trust_anchor_handle_completed_requests_of_module(
						_cbe_init, typed_ta_req, progress);

					break;

				case Module_type::CBE:

					_trust_anchor_handle_completed_requests_of_module(
						*_cbe, typed_ta_req, progress);

					break;

				default:

					class Bad_module_type { };
					throw Bad_module_type();
				}
			}
		}

		void _execute_block_io(bool &progress)
		{
			while (_blk.tx()->ack_avail()) {

				Block::Packet_descriptor packet {
					_blk.tx()->try_get_acked_packet() };

				Cbe::Io_buffer::Index const data_index {
					_packet_io_buf_idx(packet) };

				if (packet.operation() == Block::Packet_descriptor::READ &&
					packet.succeeded())
				{
					_blk_buf.item(data_index) =
						*reinterpret_cast<Cbe::Block_data*>(
							_blk.tx()->packet_content(packet));
				}
				Module_type const type { _packet_module_type(packet) };
				switch (type) {
				case Module_type::CBE_INIT:

					_cbe_init.io_request_completed(data_index,
					                               packet.succeeded());
					break;

				case Module_type::CBE:

					_cbe->io_request_completed(data_index,
					                           packet.succeeded());
					break;

				case Module_type::CBE_DUMP:

					_cbe_dump.io_request_completed(data_index,
					                               packet.succeeded());
					break;

				case Module_type::CBE_CHECK:

					_cbe_check.io_request_completed(data_index,
					                                packet.succeeded());
					break;
				}
				_blk.tx()->release_packet(packet);
				progress = true;

				if (_config_node.verbose_blk_pkt_completed()) {
					log("blk pkt completed: ", blk_pkt_to_string(packet));
				}
			}
		}

		void _execute_trust_anchor(bool &progress)
		{
			progress |= _trust_anchor.execute();
			_trust_anchor_handle_completed_requests(progress);
		}

		void _crypto_handle_completed_encrypt_requests(bool &progress)
		{
			while (true) {

				Cbe::Request const request {
					_crypto.peek_completed_encryption_request() };

				if (!request.valid()) {
					break;
				}
				Crypto_cipher_buffer::Index const data_index { request.tag() };
				bool const success {
					_crypto.supply_cipher_data(
						request, _crypto_cipher_buf.item(data_index)) };

				if (!success) {
					break;
				}
				_cbe->supply_crypto_cipher_data(data_index, request.success());
				progress = true;

				if (_config_node.verbose_crypto_req_completed()) {
					log("crypto req completed: ", request);
				}
			}
		}

		void _crypto_handle_completed_decrypt_requests(bool &progress)
		{
			while (true) {

				Cbe::Request const request {
					_crypto.peek_completed_decryption_request() };

				if (!request.valid()) {
					break;
				}
				Crypto_plain_buffer::Index const data_index { request.tag() };
				bool const success {
					_crypto.supply_plain_data(
						request, _crypto_plain_buf.item(data_index)) };

				if (!success) {
					break;
				}
				_cbe->supply_crypto_plain_data(data_index, request.success());
				progress = true;

				if (_config_node.verbose_crypto_req_completed()) {
					log("crypto req completed: ", request);
				}
			}
		}

		void _execute_crypto(bool &progress)
		{
			progress |= _crypto.execute();
			_crypto_handle_completed_encrypt_requests(progress);
			_crypto_handle_completed_decrypt_requests(progress);
		}

		void _execute()
		{
			bool progress { true };
			while (progress) {

				progress = false;
				_execute_command_pool(progress);
				_execute_cbe_init(progress);
				_execute_block_io(progress);
				_execute_trust_anchor(progress);
				_execute_cbe_check(progress);
				_execute_cbe_dump(progress);
				_execute_crypto(progress);
				if (_cbe.constructed()) {
					_execute_cbe(progress);
				}
			}
			_blk.tx()->wakeup();
		}

	public:

		Main(Env &env)
		:
			_env { env }
		{
			_blk.tx_channel()->sigh_ack_avail(_blk_sigh);
			_blk.tx_channel()->sigh_ready_to_submit(_blk_sigh);
			_execute();
		}

		~Main()
		{
			_blk.tx_channel()->sigh_ack_avail(Signal_context_capability());
			_blk.tx_channel()->sigh_ready_to_submit(Signal_context_capability());
		}
};

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

	static Main main(env);
}

extern "C" int memcmp(const void *p0, const void *p1, Genode::size_t size)
{
	return Genode::memcmp(p0, p1, size);
}
