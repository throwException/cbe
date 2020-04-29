/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <vfs/dir_file_system.h>
#include <vfs/single_file_system.h>
#include <util/xml_generator.h>
#include <trace/timestamp.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/external_crypto.h>


namespace Vfs_cbe {
	using namespace Vfs;
	using namespace Genode;

	class Data_file_system;
	class Key_file_system;

	class Create_snapshot_file_system;
	class Discard_snapshot_file_system;

	struct Control_local_factory;
	class  Control_file_system;

	struct Snapshot_local_factory;
	class  Snapshot_file_system;

	struct Snapshots_local_factory;
	class  Snapshots_file_system;

	struct Local_factory;
	class  File_system;

	class Wrapper;
}


extern "C" void adainit();


extern "C" void print_u8(unsigned char const u) { Genode::log(u); }


extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	Genode::log(Genode::Cstring(s, len));
}


struct Snapshot_id_list
{
	Genode::uint32_t id[Cbe::NUM_SNAPSHOTS];
};


class Vfs_cbe::Wrapper
{
	private:

		Vfs::Env          &_env;
		Vfs_handle *_backend { nullptr };

		friend struct Backend_io_response_handler;

		struct Backend_io_response_handler : Vfs::Io_response_handler
		{
			Vfs_cbe::Wrapper &_wrapper;

			Backend_io_response_handler(Vfs_cbe::Wrapper &wrapper)
			: _wrapper(wrapper) { }

			void read_ready_response() override { }

			void io_progress_response() override
			{
				_wrapper._notify_backend_io_progress();
			}
		};

		Backend_io_response_handler _backend_io_response_handler { *this };

		void _notify_backend_io_progress()
		{
			if (_enqueued_vfs_handle) {
				_enqueued_vfs_handle->io_progress_response();
			} else {
				_io_progress_pending = true;
			}
		}

		Cbe::Io_buffer            _io_data { };
		Cbe::Crypto_cipher_buffer _cipher_data { };
		Cbe::Crypto_plain_buffer  _plain_data { };
		External::Crypto _crypto { };
		bool _key_set { false };

		Constructible<Cbe::Library> _cbe;

		Cbe::Superblocks_index _cur_sb       { 0 };
		bool                   _cur_sb_valid { false };
		Cbe::Superblocks       _super_blocks { };

		Cbe::Snapshot_ID _snapshot_id       { 0, false };
		Cbe::Token       _snapshot_token    { 0 };
		bool             _creating_snapshot { false };

		Cbe::Snapshot_ID _discard_snapshot_id    { 0, false };
		Cbe::Token       _discard_snapshot_token { 0 };
		bool             _discard_snapshot       { false };

		/* configuration options */
		bool _verbose       { true }; // XXX
		bool _show_progress { false };

		using Backend_device_path = Genode::String<32>;
		Backend_device_path _block_device { "/dev/block" };

		void _read_config(Xml_node config)
		{
			_verbose       = config.attribute_value("verbose", _verbose);
			_show_progress = config.attribute_value("show_progress", _show_progress);
			_block_device  = config.attribute_value("block", _block_device);

			using Passphrase = Genode::String<32+1>;
			Passphrase passphrase = config.attribute_value("passphrase", Passphrase());

			if (passphrase.valid()) {

				External::Crypto::Key_data key { }; // XXX clear key material
				Genode::memset(key.value, 0xa5, sizeof (key.value));
				Genode::memcpy(key.value, passphrase.string(), passphrase.length()-1);

				set_key(0, 0, key);
			}
		}

		Cbe::Superblocks_index _read_superblocks(Cbe::Superblocks &sbs)
		{
			_cur_sb_valid = false;
			Cbe::Generation        highest_gen = 0;
			Cbe::Superblocks_index most_recent_sb { 0 };
			bool                   most_recent_sb_valid { false };

			static_assert(sizeof (Cbe::Superblock) == Cbe::BLOCK_SIZE,
			              "Super-block size mistmatch");

			// XXX clear memory of all potential superblocks b/c the
			//     SPARK library will try to parse _all_ blocks
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				Cbe::Superblock &dst = sbs.block[i];
				Genode::memset((void*)&dst, 0, sizeof (dst));
				dst.last_secured_generation = Cbe::INVALID_GEN;
			}

			// XXX above clearing does not work, better only pass in
			//     the current superblock

			/*
			 * Read all super block slots and use the most recent one.
			 */
			for (uint64_t i = 0; i < Cbe::NUM_SUPER_BLOCKS; i++) {
				file_size bytes = 0;
				_backend->seek(i * Cbe::BLOCK_SIZE);
				Cbe::Superblock &dst = sbs.block[i];

				if (!_backend->fs().queue_read(_backend, Cbe::BLOCK_SIZE)) {
					error("queue_read failed");
					return Cbe::Superblocks_index(0);
				}

				using Result = Vfs::File_io_service::Read_result;

				while (true) {

					Result const result = _backend->fs().complete_read(
						_backend, (char*)&dst, Cbe::BLOCK_SIZE, bytes);

					if (   result == Result::READ_QUEUED
						|| result == Result::READ_ERR_INTERRUPT
						|| result == Result::READ_ERR_AGAIN
						|| result == Result::READ_ERR_WOULD_BLOCK) {
						_env.env().ep().wait_and_dispatch_one_io_signal();
						continue;
					}

					if (result == Result::READ_OK) {
						break;
					}

					if (result == Result::READ_ERR_IO) {
						error("complete_read failed, bytes: ", bytes);
						return Cbe::Superblocks_index(0);
					}
				}

				if (bytes < Cbe::BLOCK_SIZE) {
					error("complete_read failed, bytes: ", bytes);
					return Cbe::Superblocks_index(0);
				}

				Genode::error("bytes: ", bytes, " SB[", i, "]: ", dst);

				if (dst.valid() &&
				    dst.snapshots[dst.curr_snap].gen >= highest_gen)
				{
					if (dst.snapshots[dst.curr_snap].gen == highest_gen &&
					    highest_gen > 0)
					{
						Genode::error("generation: ", highest_gen,
						              " not unique - cannot select proper superblock");
						most_recent_sb_valid = false;
						break;
					}
					most_recent_sb.value = i;
					most_recent_sb_valid = true;
					highest_gen = dst.snapshots[dst.curr_snap].gen;
				}
			}

			_cur_sb       = most_recent_sb;
			_cur_sb_valid = most_recent_sb_valid;

			return most_recent_sb;
		}

		struct Could_not_open_block_backend : Genode::Exception { };
		struct No_valid_superblock_found    : Genode::Exception { };

		void _initialize_cbe()
		{
			using Result = Vfs::Directory_service::Open_result;

			Result res = _env.root_dir().open(_block_device.string(),
			                                  Vfs::Directory_service::OPEN_MODE_RDWR,
			                                       (Vfs::Vfs_handle **)&_backend,
			                                       _env.alloc());
			if (res != Result::OPEN_OK) {
				error("cbe_fs: Could not open back end block device: '", _block_device, "'");
				throw Could_not_open_block_backend();
			}

			_cur_sb = _read_superblocks(_super_blocks);

			if (!_cur_sb_valid) {
				error("cbe_fs: No valid super block");
				throw No_valid_superblock_found();
			}

			Genode::log("Use superblock[", _cur_sb, "]: ",
			            _super_blocks.block[_cur_sb.value]);

			_backend->handler(&_backend_io_response_handler);
			_cbe.construct(_super_blocks, _cur_sb);
		}

		struct Backend_request
		{
			enum State {
				NONE,
				READ_PENDING, READ_IN_PROGRESS, READ_COMPLETE,
				WRITE_PENDING, WRITE_IN_PROGRESS, WRITE_COMPLETE,
				SYNC_PENDING, SYNC_IN_PROGRESS, SYNC_COMPLETE,
				ERROR,
			};

			static char const *state_to_string(State s)
			{
				switch (s) {
				case State::NONE:              return "NONE";
				case State::READ_PENDING:      return "READ_PENDING";
				case State::READ_IN_PROGRESS:  return "READ_IN_PROGRESS";
				case State::READ_COMPLETE:     return "READ_COMPLETE";
				case State::WRITE_PENDING:     return "WRITE_PENDING";
				case State::WRITE_IN_PROGRESS: return "WRITE_IN_PROGRESS";
				case State::WRITE_COMPLETE:    return "WRITE_COMPLETE";
				case State::SYNC_PENDING:      return "SYNC_PENDING";
				case State::SYNC_IN_PROGRESS:  return "SYNC_IN_PROGRESS";
				case State::SYNC_COMPLETE:     return "SYNC_COMPLETE";
				case State::ERROR:             return "ERROR";
				}
				return "<unknown>";
			}
			State           state       { NONE };
			Cbe::Block_data block_data  { };
			Cbe::Request    cbe_request { };
			file_size       count       { 0 };
			bool            success     { false };
		};

		Backend_request _backend_request { };

		bool _backend_read(Cbe::Request &request,
		                   Cbe::Io_buffer::Index const data_index)
		{
			bool progress = false;

			switch (_backend_request.state) {
			case Backend_request::State::NONE:

				_backend_request.count = request.count * Cbe::BLOCK_SIZE;
				if (_backend_request.count > sizeof (_backend_request.block_data)) {
					struct Buffer_too_small { };
					throw Buffer_too_small ();
				}

				request.tag = data_index.value;

				_backend_request.cbe_request = request;
				_backend_request.state       = Backend_request::State::READ_PENDING;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::READ_PENDING:

				_backend->seek(request.block_number * Cbe::BLOCK_SIZE);
				if (!_backend->fs().queue_read(_backend, _backend_request.count)) {
					return progress;
				}

				_cbe->io_request_in_progress(data_index);
				_backend_request.state = Backend_request::State::READ_IN_PROGRESS;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::READ_IN_PROGRESS:
			{
				using Result = Vfs::File_io_service::Read_result;

				file_size out = 0;

				char * const data = reinterpret_cast<char* const>(&_backend_request.block_data);
				Result const result =
					_backend->fs().complete_read(_backend, data,
					                             _backend_request.count, out);
				if (   result == Result::READ_QUEUED
				    || result == Result::READ_ERR_INTERRUPT
				    || result == Result::READ_ERR_AGAIN
				    || result == Result::READ_ERR_WOULD_BLOCK) {
					return progress;
				}

				if (result == Result::READ_OK) {
					_io_data.item(data_index) = _backend_request.block_data;
					_backend_request.success = true;
				}

				if (   result == Result::READ_ERR_IO
				    || result == Result::READ_ERR_INVALID
				    /* partial read not supported */
				    || out != _backend_request.count) {
					_backend_request.success = false;
				}
				_backend_request.state = Backend_request::State::READ_COMPLETE;
				progress = true;
			}
			[[fallthrough]]
			case Backend_request::State::READ_COMPLETE:

				_cbe->io_request_completed(data_index, _backend_request.success);
				_backend_request.cbe_request = Cbe::Request { };
				_backend_request.state       = Backend_request::State::NONE;
				progress = true;
			default: break;
			}

			return progress;
		}

		bool _backend_write(Cbe::Request &request,
		                    Cbe::Io_buffer::Index const data_index)
		{
			bool progress = false;

			switch (_backend_request.state) {
			case Backend_request::State::NONE:

				_backend_request.count = request.count * Cbe::BLOCK_SIZE;
				if (_backend_request.count > sizeof (_backend_request.block_data)) {
					struct Buffer_too_small { };
					throw Buffer_too_small ();
				}

				request.tag = data_index.value;

				_backend_request.cbe_request = request;
				_backend_request.state       = Backend_request::State::WRITE_PENDING;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::WRITE_PENDING:

				_backend->seek(request.block_number * Cbe::BLOCK_SIZE);

				_backend_request.block_data = _io_data.item(data_index);
				_cbe->io_request_in_progress(data_index);
				_backend_request.state = Backend_request::State::WRITE_IN_PROGRESS;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::WRITE_IN_PROGRESS:
			{
				using Result = Vfs::File_io_service::Write_result;

				file_size out = 0;

				Result result = Result::WRITE_ERR_INVALID;
				try {
					char const * const data =
						reinterpret_cast<char const * const>(&_backend_request.block_data);
					result = _backend->fs().write(_backend, data,
					                              _backend_request.count, out);
				} catch (Vfs::File_io_service::Insufficient_buffer) {
					return progress;
				}
				if (   result == Result::WRITE_ERR_AGAIN
				    || result == Result::WRITE_ERR_INTERRUPT
				    || result == Result::WRITE_ERR_WOULD_BLOCK) {
					return progress;
				}
				if (result == Result::WRITE_OK) {
				    /* partial write not supported */
					_backend_request.success =
						out == _backend_request.count;
				}

				if (   result == Result::WRITE_ERR_IO
				    || result == Result::WRITE_ERR_INVALID
				    /* partial write not supported */
				    || out != _backend_request.count) {
					_backend_request.success = false;
				}
				_backend_request.state = Backend_request::State::WRITE_COMPLETE;
				progress = true;
			}
			[[fallthrough]]
			case Backend_request::State::WRITE_COMPLETE:

				_cbe->io_request_completed(data_index, _backend_request.success);
				_backend_request.cbe_request = Cbe::Request { };
				_backend_request.state       = Backend_request::State::NONE;
				progress = true;
			default: break;
			}

			return progress;
		}

		bool _backend_sync(Cbe::Request &request,
		                   Cbe::Io_buffer::Index const data_index)
		{
			bool progress = false;

			switch (_backend_request.state) {
			case Backend_request::State::NONE:

				request.tag = data_index.value;

				_backend_request.cbe_request = request;
				_backend_request.state       = Backend_request::State::SYNC_PENDING;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::SYNC_PENDING:

				if (!_backend->fs().queue_sync(_backend)) {
					return progress;
				}
				_cbe->io_request_in_progress(data_index);
				_backend_request.state = Backend_request::State::SYNC_IN_PROGRESS;
				progress = true;
			[[fallthrough]]
			case Backend_request::State::SYNC_IN_PROGRESS:
			{
				using Result = Vfs::File_io_service::Sync_result;
				Result const result = _backend->fs().complete_sync(_backend);

				if (result == Result::SYNC_QUEUED) {
					return progress;
				}

				if (result == Result::SYNC_ERR_INVALID) {
					_backend_request.success = false;
				}

				if (result == Result::SYNC_OK) {
					_backend_request.success = true;
				}

				_backend_request.state = Backend_request::State::SYNC_COMPLETE;
				progress = true;
			}
			[[fallthrough]]
			case Backend_request::State::SYNC_COMPLETE:

				_cbe->io_request_completed(data_index, _backend_request.success);
				_backend_request.cbe_request = Cbe::Request { };
				_backend_request.state       = Backend_request::State::NONE;
				progress = true;
			default: break;
			}

			return progress;
		}

	public:

		Wrapper(Vfs::Env &env, Xml_node config) : _env(env)
		{
			_read_config(config);
			_initialize_cbe();
		}

		External::Crypto &crypto()
		{
			return _crypto;
		}

		Cbe::Library &cbe()
		{
			if (!_cbe.constructed()) {
				struct Cbe_Not_Initialized { };
				throw Cbe_Not_Initialized();
			}

			return *_cbe;
		}

		struct Invalid_Request : Genode::Exception { };

		struct Helper_request
		{
			enum { BLOCK_SIZE = 512, };
			enum State { NONE, PENDING, IN_PROGRESS, COMPLETE, ERROR };

			State state { NONE };

			Cbe::Block_data block_data  { };
			Cbe::Request    cbe_request { };

			bool pending()     const { return state == PENDING; }
			bool in_progress() const { return state == IN_PROGRESS; }
			bool complete()    const { return state == COMPLETE; }
		};

		Helper_request _helper_read_request  { };
		Helper_request _helper_write_request { };

		struct Frontend_request
		{
			enum State {
				NONE,
				PENDING, IN_PROGRESS, COMPLETE,
				ERROR, ERROR_EOF
			};
			State        state       { NONE };
			file_size    count       { 0 };
			Cbe::Request cbe_request { };
			uint32_t     snap_id     { 0 };

			uint64_t helper_offset { 0 };

			bool pending()     const { return state == PENDING; }
			bool in_progress() const { return state == IN_PROGRESS; }
			bool complete()    const { return state == COMPLETE; }

			static char const *state_to_string(State s)
			{
				switch (s) {
				case State::NONE:         return "NONE";
				case State::PENDING:      return "PENDING";
				case State::IN_PROGRESS:  return "IN_PROGRESS";
				case State::COMPLETE:     return "COMPLETE";
				case State::ERROR:        return "ERROR";
				case State::ERROR_EOF:    return "ERROR_EOF";
				}
				return "<unknown>";
			}
		};

		Frontend_request _frontend_request { };

		Frontend_request const & frontend_request() const
		{
			return _frontend_request;
		}

		// XXX needs to be a list when snapshots are used
		Vfs_handle *_enqueued_vfs_handle { nullptr };
		bool        _io_progress_pending { false };

		void enqueue_handle(Vfs_handle &handle)
		{
			_enqueued_vfs_handle = &handle;
			if (_io_progress_pending) {
				_enqueued_vfs_handle->io_progress_response();
				_io_progress_pending = false;
			}
		}

		void ack_frontend_request(Vfs_handle &handle)
		{
			// assert current state was *_COMPLETE
			_frontend_request.state = Frontend_request::State::NONE;
			_enqueued_vfs_handle = nullptr;
		}

		bool submit_frontend_request(Vfs_handle              &handle,
		                             char                    *data,
		                             file_size                count,
		                             Cbe::Request::Operation  op,
		                             uint32_t                 snap_id)
		{
			if (_frontend_request.state != Frontend_request::State::NONE) {
				return false;
			}

			/* short-cut for SYNC requests */
			if (op == Cbe::Request::Operation::SYNC) {
				_frontend_request.cbe_request = Cbe::Request {
					.operation    = op,
					.success      = Cbe::Request::Success::FALSE,
					.block_number = 0,
					.offset       = 0,
					.count        = 1,
				};
				_frontend_request.count   = 0;
				_frontend_request.snap_id = 0;
				_frontend_request.state   = Frontend_request::State::PENDING;
				if (_verbose) {
					Genode::log("Req: (front req: ",
					            _frontend_request.cbe_request, ")");
				}
				return true;
			}

			file_size const offset = handle.seek();
			bool unaligned_request = false;

			/* unaligned request if any condition is true */
			unaligned_request |= (offset % Cbe::BLOCK_SIZE) != 0;
			unaligned_request |= (count < Cbe::BLOCK_SIZE);
			unaligned_request |= (count % Cbe::BLOCK_SIZE) != 0;

			if (unaligned_request) {
				_helper_read_request.cbe_request = Cbe::Request {
					.operation    = Cbe::Request::Operation::READ,
					.success      = Cbe::Request::Success::FALSE,
					.block_number = offset / Cbe::BLOCK_SIZE,
					.offset       = (uint64_t)&_helper_read_request.block_data,
					.count        = 1,
				};
				_helper_read_request.state = Helper_request::State::PENDING;

				_frontend_request.helper_offset = (offset % Cbe::BLOCK_SIZE);
				if (count >= (Cbe::BLOCK_SIZE - _frontend_request.helper_offset)) {
					_frontend_request.count = Cbe::BLOCK_SIZE - _frontend_request.helper_offset;
				} else {
					_frontend_request.count = count;
				}

				/* skip handling by the CBE, helper requests will do that for us */
				_frontend_request.state = Frontend_request::State::IN_PROGRESS;

			} else {
				_frontend_request.count = count;
				_frontend_request.state = Frontend_request::State::PENDING;
			}

			_frontend_request.cbe_request = Cbe::Request {
				.operation    = op,
				.success      = Cbe::Request::Success::FALSE,
				.block_number = offset / Cbe::BLOCK_SIZE,
				.offset       = (uint64_t)data,
				.count        = (uint32_t)(count / Cbe::BLOCK_SIZE),
			};

			if (_verbose) {
				if (unaligned_request) {
					Genode::log("Unaligned req: ",
					            "off: ", offset, " bytes: ", count,
					            " (front req: ", _frontend_request.cbe_request,
					            " (helper req: ", _helper_read_request.cbe_request,
					            " off: ", _frontend_request.helper_offset,
					            " count: ", _frontend_request.count, ")");
				} else {
					Genode::log("Req: ",
					            "off: ", offset, " count: ", count,
					            " (front req: ", _frontend_request.cbe_request, ")");
				}
			}

			_frontend_request.snap_id = snap_id;
			return true;
		}

		bool _handle_cbe_backend(Cbe::Library &cbe, Backend_request &req)
		{
			using ST = Backend_request::State;

			bool progress = false;

			Cbe::Io_buffer::Index data_index { 0 };
			Cbe::Request cbe_request = _cbe->has_io_request(data_index);
			if (cbe_request.valid() && req.state == ST::NONE) {

				if (cbe_request.read()) {
					progress |= _backend_read(cbe_request, data_index);
				} else if (cbe_request.write()) {
					progress |= _backend_write(cbe_request, data_index);
				} else if (cbe_request.sync()) {
					progress |= _backend_sync(cbe_request, data_index);
				} else {
					Genode::error("invalid cbe_request");
					throw -1;
				}
			}

			if (req.state != ST::NONE) {
				Cbe::Io_buffer::Index const data_index {
					req.cbe_request.tag };

				if (req.cbe_request.valid()) {
					if (req.cbe_request.read()) {
						progress |= _backend_read(req.cbe_request, data_index);
					} else if (req.cbe_request.write()) {
						progress |= _backend_write(req.cbe_request, data_index);
					} else if (req.cbe_request.sync()) {
						progress |= _backend_sync(req.cbe_request, data_index);
					} else {
						Genode::error("invalid req.cbe_request");
						throw -1;
					}
				} else {
					Genode::warning("req.cbe_request no longer valid");
				}
			}

			return progress;
		}

		bool _handle_cbe_frontend(Cbe::Library &cbe, Frontend_request &frontend_request)
		{
			if (_helper_read_request.pending()) {
				if (_cbe->client_request_acceptable()) {
					_cbe->submit_client_request(_helper_read_request.cbe_request,
					                            frontend_request.snap_id);
					_helper_read_request.state = Helper_request::State::IN_PROGRESS;
				}
			}

			if (_helper_write_request.pending()) {
				if (_cbe->client_request_acceptable()) {
					_cbe->submit_client_request(_helper_write_request.cbe_request,
					                            frontend_request.snap_id);
					_helper_write_request.state = Helper_request::State::IN_PROGRESS;
				}
			}

			if (frontend_request.pending()) {

				using ST = Frontend_request::State;

				Cbe::Request const &request = frontend_request.cbe_request;
				Cbe::Virtual_block_address const vba = request.block_number;
				uint32_t const snap_id = frontend_request.snap_id;

				if (vba > cbe.max_vba()) {
					warning("reject request with out-of-range virtual block start address ", vba);
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (vba + request.count < vba) {
					warning("reject wraping request", vba);
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (vba + request.count > (cbe.max_vba() + 1)) {
					warning("reject invalid request ", vba, " ", request.count);
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (cbe.client_request_acceptable()) {
					cbe.submit_client_request(request, snap_id);
					frontend_request.state = ST::IN_PROGRESS;
				}
			}

			cbe.execute(_io_data, _plain_data, _cipher_data, 0);
			bool progress = cbe.execute_progress();

			using ST = Frontend_request::State;

			while (true) {
				Cbe::Request const cbe_request = cbe.peek_completed_client_request();
				if (!cbe_request.valid()) { break; }

				cbe.drop_completed_client_request(cbe_request);

				if (_helper_read_request.in_progress()) {
					_helper_read_request.state = Helper_request::State::COMPLETE;
				} else if (_helper_write_request.in_progress()) {
					_helper_write_request.state = Helper_request::State::COMPLETE;
				} else {
					frontend_request.state = ST::COMPLETE;
					if (_verbose) {
						Genode::log("Complete request: ",
						            " (frontend request: ", _frontend_request.cbe_request,
						            " count: ", _frontend_request.count, ")");
					}
				}
				progress = true;
			}

			if (_helper_read_request.complete()) {
				if (frontend_request.cbe_request.read()) {
					char       * dst = reinterpret_cast<char*>
						(frontend_request.cbe_request.offset);
					char const * src = reinterpret_cast<char const*>
						(&_helper_read_request.block_data) + frontend_request.helper_offset;

					Genode::memcpy(dst, src, _frontend_request.count);

					_helper_read_request.state = Helper_request::State::NONE;
					frontend_request.state = ST::COMPLETE;

					if (_verbose) {
						Genode::log("Complete invalid READ request: ",
						            " (frontend request: ", _frontend_request.cbe_request,
						            " (helper request: ", _helper_read_request.cbe_request,
						            " offset: ", _frontend_request.helper_offset,
						            " count: ", _frontend_request.count, ")");
					}
				}

				if (frontend_request.cbe_request.write()) {
					/* copy whole block first */
					{
						char       * dst = reinterpret_cast<char*>
							(&_helper_write_request.block_data);
						char const * src = reinterpret_cast<char const*>
							(&_helper_read_request.block_data);
						Genode::memcpy(dst, src, sizeof (Cbe::Block_data));
					}

					/* and than actual request data */
					{
						char       * dst = reinterpret_cast<char*>
							(&_helper_write_request.block_data) + frontend_request.helper_offset;
						char const * src = reinterpret_cast<char const*>
							(frontend_request.cbe_request.offset);
						Genode::memcpy(dst, src, _frontend_request.count);
					}

					/* re-use request */
					_helper_write_request.cbe_request = _helper_read_request.cbe_request;
					_helper_write_request.cbe_request.operation = Cbe::Request::Operation::WRITE;
					_helper_write_request.cbe_request.success   = Cbe::Request::Success::FALSE;

					_helper_write_request.state = Helper_request::State::PENDING;
					_helper_read_request.state  = Helper_request::State::NONE;
				}
				progress = true;
			}

			if (_helper_write_request.complete()) {
				if (_verbose) {
					Genode::log("Complete invalid WRITE request: ",
					            " (frontend request: ", _frontend_request.cbe_request,
					            " (helper request: ", _helper_read_request.cbe_request,
					            " offset: ", _frontend_request.helper_offset,
					            " count: ", _frontend_request.count, ")");
				}

				_helper_write_request.state = Helper_request::State::NONE;
				frontend_request.state = ST::COMPLETE;
				progress = true;
			}

			/* read */
			Cbe::Request cbe_request = cbe.client_data_ready();
			if (cbe_request.valid() && cbe_request.read()) {

				uint64_t const prim_index = cbe.client_data_index(cbe_request);
				if (prim_index == ~0ull) {
					Genode::error("prim_index invalid: ", cbe_request);

					frontend_request.state = ST::ERROR;
					return progress;
				}

				Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(
					cbe_request.offset + (prim_index * Cbe::BLOCK_SIZE));

				Cbe::Crypto_plain_buffer::Index data_index(~0);
				bool const data_index_valid =
					cbe.obtain_client_data(cbe_request, data_index);

				if (data_index_valid) {
					Genode::memcpy(&data, &_plain_data.item(data_index), sizeof (Cbe::Block_data));

					progress = true;
				}
			}

			/* write */
			cbe_request = cbe.client_data_required();
			if (cbe_request.valid() && cbe_request.write()) {
				uint64_t const prim_index = cbe.client_data_index(cbe_request);
				if (prim_index == ~0ull) {
					Genode::error("prim_index invalid: ", cbe_request);
					frontend_request.state = ST::ERROR;
					return progress;
				}

				Cbe::Block_data &data = *reinterpret_cast<Cbe::Block_data*>(
					cbe_request.offset + (prim_index * Cbe::BLOCK_SIZE));

				progress = cbe.supply_client_data(0, cbe_request, data);
				progress |= true; /// XXX why?
			}

			return progress;
		}

		bool _handle_crypto(Cbe::Library              &cbe,
		                    External::Crypto          &cry,
		                    Cbe::Crypto_cipher_buffer &cipher,
		                    Cbe::Crypto_plain_buffer  &plain)
		{
			bool progress = cry.execute();

			/* encrypt */
			while (true) {
				Cbe::Crypto_plain_buffer::Index data_index { 0 };
				Cbe::Request request = cbe.crypto_cipher_data_required(data_index);
				if (!request.valid() || !cry.encryption_request_acceptable()) { break; }

				request.tag = data_index.value;
				cry.submit_encryption_request(request, plain.item(data_index), 0);
				cbe.crypto_cipher_data_requested(data_index);
				progress |= true;
			}

			while (true) {
				Cbe::Request const request = cry.peek_completed_encryption_request();
				if (!request.valid()) {
					break;
				}
				Cbe::Crypto_cipher_buffer::Index const data_index(request.tag);
				if (!cry.supply_cipher_data(request, cipher.item(data_index))) {
					break;
				}
				bool const success = request.success == Cbe::Request::Success::TRUE;
				cbe.supply_crypto_cipher_data(data_index, success);
				progress |= true;
			}

			/* decrypt */
			while (true) {
				Cbe::Crypto_cipher_buffer::Index data_index { 0 };
				Cbe::Request request = cbe.crypto_plain_data_required(data_index);
				if (!request.valid() || !cry.decryption_request_acceptable()) { break; }

				request.tag = data_index.value;
				cry.submit_decryption_request(request, cipher.item(data_index), 0);
				cbe.crypto_plain_data_requested(data_index);
				progress |= true;
			}

			while (true) {
				Cbe::Request const request = cry.peek_completed_decryption_request();
				if (!request.valid()) { break; }

				Cbe::Crypto_plain_buffer::Index const data_index(request.tag);
				if (!cry.supply_plain_data(request, plain.item(data_index))) {
					break;
				}
				bool const success = request.success == Cbe::Request::Success::TRUE;
				cbe.supply_crypto_plain_data(data_index, success);
				progress |= true;
			}

			return progress;
		}

		void handle_frontend_request()
		{
			while (true) {

				bool progress = false;

				bool const frontend_progress =
					_handle_cbe_frontend(*_cbe, _frontend_request);
				progress |= frontend_progress;

				bool const backend_progress =
					_handle_cbe_backend(*_cbe, _backend_request);
				progress |= backend_progress;

				bool const crypto_progress =
					_handle_crypto(*_cbe, _crypto, _cipher_data, _plain_data);
				progress |= crypto_progress;

				if (_creating_snapshot &&
				    _cbe->snapshot_creation_complete(_snapshot_token, _snapshot_id)) {
					_creating_snapshot = false;
				}

				if (_discard_snapshot) {
					Cbe::Token token { 0 };

					if (_cbe->discard_snapshot_complete(token)
					    && token.value == _snapshot_token.value) {
						_discard_snapshot = false;
					}
				}

				if (!progress) { break; }
			}
		}

		bool client_request_acceptable() const
		{
			return _key_set ? _cbe->client_request_acceptable()
		                    : false;
		}

		void set_key(unsigned slot, unsigned id,
		             External::Crypto::Key_data const &key)
		{
			_crypto.set_key(slot, id, key);
			_key_set |= true;
		}

		void active_snapshot_ids(Cbe::Active_snapshot_ids &ids)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}
			_cbe->active_snapshot_ids(ids);
			handle_frontend_request();
		}

		void create_snapshot(bool quaratine)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}
			if (_creating_snapshot) { return; }

			++_snapshot_token.value;
			_creating_snapshot = _cbe->create_snapshot(_snapshot_token, quaratine);
			if (_creating_snapshot) {
				handle_frontend_request();
			}
		}

		bool discard_snapshot(Cbe::Snapshot_ID id)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}

			if (_discard_snapshot) { return false; }

			++_discard_snapshot_token.value;
			_discard_snapshot  = _cbe->discard_snapshot(_discard_snapshot_token, id);
			if (_discard_snapshot) {
				handle_frontend_request();
			}
			return true;
		}

		Genode::Lock _frontend_lock { };

		Genode::Lock &frontend_lock() { return _frontend_lock; }
};


class Vfs_cbe::Data_file_system : public Single_file_system
{
	private:

		Wrapper &_w;
		uint32_t _snap_id;

	public:

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;
			uint32_t _snap_id { 0 };

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper       &w,
			           uint32_t snap_id)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w), _snap_id(snap_id)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				Genode::Lock_guard guard { _w.frontend_lock() };

				using State = Wrapper::Frontend_request::State;

				State state = _w.frontend_request().state;
				if (state == State::NONE) {

					if (!_w.client_request_acceptable()) {
						return READ_QUEUED;
					}
					using Op = Cbe::Request::Operation;

					bool const accepted =
						_w.submit_frontend_request(*this, dst, count,
						                           Op::READ, _snap_id);
					if (!accepted) { return READ_ERR_IO; }
				}

				_w.handle_frontend_request();
				state = _w.frontend_request().state;

				if (   state == State::PENDING
				    || state == State::IN_PROGRESS) {
					_w.enqueue_handle(*this);
					return READ_QUEUED;
				}

				if (state == State::COMPLETE) {
					out_count = _w.frontend_request().count;
					_w.ack_frontend_request(*this);
					return READ_OK;
				}

				if (state == State::ERROR_EOF) {
					out_count = 0;
					_w.ack_frontend_request(*this);
					return READ_OK;
				}

				if (state == State::ERROR) {
					out_count = 0;
					_w.ack_frontend_request(*this);
					return READ_ERR_IO;
				}

				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count,
			                   file_size &out_count) override
			{
				Genode::Lock_guard guard { _w.frontend_lock() };

				using State = Wrapper::Frontend_request::State;

				State state = _w.frontend_request().state;
				if (state == State::NONE) {

					if (!_w.client_request_acceptable()) {
						throw Insufficient_buffer();
					}
					using Op = Cbe::Request::Operation;

					bool const accepted =
						_w.submit_frontend_request(*this, const_cast<char*>(src),
						                           count, Op::WRITE, _snap_id);
					if (!accepted) { return WRITE_ERR_IO; }
				}

				_w.handle_frontend_request();
				state = _w.frontend_request().state;

				if (   state == State::PENDING
				    || state == State::IN_PROGRESS) {
					_w.enqueue_handle(*this);
					throw Insufficient_buffer();
				}

				if (state == State::COMPLETE) {
					out_count = _w.frontend_request().count;
					_w.ack_frontend_request(*this);
					return WRITE_OK;
				}

				if (state == State::ERROR_EOF) {
					out_count = 0;
					_w.ack_frontend_request(*this);
					return WRITE_OK;
				}

				if (state == State::ERROR) {
					out_count = 0;
					_w.ack_frontend_request(*this);
					return WRITE_ERR_IO;
				}

				return WRITE_ERR_IO;
			}

			Sync_result sync() override
			{
				Genode::Lock_guard guard { _w.frontend_lock() };

				using State = Wrapper::Frontend_request::State;

				State state = _w.frontend_request().state;
				if (state == State::NONE) {

					if (!_w.client_request_acceptable()) {
						return SYNC_QUEUED;
					}
					using Op = Cbe::Request::Operation;

					bool const accepted =
						_w.submit_frontend_request(*this, nullptr, 0, Op::SYNC, 0);
					if (!accepted) { return SYNC_ERR_INVALID; }
				}

				_w.handle_frontend_request();
				state = _w.frontend_request().state;

				if (   state == State::PENDING
				    || state == State::IN_PROGRESS) {
					_w.enqueue_handle(*this);
					return SYNC_QUEUED;
				}

				if (state == State::COMPLETE) {
					_w.ack_frontend_request(*this);
					return SYNC_OK;
				}

				if (state == State::ERROR) {
					_w.ack_frontend_request(*this);
					return SYNC_ERR_INVALID;
				}

				return SYNC_ERR_INVALID;
			}

			bool read_ready() override { return true; }
		};

		Data_file_system(Wrapper &w, uint32_t snap_id)
		:
			Single_file_system(Node_type::CONTINUOUS_FILE, type_name(),
			                   Node_rwx::rw(), Xml_node("<data/>")),
			_w(w), _snap_id(snap_id)
		{ }

		~Data_file_system() { /* XXX sync on close */ }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Stat_result stat(char const *path, Stat &out) override
		{
			try {
				(void)_w.cbe();
			} catch (...) {
				return STAT_ERR_NO_ENTRY;
			}

			Stat_result result = Single_file_system::stat(path, out);

			/* setting a size is not strictly correct, but ... */
			out.size = 0;
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}


		/***************************
		 ** File-system interface **
		 ***************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				(void)_w.cbe();
			} catch (...) {
				return OPEN_ERR_UNACCESSIBLE;
			}

			*out_handle =
				new (alloc) Vfs_handle(*this, *this, alloc, _w, _snap_id);

			return OPEN_OK;
		}

		static char const *type_name() { return "data"; }
		char const *type() override { return type_name(); }
};


class Vfs_cbe::Key_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		static constexpr Genode::size_t MAX_KEY_SIZE = sizeof (External::Crypto::Key_data);

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper           &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0), _w(w)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				out_count = 0;

				if (seek() > (file_size)MAX_KEY_SIZE || count > (file_size)MAX_KEY_SIZE)
					return WRITE_ERR_INVALID;

				External::Crypto::Key_data key { }; // XXX clear key material
				Genode::memset(key.value, 0xa5, sizeof (key.value));
				Genode::memcpy(key.value, src, count);

				_w.set_key(0, 0, key);

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Key_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::rw(), Xml_node("<key/>")),
			_w(w)
		{ }

		static char const *type_name() { return "key"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle = new (alloc)
					Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			out.size = sizeof (External::Crypto::Key_data);
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


class Vfs_cbe::Create_snapshot_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				bool create_snapshot { false };
				Genode::ascii_to(src, create_snapshot);
				Genode::String<64> str(Genode::Cstring(src, count));

				if (!create_snapshot) {
					return WRITE_ERR_IO;
				}
				out_count = count;
				_w.create_snapshot(true);
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Create_snapshot_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<create_snapshot/>")),
			_w(w)
		{ }

		static char const *type_name() { return "create_snapshot"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle =
					new (alloc) Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			return result;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


class Vfs_cbe::Discard_snapshot_file_system : public Vfs::Single_file_system
{
	private:

		Wrapper &_w;

		struct Vfs_handle : Single_vfs_handle
		{
			Wrapper &_w;

			Vfs_handle(Directory_service &ds,
			           File_io_service   &fs,
			           Genode::Allocator &alloc,
			           Wrapper &w)
			:
				Single_vfs_handle(ds, fs, alloc, 0),
				_w(w)
			{ }

			Read_result read(char *, file_size, file_size &) override
			{
				return READ_ERR_IO;
			}

			Write_result write(char const *src, file_size count,
			                   file_size &out_count) override
			{
				out_count = 0;

				Genode::uint64_t id { 0 };
				Genode::ascii_to(src, id);
				if (id == 0) {
					return WRITE_ERR_IO;
				}
				Cbe::Snapshot_ID snap { .value = id, .valid = true };
				if (!_w.discard_snapshot(snap)) {
					return WRITE_ERR_IO;
				}

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Discard_snapshot_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<discard_snapshot/>")),
			_w(w)
		{ }

		static char const *type_name() { return "discard_snapshot"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory-service interface **
		 *********************************/

		Open_result open(char const  *path, unsigned,
		                 Vfs::Vfs_handle **out_handle,
		                 Genode::Allocator   &alloc) override
		{
			if (!_single_file(path))
				return OPEN_ERR_UNACCESSIBLE;

			try {
				*out_handle =
					new (alloc) Vfs_handle(*this, *this, alloc, _w);
				return OPEN_OK;
			}
			catch (Genode::Out_of_ram)  { return OPEN_ERR_OUT_OF_RAM; }
			catch (Genode::Out_of_caps) { return OPEN_ERR_OUT_OF_CAPS; }
		}

		Stat_result stat(char const *path, Stat &out) override
		{
			Stat_result result = Single_file_system::stat(path, out);
			return result;
		}

		/********************************
		 ** File I/O service interface **
		 ********************************/

		Ftruncate_result ftruncate(Vfs::Vfs_handle *handle, file_size) override
		{
			return FTRUNCATE_OK;
		}
};


struct Vfs_cbe::Snapshot_local_factory : File_system_factory
{
	Data_file_system _block_fs;

	Snapshot_local_factory(Vfs::Env    &env,
	                       Wrapper &cbe,
	                       uint32_t snap_id)
	: _block_fs(cbe, snap_id) { }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		if (node.has_type(Data_file_system::type_name()))
			return &_block_fs;

		return nullptr;
	}
};


class Vfs_cbe::Snapshot_file_system : private Snapshot_local_factory,
                                      public Vfs::Dir_file_system
{
	private:

		Genode::uint32_t _snap_id;

		typedef String<128> Config;

		static Config _config(Genode::uint32_t snap_id, bool readonly)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {

				xml.attribute("name",
				              !readonly ? String<16>("current")
				                        : String<16>(snap_id));

				xml.node("data", [&] () {
					xml.attribute("readonly", readonly);
				});
			});

			return Config(Cstring(buf));
		}

	public:

		Snapshot_file_system(Vfs::Env        &vfs_env,
		                    Wrapper          &cbe,
		                    Genode::uint32_t  snap_id,
		                    bool              readonly = false)
		:
			Snapshot_local_factory(vfs_env, cbe, snap_id),
			Vfs::Dir_file_system(vfs_env,
			                     Xml_node(_config(snap_id, readonly).string()),
			                     *this),
			_snap_id(snap_id)
		{ }

		static char const *type_name() { return "snapshot"; }

		char const *type() override { return type_name(); }

		Genode::uint32_t snapshot_id() const
		{
			return _snap_id;
		}
};


class Vfs_cbe::Snapshots_file_system : public Vfs::File_system
{
	private:

		Vfs::Env &_vfs_env;

		bool _root_dir(char const *path) { return strcmp(path, "/snapshots") == 0; }
		bool _top_dir(char const *path) { return strcmp(path, "/") == 0; }

		struct Snapshot_registry
		{
			Genode::Allocator &_alloc;
			Wrapper &_w;

			uint32_t _number_of_snapshots { 0 };

			Genode::Registry<Genode::Registered<Snapshot_file_system>> _snap_fs { };

			Snapshot_registry(Genode::Allocator &alloc, Wrapper &w)
			: _alloc(alloc), _w(w)
			{ }

			void update(Vfs::Env &vfs_env)
			{
				Cbe::Active_snapshot_ids list { };
				_w.active_snapshot_ids(list);

				/* alloc new */
				for (size_t i = 0; i < sizeof (list.values) / sizeof (list.values[0]); i++) {
					uint32_t const id = list.values[i];
					if (!id) { continue; }

					bool is_old = false;
					auto find_old = [&] (Snapshot_file_system const &fs) {
						is_old |= (fs.snapshot_id() == id);
					};
					_snap_fs.for_each(find_old);

					if (!is_old) {
						new (_alloc) Genode::Registered<Snapshot_file_system>(
							_snap_fs, vfs_env, _w, id, true);
						++_number_of_snapshots;
					}
				}

				/* destroy old */
				auto find_stale = [&] (Snapshot_file_system const &fs) {
					bool is_stale = true;
					for (size_t i = 0; i < sizeof (list.values) / sizeof (list.values[0]); i++) {
						uint32_t const id = list.values[i];
						if (!id) { continue; }

						if (fs.snapshot_id() == id) {
							is_stale = false;
							break;
						}
					}

					if (is_stale) {
						destroy(&_alloc, &const_cast<Snapshot_file_system&>(fs));
						--_number_of_snapshots;
					}
				};
				_snap_fs.for_each(find_stale);
			}

			uint32_t number_of_snapshots() const { return _number_of_snapshots; }

			Snapshot_file_system const &by_index(uint32_t idx) const
			{
				uint32_t i = 0;
				Snapshot_file_system const *fsp { nullptr };
				auto lookup = [&] (Snapshot_file_system const &fs) {
					if (i == idx) {
						fsp = &fs;
					}
					++i;
				};
				_snap_fs.for_each(lookup);
				return *fsp;
			}

			Snapshot_file_system &by_id(uint32_t id)
			{
				Snapshot_file_system *fsp { nullptr };
				auto lookup = [&] (Snapshot_file_system &fs) {
					if (fs.snapshot_id() == id) {
						fsp = &fs;
					}
				};
				_snap_fs.for_each(lookup);
				return *fsp;
			}
		};

	public:

		struct Snap_vfs_handle : Vfs::Vfs_handle
		{
			using Vfs_handle::Vfs_handle;

			virtual Read_result read(char *dst, file_size count,
			                         file_size &out_count) = 0;

			virtual Write_result write(char const *src, file_size count,
			                           file_size &out_count) = 0;

			virtual Sync_result sync()
			{
				return SYNC_OK;
			}

			virtual bool read_ready() = 0;
		};


		struct Dir_vfs_handle : Snap_vfs_handle
		{
			Snapshot_registry const &_snap_reg;

			bool const _root_dir { false };

			Dir_vfs_handle(Directory_service &ds,
			               File_io_service   &fs,
			               Genode::Allocator &alloc,
			               int                status_flags,
			               Snapshot_registry const &snap_reg,
			               bool root_dir)
			:
				Snap_vfs_handle(ds, fs, alloc, status_flags),
				_snap_reg(snap_reg), _root_dir(root_dir)
			{ }

			Read_result read(char *dst, file_size count,
			                 file_size &out_count) override
			{
				out_count = 0;

				if (count < sizeof(Dirent))
					return READ_ERR_INVALID;

				file_size index = seek() / sizeof(Dirent);

				Dirent &out = *(Dirent*)dst;

				if (!_root_dir) {
					if (index < _snap_reg.number_of_snapshots()) {
						Snapshot_file_system const &fs = _snap_reg.by_index(index);
						Genode::String<32> name { fs.snapshot_id() };

						out = {
							.fileno = (Genode::addr_t)this | index,
							.type   = Dirent_type::DIRECTORY,
							.rwx    = Node_rwx::rx(),
							.name   = { name.string() },
						};
					} else {
						out.type = Dirent_type::END;
					}
				} else {
					if (index == 0) {
						out = {
							.fileno = (Genode::addr_t)this,
							.type   = Dirent_type::DIRECTORY,
							.rwx    = Node_rwx::rx(),
							.name   = { "snapshots" }
						};
					} else {
						out.type = Dirent_type::END;
					}
				}

				out_count = sizeof(Dirent);
				return READ_OK;
			}

			Write_result write(char const *, file_size, file_size &) override
			{
				return WRITE_ERR_INVALID;
			}

			bool read_ready() override { return true; }

		};

		Snapshot_registry _snap_reg;

		char const *_sub_path(char const *path) const
		{
			/* skip heading slash in path if present */
			if (path[0] == '/') {
				path++;
			}

			Genode::size_t const name_len = strlen(type_name());
			if (strcmp(path, type_name(), name_len) != 0) {
				return nullptr;
			}

			path += name_len;

			/*
			 * The first characters of the first path element are equal to
			 * the current directory name. Let's check if the length of the
			 * first path element matches the name length.
			 */
			if (*path != 0 && *path != '/') {
				return 0;
			}

			return path;
		}


		Snapshots_file_system(Vfs::Env         &vfs_env,
		                      Genode::Xml_node  node,
		                      Wrapper          &w)
		: _vfs_env(vfs_env), _snap_reg(vfs_env.alloc(), w)
		{ }

		static char const *type_name() { return "snapshots"; }

		char const *type() override { return type_name(); }


		/*********************************
		 ** Directory service interface **
		 *********************************/

		Dataspace_capability dataspace(char const *path)
		{
			return Genode::Dataspace_capability();
		}

		void release(char const *path, Dataspace_capability)
		{
		}

		Open_result open(char const       *path,
		                 unsigned          mode,
		                 Vfs::Vfs_handle **out_handle,
		                 Allocator        &alloc) override
		{
			path = _sub_path(path);
			if (!path || path[0] != '/') {
				return OPEN_ERR_UNACCESSIBLE;
			}
			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			return fs.open(path, mode, out_handle, alloc);
		}

		Opendir_result opendir(char const       *path,
		                       bool              create,
		                       Vfs::Vfs_handle **out_handle,
		                       Allocator        &alloc) override
		{
			if (create) {
				return OPENDIR_ERR_PERMISSION_DENIED;
			}

			bool const root = strcmp(path, "/") == 0;
			if (_root_dir(path) || root) {

				_snap_reg.update(_vfs_env);

				*out_handle = new (alloc) Dir_vfs_handle(*this, *this, alloc, 0, _snap_reg, root);
				return OPENDIR_OK;
			}
			return OPENDIR_ERR_LOOKUP_FAILED;
		}

		void close(Vfs_handle *handle) override
		{
			if (handle && (&handle->ds() == this))
				destroy(handle->alloc(), handle);
		}

		Stat_result stat(char const *path, Stat &out_stat) override
		{
			out_stat = Stat { };
			path = _sub_path(path);

			/* path does not match directory name */
			if (!path) {
				return STAT_ERR_NO_ENTRY;
			}

			/*
			 * If path equals directory name, return information about the
			 * current directory.
			 */
			if (strlen(path) == 0 || _top_dir(path)) {

				out_stat.type   = Node_type::DIRECTORY;
				out_stat.inode  = 1;
				out_stat.device = (Genode::addr_t)this;
				return STAT_OK;
			}

			if (!path || path[0] != '/') {
				return STAT_ERR_NO_ENTRY;
			}

			/* strip / */
			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			return fs.stat(path, out_stat);

		}

		Unlink_result unlink(char const *path)
		{
			return UNLINK_ERR_NO_PERM;
		}

		Rename_result rename(char const *from, char const *to) override
		{
			return RENAME_ERR_NO_PERM;
		}

		file_size num_dirent(char const *path) override
		{
			return strcmp(path, "/") == 0 ? 1 : _snap_reg.number_of_snapshots();
		}

		bool directory(char const *path) override
		{
			if (Genode::strcmp(path, "/snapshots") == 0) {
				return true;
			}
			return false;
		}

		char const *leaf_path(char const *path) override
		{
			path = _sub_path(path);
			if (!path) { //|| path[0] != '/') {
				return nullptr;
			}

			if (strlen(path) == 0) { 
				return path;
			}

			path++;

			uint32_t id { 0 };
			Genode::ascii_to(path, id);
			Snapshot_file_system &fs = _snap_reg.by_id(id);
			char const *leaf_path = fs.leaf_path(path);
			if (leaf_path) {
				return leaf_path;
			}

			return nullptr;
		}


		/********************************
		 ** File I/O service interface **
		 ********************************/

		Write_result write(Vfs::Vfs_handle *vfs_handle,
		                   char const *buf, file_size buf_size,
		                   file_size &out_count) override
		{
			return WRITE_ERR_IO;
		}

		bool queue_read(Vfs::Vfs_handle *, file_size) override
		{
			return true;
		}

		Read_result complete_read(Vfs::Vfs_handle *vfs_handle,
		                          char *dst, file_size count,
		                          file_size & out_count) override
		{
			Snap_vfs_handle *handle =
				static_cast<Snap_vfs_handle*>(vfs_handle);

			if (handle)
				return handle->read(dst, count, out_count);

			return READ_ERR_IO;
		}

		bool read_ready(Vfs::Vfs_handle *) override
		{
			return true;
		}

		Ftruncate_result ftruncate(Vfs::Vfs_handle *vfs_handle,
		                           file_size len) override
		{
			return FTRUNCATE_OK;
		}
};


struct Vfs_cbe::Control_local_factory : File_system_factory
{
	Key_file_system               _key_fs;
	Create_snapshot_file_system   _create_snapshot_fs;
	Discard_snapshot_file_system  _discard_snapshot_fs;

	Control_local_factory(Vfs::Env     &env,
	                      Xml_node      config,
	                      Wrapper      &cbe)
	:
		_key_fs(cbe),
		_create_snapshot_fs(cbe),
		_discard_snapshot_fs(cbe)
	{ }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		if (node.has_type(Key_file_system::type_name())) {
			return &_key_fs;
		}

		if (node.has_type(Create_snapshot_file_system::type_name())) {
			return &_create_snapshot_fs;
		}

		if (node.has_type(Discard_snapshot_file_system::type_name())) {
			return &_discard_snapshot_fs;
		}

		return nullptr;
	}
};


class Vfs_cbe::Control_file_system : private Control_local_factory,
                                     public Vfs::Dir_file_system
{
	private:

		typedef String<128> Config;

		static Config _config(Xml_node node)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {
				xml.attribute("name", "control");
				xml.node("key", [&] () { });
				xml.node("create_snapshot", [&] () { });
				xml.node("discard_snapshot", [&] () { });
			});

			return Config(Cstring(buf));
		}

	public:

		Control_file_system(Vfs::Env         &vfs_env,
		                    Genode::Xml_node  node,
		                    Wrapper          &cbe)
		:
			Control_local_factory(vfs_env, node, cbe),
			Vfs::Dir_file_system(vfs_env, Xml_node(_config(node).string()),
			                     *this)
		{ }

		static char const *type_name() { return "control"; }

		char const *type() override { return type_name(); }
};


struct Vfs_cbe::Local_factory : File_system_factory
{
	Snapshot_file_system _current_snapshot_fs;
	Snapshots_file_system _snapshots_fs;
	Control_file_system _control_fs;

	Local_factory(Vfs::Env &env, Xml_node config,
	              Wrapper &cbe)
	:
		_current_snapshot_fs(env, cbe, 0, false),
		_snapshots_fs(env, config, cbe),
		_control_fs(env, config, cbe)
	{ }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		using Name = String<64>;
		if (node.has_type(Snapshot_file_system::type_name())
		    && node.attribute_value("name", Name()) == "current")
			return &_current_snapshot_fs;

		if (node.has_type(Control_file_system::type_name()))
			return &_control_fs;

		if (node.has_type(Snapshots_file_system::type_name()))
			return &_snapshots_fs;

		return nullptr;
	}
};


class Vfs_cbe::File_system : private Local_factory,
                             public Vfs::Dir_file_system
{
	private:

		Wrapper &_wrapper;

		typedef String<256> Config;

		static Config _config(Xml_node node)
		{
			char buf[Config::capacity()] { };

			Xml_generator xml(buf, sizeof(buf), "dir", [&] () {
				typedef String<64> Name;

				xml.attribute("name", node.attribute_value("name", Name()));

				xml.node("control", [&] () { });

				xml.node("snapshot", [&] () {
					xml.attribute("name", "current");
				});

				xml.node("snapshots", [&] () { });
			});

			return Config(Cstring(buf));
		}

	public:

		File_system(Vfs::Env &vfs_env, Genode::Xml_node node,
		            Wrapper &wrapper)
		:
			Local_factory(vfs_env, node, wrapper),
			Vfs::Dir_file_system(vfs_env, Xml_node(_config(node).string()),
			                     *this),
			_wrapper(wrapper)
		{ }

		~File_system()
		{
			// XXX rather then destroying the wrapper here, it should be
			//     done on the out-side where it was allocated in the first
			//     place but the factory interface does not support that yet
			// destroy(vfs_env.alloc().alloc()), &_wrapper);
		}
};


/**************************
 ** VFS plugin interface **
 **************************/

extern "C" Vfs::File_system_factory *vfs_file_system_factory(void)
{
	struct Factory : Vfs::File_system_factory
	{
		Vfs::File_system *create(Vfs::Env &vfs_env,
		                         Genode::Xml_node node) override
		{
			try {
				/* XXX wrapper is not managed and will leak */
				Vfs_cbe::Wrapper *wrapper =
					new (vfs_env.alloc()) Vfs_cbe::Wrapper { vfs_env, node };
				return new (vfs_env.alloc())
					Vfs_cbe::File_system(vfs_env, node, *wrapper);
			} catch (...) {
				Genode::error("could not create 'cbe_fs' ");
			}
			return nullptr;
		}
	};

	/* the CBE library requires a stack larger than the default */
	Genode::Thread::myself()->stack_size(64*1024);

	adainit();

	Cbe::assert_valid_object_size<Cbe::Library>();

	static Factory factory;
	return &factory;
}


/*
 * The SPARK compiler might generate a call to memcmp when it wants to
 * compare objects. For the time being we implement here and hopefully
 * any other memcmp symbol has at least the same semantics.
 */
extern "C" int memcmp(const void *s1, const void *s2, Genode::size_t n)
{
	return Genode::memcmp(s1, s2, n);
}
