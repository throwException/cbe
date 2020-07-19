/*
 * Copyright (C) 2019-2020 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <vfs/dir_file_system.h>
#include <vfs/single_file_system.h>
#include <util/arg_string.h>
#include <util/xml_generator.h>
#include <trace/timestamp.h>

/* repo includes */
#include <util/sha256_4k.h>

/* cbe includes */
#include <cbe/library.h>
#include <cbe/external_crypto.h>

/* local includes */
#include "io_job.h"


namespace Vfs_cbe {
	using namespace Vfs;
	using namespace Genode;

	class Data_file_system;

	class Extend_file_system;
	class Rekey_file_system;
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

	class Backend_file;
	class Wrapper;
}


extern "C" void adainit();


extern "C" void print_u8(unsigned char const u) { Genode::log(u); }


extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	Genode::log(Genode::Cstring(s, len));
}


class Vfs_cbe::Wrapper
{
	private:

		Vfs::Env &_env;

		Vfs_handle            *_backend_handle { nullptr };
		Constructible<Io_job>  _backend_job { };

		friend struct Backend_io_response_handler;

		struct Backend_io_response_handler : Vfs::Io_response_handler
		{
			Vfs_cbe::Wrapper &_wrapper;
			Genode::Signal_context_capability _io_sigh;

			Backend_io_response_handler(Vfs_cbe::Wrapper &wrapper,
			                            Genode::Signal_context_capability io_sigh)
			: _wrapper(wrapper), _io_sigh(io_sigh) { }

			void read_ready_response() override { }

			void io_progress_response() override
			{
				if (_io_sigh.valid()) {
					Genode::Signal_transmitter(_io_sigh).submit();
				}
			}
		};

		Genode::Io_signal_handler<Wrapper> _io_handler {
			_env.env().ep(), *this, &Wrapper::_handle_io };

		void _handle_io()
		{
			_notify_backend_io_progress();
		}

		void _notify_backend_io_progress()
		{
			if (_enqueued_vfs_handle) {
				_enqueued_vfs_handle->io_progress_response();
			} else {
				handle_frontend_request();
				_io_progress_pending = true;
			}
		}

		Backend_io_response_handler _backend_io_response_handler { *this, _io_handler };

		Cbe::Io_buffer            _io_data { };
		Cbe::Crypto_cipher_buffer _cipher_data { };
		Cbe::Crypto_plain_buffer  _plain_data { };
		External::Crypto _crypto { };

		Constructible<Cbe::Library> _cbe;

	public:

		struct Rekeying
		{
			enum State { UNKNOWN, IDLE, IN_PROGRESS, };
			enum Result { NONE, SUCCESS, FAILED, };
			State    state;
			Result   last_result;
			uint32_t key_id;

			static char const *state_to_cstring(State const s)
			{
				switch (s) {
				case State::UNKNOWN:     return "unknown";
				case State::IDLE:        return "idle";
				case State::IN_PROGRESS: return "in-progress";
				}

				return "-";
			}
		};

		struct Extending
		{
			enum Type { INVALID, VBD, FT };
			enum State { UNKNOWN, IDLE, IN_PROGRESS, };
			enum Result { NONE, SUCCESS, FAILED, };
			Type   type;
			State  state;
			Result last_result;

			static char const *state_to_cstring(State const s)
			{
				switch (s) {
				case State::UNKNOWN:     return "unknown";
				case State::IDLE:        return "idle";
				case State::IN_PROGRESS: return "in-progress";
				}

				return "-";
			}

			static Type string_to_type(char const *s)
			{
				if (Genode::strcmp("vbd", s, 3) == 0) {
					return Type::VBD;
				} else

				if (Genode::strcmp("ft", s, 2) == 0) {
					return Type::FT;
				}

				return Type::INVALID;
			}
		};

	private:

		Rekeying _rekey_obj {
			.state       = Rekeying::State::UNKNOWN,
			.last_result = Rekeying::Result::NONE,
			.key_id      = 0, };

		Extending _extend_obj {
			.type        = Extending::Type::INVALID,
			.state       = Extending::State::UNKNOWN,
			.last_result = Extending::Result::NONE,
		};

		/* configuration options */
		bool _verbose       { false };
		bool _debug         { false };

		using Backend_device_path = Genode::String<32>;
		Backend_device_path _block_device { "/dev/block" };

		void _read_config(Xml_node config)
		{
			_verbose      = config.attribute_value("verbose", _verbose);
			_debug        = config.attribute_value("debug",   _debug);
			_block_device = config.attribute_value("block",   _block_device);
		}

		struct Could_not_open_block_backend : Genode::Exception { };
		struct No_valid_superblock_found    : Genode::Exception { };

		void _initialize_cbe()
		{
			using Result = Vfs::Directory_service::Open_result;

			Result res = _env.root_dir().open(_block_device.string(),
			                                  Vfs::Directory_service::OPEN_MODE_RDWR,
			                                  (Vfs::Vfs_handle **)&_backend_handle,
			                                  _env.alloc());
			if (res != Result::OPEN_OK) {
				error("cbe_fs: Could not open back end block device: '", _block_device, "'");
				throw Could_not_open_block_backend();
			}

			_backend_handle->handler(&_backend_io_response_handler);
			_cbe.construct();
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

			uint64_t offset { 0 };
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
			_frontend_request.cbe_request = Cbe::Request { };
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
				_frontend_request.cbe_request = Cbe::Request(
					op,
					false,
					0,
					0,
					1,
					0,
					0);
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

			if ((count % Cbe::BLOCK_SIZE) != 0 &&
			    !unaligned_request)
			{
				count = count - (count % Cbe::BLOCK_SIZE);
			}

			if (unaligned_request) {
				_helper_read_request.cbe_request = Cbe::Request(
					Cbe::Request::Operation::READ,
					false,
					offset / Cbe::BLOCK_SIZE,
					(uint64_t)&_helper_read_request.block_data,
					1,
					0,
					0);
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

			_frontend_request.offset = offset;
			_frontend_request.cbe_request = Cbe::Request(
				op,
				false,
				offset / Cbe::BLOCK_SIZE,
				(uint64_t)data,
				(uint32_t)(count / Cbe::BLOCK_SIZE),
				0,
				0);

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
					            "off: ", offset, " bytes: ", count,
					            " (front req: ", _frontend_request.cbe_request, ")");
				}
			}

			_frontend_request.snap_id = snap_id;
			return true;
		}

		bool _handle_cbe_backend(Cbe::Library &cbe, Cbe::Io_buffer &io_data)
		{
			Cbe::Io_buffer::Index data_index { 0 };
			Cbe::Request cbe_request = cbe.has_io_request(data_index);

			if (cbe_request.valid() && !_backend_job.constructed()) {

				file_offset const base_offset = cbe_request.block_number()
				                              * Cbe::BLOCK_SIZE;
				file_size const count = cbe_request.count()
				                      * Cbe::BLOCK_SIZE;

				_backend_job.construct(*_backend_handle, cbe_request.operation(),
				                       data_index, base_offset, count);
			}

			if (!_backend_job.constructed()) {
				return false;
			}

			bool progress = _backend_job->execute(cbe, io_data);

			if (_backend_job->completed()) {
				_backend_job.destruct();
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
				Cbe::Virtual_block_address const vba = request.block_number();
				uint32_t const snap_id = frontend_request.snap_id;

				if (vba > cbe.max_vba()) {
					warning("reject request with out-of-range virtual block start address ", vba);
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (vba + request.count() < vba) {
					warning("reject wraping request", vba);
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (vba + request.count() > (cbe.max_vba() + 1)) {
					warning("reject invalid request ", vba, " ", request.count());
					_frontend_request.state = ST::ERROR_EOF;
					return false;
				}

				if (cbe.client_request_acceptable()) {
					cbe.submit_client_request(request, snap_id);
					frontend_request.state = ST::IN_PROGRESS;
				}
			}

			cbe.execute(_io_data, _plain_data, _cipher_data);
			bool progress = cbe.execute_progress();

			using ST = Frontend_request::State;

			while (true) {
				Cbe::Request const cbe_request = cbe.peek_completed_client_request();
				if (!cbe_request.valid()) { break; }

				cbe.drop_completed_client_request(cbe_request);
				progress = true;

				if (cbe_request.operation() == Cbe::Request::Operation::REKEY) {
					bool const req_sucess = cbe_request.success();
					if (_verbose) {
						log("Complete request: backend request (", cbe_request, ")");
					}
					_rekey_obj.state = Rekeying::State::IDLE;
					_rekey_obj.last_result = req_sucess ? Rekeying::Result::SUCCESS
					                                    : Rekeying::Result::FAILED;
					continue;
				}

				if (cbe_request.operation() == Cbe::Request::Operation::EXTEND_VBD) {
					bool const req_sucess = cbe_request.success();
					if (_verbose) {
						log("Complete request: backend request (", cbe_request, ")");
					}
					_extend_obj.state = Extending::State::IDLE;
					_extend_obj.last_result =
						req_sucess ? Extending::Result::SUCCESS
						           : Extending::Result::FAILED;
					continue;
				}

				if (cbe_request.operation() == Cbe::Request::Operation::EXTEND_FT) {
					bool const req_sucess = cbe_request.success();
					if (_verbose) {
						log("Complete request: backend request (", cbe_request, ")");
					}
					_extend_obj.state = Extending::State::IDLE;
					_extend_obj.last_result =
						req_sucess ? Extending::Result::SUCCESS
						           : Extending::Result::FAILED;
					continue;
				}

				if (cbe_request.operation() == Cbe::Request::Operation::CREATE_SNAPSHOT) {
					if (_verbose) {
						log("Complete request: (", cbe_request, ")");
					}
					_create_snapshot_request.cbe_request = Cbe::Request();
					continue;
				}

				if (cbe_request.operation() == Cbe::Request::Operation::DISCARD_SNAPSHOT) {
					if (_verbose) {
						log("Complete request: (", cbe_request, ")");
					}
					_discard_snapshot_request.cbe_request = Cbe::Request();
					continue;
				}

				if (!cbe_request.success()) {
					_helper_read_request.state  = Helper_request::State::NONE;
					_helper_write_request.state = Helper_request::State::NONE;

					frontend_request.state = ST::COMPLETE;
					frontend_request.cbe_request.success(cbe_request.success());
					break;
				}

				if (_helper_read_request.in_progress()) {
					_helper_read_request.state = Helper_request::State::COMPLETE;
					_helper_read_request.cbe_request.success(
						cbe_request.success());
				} else if (_helper_write_request.in_progress()) {
					_helper_write_request.state = Helper_request::State::COMPLETE;
					_helper_write_request.cbe_request.success(
						cbe_request.success());
				} else {
					frontend_request.state = ST::COMPLETE;
					frontend_request.cbe_request.success(cbe_request.success());
					if (_verbose) {
						Genode::log("Complete request: ",
						            " (frontend request: ", _frontend_request.cbe_request,
						            " count: ", _frontend_request.count, ")");
					}
				}
			}

			if (_helper_read_request.complete()) {
				if (frontend_request.cbe_request.read()) {
					char       * dst = reinterpret_cast<char*>
						(frontend_request.cbe_request.offset());
					char const * src = reinterpret_cast<char const*>
						(&_helper_read_request.block_data) + frontend_request.helper_offset;

					Genode::memcpy(dst, src, _frontend_request.count);

					_helper_read_request.state = Helper_request::State::NONE;
					frontend_request.state = ST::COMPLETE;
					frontend_request.cbe_request.success(
						_helper_read_request.cbe_request.success());

					if (_verbose) {
						Genode::log("Complete unaligned READ request: ",
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
							(frontend_request.cbe_request.offset());
						Genode::memcpy(dst, src, _frontend_request.count);
					}

					/* re-use request */
					_helper_write_request.cbe_request = Cbe::Request(
						Cbe::Request::Operation::WRITE,
						false,
						_helper_read_request.cbe_request.block_number(),
						(uint64_t) &_helper_write_request.block_data,
						_helper_read_request.cbe_request.count(),
						_helper_read_request.cbe_request.key_id(),
						_helper_read_request.cbe_request.tag());

					_helper_write_request.state = Helper_request::State::PENDING;
					_helper_read_request.state  = Helper_request::State::NONE;
				}
				progress = true;
			}

			if (_helper_write_request.complete()) {
				if (_verbose) {
					Genode::log("Complete unaligned WRITE request: ",
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
			{
				struct Data_pointer_is_null : Genode::Exception { };
				struct Front_end_request_should_be_in_progress :
					Genode::Exception { };

				Cbe::Request cbe_req { };
				uint64_t vba { 0 };
				Cbe::Crypto_plain_buffer::Index plain_buf_idx { 0 };

				_cbe->client_transfer_read_data_required(
					cbe_req, vba, plain_buf_idx);

				if (cbe_req.valid()) {

					Cbe::Block_data *data { nullptr };

					if (_helper_read_request.in_progress()) {
						data = reinterpret_cast<Cbe::Block_data*>(
							&_helper_read_request.block_data);
					} else {

						if (_frontend_request.in_progress()) {
							// XXX check after helper request because it will be IN_PROGRESS
							//     in case helper request is used

							uint64_t buf_base { cbe_req.offset() };
							uint64_t blk_off { vba - cbe_req.block_number() };
							data = reinterpret_cast<Cbe::Block_data*>(
								buf_base + (blk_off * Cbe::BLOCK_SIZE));

						} else {
							throw Front_end_request_should_be_in_progress();
						}
					}
					if (data == nullptr) {
						throw Data_pointer_is_null();
					}
					Genode::memcpy(
						data,
						&_plain_data.item(plain_buf_idx),
						sizeof (Cbe::Block_data));

					_cbe->client_transfer_read_data_in_progress(
						plain_buf_idx);

					_cbe->client_transfer_read_data_completed(
						plain_buf_idx, true);

					progress = true;
				}
			}

			/* write */
			Cbe::Request cbe_request = cbe.client_data_required();
			if (cbe_request.valid() && cbe_request.write()) {
				uint64_t const prim_index = cbe.client_data_index(cbe_request);
				if (prim_index == ~0ull) {
					Genode::error("prim_index invalid: ", cbe_request);
					frontend_request.state = ST::ERROR;
					return progress;
				}

				Cbe::Block_data *data = nullptr;

				if (_helper_write_request.in_progress()) {
					data = reinterpret_cast<Cbe::Block_data*>(
						&_helper_write_request.block_data);
				} else

				if (_frontend_request.in_progress()) {
					// XXX check after helper request because it will be IN_PROGRESS
					//     in case helper request is used
					data = reinterpret_cast<Cbe::Block_data*>(
						cbe_request.offset() + (prim_index * Cbe::BLOCK_SIZE));
				} else
					throw -1;

				if (data == nullptr) {
					struct Data_nullptr { };
					throw Data_nullptr();
				}

				progress = cbe.supply_client_data(cbe_request, *data);
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

			/* add keys */
			while (true) {
				Cbe::Key key { };
				Cbe::Request request = _cbe->crypto_add_key_required(key);
				if (!request.valid()) {
					break;
				}
				_cbe->crypto_add_key_requested(request);
				External::Crypto::Key_data data { };
				memcpy(data.value, key.value, sizeof (data.value));

				_crypto.add_key(key.id, data);
				request.success(true);

				if (_verbose) {
					log("Add key: id " , (unsigned)key.id.value);
				}
				_cbe->crypto_add_key_completed(request);
				progress |= true;
			}

			/* remove keys */
			while (true) {
				Cbe::Key::Id key_id;
				Cbe::Request request =
					_cbe->crypto_remove_key_required(key_id);

				if (!request.valid()) {
					break;
				}
				_cbe->crypto_remove_key_requested(request);

				_crypto.remove_key(key_id);
				request.success(true);

				if (_verbose) {
					log("Remove key: id " , (unsigned)key_id.value);
				}
				_cbe->crypto_remove_key_completed(request);
				progress |= true;
			}

			/* encrypt */
			while (true) {
				Cbe::Crypto_plain_buffer::Index data_index { 0 };
				Cbe::Request request = cbe.crypto_cipher_data_required(data_index);
				if (!request.valid() || !cry.encryption_request_acceptable()) { break; }

				request.tag(data_index.value);
				cry.submit_encryption_request(request, plain.item(data_index), 0);
				cbe.crypto_cipher_data_requested(data_index);
				progress |= true;
			}

			while (true) {
				Cbe::Request const request = cry.peek_completed_encryption_request();
				if (!request.valid()) {
					break;
				}
				Cbe::Crypto_cipher_buffer::Index const data_index(request.tag());
				if (!cry.supply_cipher_data(request, cipher.item(data_index))) {
					break;
				}
				bool const success = request.success();
				cbe.supply_crypto_cipher_data(data_index, success);
				progress |= true;
			}

			/* decrypt */
			while (true) {
				Cbe::Crypto_cipher_buffer::Index data_index { 0 };
				Cbe::Request request = cbe.crypto_plain_data_required(data_index);
				if (!request.valid() || !cry.decryption_request_acceptable()) { break; }

				request.tag(data_index.value);
				cry.submit_decryption_request(request, cipher.item(data_index), 0);
				cbe.crypto_plain_data_requested(data_index);
				progress |= true;
			}

			while (true) {
				Cbe::Request const request = cry.peek_completed_decryption_request();
				if (!request.valid()) { break; }

				Cbe::Crypto_plain_buffer::Index const data_index(request.tag());
				if (!cry.supply_plain_data(request, plain.item(data_index))) {
					break;
				}
				bool const success = request.success();
				cbe.supply_crypto_plain_data(data_index, success);
				progress |= true;
			}

			return progress;
		}

		void _dump_state()
		{
			if (_debug) {
				static uint64_t cnt = 0;
				log("FE: ", Frontend_request::state_to_string(_frontend_request.state),
				     " (", _frontend_request.cbe_request, ") ",
				    "BE: ", *_backend_job, " ", ++cnt);
			}
		}

		void handle_frontend_request()
		{
			while (true) {

				bool progress = false;

				bool const frontend_progress =
					_handle_cbe_frontend(*_cbe, _frontend_request);
				progress |= frontend_progress;

				bool const backend_progress = _handle_cbe_backend(*_cbe, _io_data);
				progress |= backend_progress;

				bool const crypto_progress =
					_handle_crypto(*_cbe, _crypto, _cipher_data, _plain_data);
				progress |= crypto_progress;

				if (!progress) {
					_dump_state();
				}

				if (_debug) {
					log("frontend_progress: ", frontend_progress,
					    " backend_progress: ", backend_progress,
					    " crypto_progress: ", crypto_progress);
				}

				if (!progress) { break; }
			}

			Cbe::Info const info = _cbe->info();

			using ES = Extending::State;
			if (_extend_obj.state == ES::UNKNOWN && info.valid) {
				if (info.extending_ft) {
					_extend_obj.state = ES::IN_PROGRESS;
					_extend_obj.type  = Extending::Type::FT;
				} else

				if (info.extending_vbd) {
					_extend_obj.state = ES::IN_PROGRESS;
					_extend_obj.type  = Extending::Type::VBD;
				} else {
					_extend_obj.state = ES::IDLE;
				}
			}
			using RS = Rekeying::State;
			if (_rekey_obj.state == RS::UNKNOWN && info.valid) {
				_rekey_obj.state =
					info.rekeying ? RS::IN_PROGRESS : RS::IDLE;
			}
		}

		bool client_request_acceptable() const
		{
			return _cbe->client_request_acceptable();
		}

		bool start_rekeying()
		{
			if (!_cbe->client_request_acceptable()) {
				return false;
			}

			Cbe::Request req(
				Cbe::Request::Operation::REKEY,
				false,
				0, 0, 0,
				_rekey_obj.key_id,
				0);

			if (_verbose) {
				Genode::log("Req: (background req: ", req, ")");
			}

			_cbe->submit_client_request(req, 0);
			_rekey_obj.state       = Rekeying::State::IN_PROGRESS;
			_rekey_obj.last_result = Rekeying::Rekeying::FAILED;

			// XXX kick-off rekeying
			handle_frontend_request();
			return true;
		}

		Rekeying const rekeying_progress() const
		{
			return _rekey_obj;
		}


		bool start_extending(Extending::Type       type,
		                     Cbe::Number_of_blocks blocks)
		{
			if (!_cbe->client_request_acceptable()) {
				return false;
			}

			Cbe::Request::Operation op =
				Cbe::Request::Operation::INVALID;

			switch (type) {
			case Extending::Type::VBD:
				op = Cbe::Request::Operation::EXTEND_VBD;
				break;
			case Extending::Type::FT:
				op = Cbe::Request::Operation::EXTEND_FT;
				break;
			case Extending::Type::INVALID:
				return false;
			}

			Cbe::Request req(op, false,
			                 0, 0, blocks, 0, 0);

			if (_verbose) {
				Genode::log("Req: (background req: ", req, ")");
			}

			_cbe->submit_client_request(req, 0);
			_extend_obj.type        = type;
			_extend_obj.state       = Extending::State::IN_PROGRESS;
			_extend_obj.last_result = Extending::Result::NONE;

			// XXX kick-off extending
			handle_frontend_request();
			return true;
		}

		Extending const extending_progress() const
		{
			return _extend_obj;
		}

		void active_snapshot_ids(Cbe::Active_snapshot_ids &ids)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}
			_cbe->active_snapshot_ids(ids);
			handle_frontend_request();
		}


		Frontend_request _create_snapshot_request { };

		bool create_snapshot()
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}

			if (!_cbe->client_request_acceptable()) {
				return false;
			}

			if (_create_snapshot_request.cbe_request.valid()) {
				return false;
			}

			Cbe::Request::Operation const op =
				Cbe::Request::Operation::CREATE_SNAPSHOT;

			_create_snapshot_request.cbe_request =
				Cbe::Request(op, false, 0, 0, 1, 0, 0);

			if (_verbose) {
				Genode::log("Req: (req: ", _create_snapshot_request.cbe_request, ")");
			}

			_cbe->submit_client_request(_create_snapshot_request.cbe_request, 0);

			_create_snapshot_request.state =
				Frontend_request::State::IN_PROGRESS;

			// XXX kick-off snapshot creation request
			handle_frontend_request();
			return true;
		}

		Frontend_request _discard_snapshot_request { };

		bool discard_snapshot(Cbe::Generation id)
		{
			if (!_cbe.constructed()) {
				_initialize_cbe();
			}

			if (!_cbe->client_request_acceptable()) {
				return false;
			}

			if (_discard_snapshot_request.cbe_request.valid()) {
				return false;
			}

			Cbe::Request::Operation const op =
				Cbe::Request::Operation::DISCARD_SNAPSHOT;

			_discard_snapshot_request.cbe_request =
				Cbe::Request(op, false, 0, 0, 1, 0, 0);

			if (_verbose) {
				Genode::log("Req: (req: ", _discard_snapshot_request.cbe_request, ")");
			}

			_cbe->submit_client_request(_discard_snapshot_request.cbe_request, 0);

			_discard_snapshot_request.state =
				Frontend_request::State::IN_PROGRESS;

			// XXX kick-off snapshot creation request
			handle_frontend_request();
			return true;
		}

		Genode::Mutex _frontend_mtx { };

		Genode::Mutex &frontend_mtx() { return _frontend_mtx; }
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
				Genode::Mutex::Guard guard { _w.frontend_mtx() };

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
				Genode::Mutex::Guard guard { _w.frontend_mtx() };

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
				Genode::Mutex::Guard guard { _w.frontend_mtx() };

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

			/* max_vba range is from 0 ... N - 1 */
			out.size = (_w.cbe().max_vba() + 1) * Cbe::BLOCK_SIZE;
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


class Vfs_cbe::Extend_file_system : public Vfs::Single_file_system
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
				if (seek() != 0) {
					out_count = 0;
					return READ_OK;
				}

				using Extending = Wrapper::Extending;

				Extending const exp = _w.extending_progress();

				bool const in_progress =
					exp.state == Extending::State::IN_PROGRESS;
				bool const last_result =
					!in_progress && exp.last_result != Extending::Result::NONE;
				bool const success =
					exp.last_result == Extending::Result::SUCCESS;

				using Result = Genode::String<32>;
				Result result {
					Extending::state_to_cstring(exp.state),
					" last-result:", last_result ? success ?
					                 "success" : "failed" : "none",
					"\n" };
				copy_cstring(dst, result.string(), count);
				size_t const length_without_nul = result.length() - 1;
				out_count = count > length_without_nul - 1 ?
				            length_without_nul : count;
				return READ_OK;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				using Type = Wrapper::Extending::Type;
				using State = Wrapper::Extending::State;
				if (_w.extending_progress().state != State::IDLE) {
					return WRITE_ERR_IO;
				}

				char tree[16];
				Arg_string::find_arg(src, "tree").string(tree, sizeof (tree), "-");
				Type type = Wrapper::Extending::string_to_type(tree);
				if (type == Type::INVALID) {
					return WRITE_ERR_IO;
				}

				unsigned long blocks = Arg_string::find_arg(src, "blocks").ulong_value(0);
				if (blocks == 0) {
					return WRITE_ERR_IO;
				}

				bool const okay = _w.start_extending(type, blocks);
				if (!okay) {
					return WRITE_ERR_IO;
				}

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Extend_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<extend/>")),
			_w(w)
		{ }

		static char const *type_name() { return "extend"; }

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


class Vfs_cbe::Rekey_file_system : public Vfs::Single_file_system
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
				if (seek() != 0) {
					out_count = 0;
					return READ_OK;
				}

				Wrapper::Rekeying const & rkp =
					_w.rekeying_progress();

				using Result = Genode::String<32>;
				using Rekeying = Wrapper::Rekeying;
				bool const in_progress =
					rkp.state == Rekeying::State::IN_PROGRESS;
				bool const last_result =
					!in_progress && rkp.last_result != Rekeying::Result::NONE;
				bool const success =
					rkp.last_result == Rekeying::Result::SUCCESS;

				Result result {
					Rekeying::state_to_cstring(rkp.state),
					" last-result:", last_result ? success ?
					                 "success" : "failed" : "none",
					"\n" };
				copy_cstring(dst, result.string(), count);
				size_t const length_without_nul = result.length() - 1;
				out_count = count > length_without_nul - 1 ?
				            length_without_nul : count;
				return READ_OK;
			}

			Write_result write(char const *src, file_size count, file_size &out_count) override
			{
				using State = Wrapper::Rekeying::State;
				if (_w.rekeying_progress().state != State::IDLE) {
					return WRITE_ERR_IO;
				}

				bool start_rekeying { false };
				Genode::ascii_to(src, start_rekeying);

				if (!start_rekeying) {
					return WRITE_ERR_IO;
				}

				if (!_w.start_rekeying()) {
					return WRITE_ERR_IO;
				}

				out_count = count;
				return WRITE_OK;
			}

			bool read_ready() override { return true; }
		};

	public:

		Rekey_file_system(Wrapper &w)
		:
			Single_file_system(Node_type::TRANSACTIONAL_FILE, type_name(),
			                   Node_rwx::wo(), Xml_node("<rekey/>")),
			_w(w)
		{ }

		static char const *type_name() { return "rekey"; }

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

				if (!_w.create_snapshot()) {
					out_count = 0;
					return WRITE_OK;
				}

				out_count = count;
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

				if (!_w.discard_snapshot(Cbe::Generation { id })) {
					out_count = 0;
					return WRITE_OK;
				}

				return WRITE_ERR_IO;
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

			struct Invalid_index : Genode::Exception { };
			struct Invalid_path  : Genode::Exception { };

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
				if (fsp == nullptr) {
					throw Invalid_index();
				}
				return *fsp;
			}

			Snapshot_file_system &_by_id(uint32_t id)
			{
				Snapshot_file_system *fsp { nullptr };
				auto lookup = [&] (Snapshot_file_system &fs) {
					if (fs.snapshot_id() == id) {
						fsp = &fs;
					}
				};
				_snap_fs.for_each(lookup);
				if (fsp == nullptr) {
					throw Invalid_path();
				}
				return *fsp;
			}

			Snapshot_file_system &by_path(char const *path)
			{
				if (!path) {
					throw Invalid_path();
				}

				if (path[0] == '/') {
					path++;
				}

				uint32_t id { 0 };
				Genode::ascii_to(path, id);
				return _by_id(id);
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

			Read_result _query_snapshots(file_size  index,
			                             file_size &out_count,
			                             Dirent    &out)
			{
				if (index >= _snap_reg.number_of_snapshots()) {
					out_count = sizeof(Dirent);
					out.type = Dirent_type::END;
					return READ_OK;
				}

				try {
					Snapshot_file_system const &fs = _snap_reg.by_index(index);
					Genode::String<32> name { fs.snapshot_id() };

					out = {
						.fileno = (Genode::addr_t)this | index,
						.type   = Dirent_type::DIRECTORY,
						.rwx    = Node_rwx::rx(),
						.name   = { name.string() },
					};
					out_count = sizeof(Dirent);
					return READ_OK;
				} catch (Snapshot_registry::Invalid_index) {
					return READ_ERR_INVALID;
				}
			}

			Read_result _query_root(file_size  index,
			                        file_size &out_count,
			                        Dirent    &out)
			{
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

				out_count = sizeof(Dirent);
				return READ_OK;
			}

			Dir_vfs_handle(Directory_service &ds,
			               File_io_service   &fs,
			               Genode::Allocator &alloc,
			               Snapshot_registry const &snap_reg,
			               bool root_dir)
			:
				Snap_vfs_handle(ds, fs, alloc, 0),
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

					/* opended as "/snapshots" */
					return _query_snapshots(index, out_count, out);

				} else {
					/* opened as "/" */
					return _query_root(index, out_count, out);
				}
			}

			Write_result write(char const *, file_size, file_size &) override
			{
				return WRITE_ERR_INVALID;
			}

			bool read_ready() override { return true; }

		};

		struct Dir_snap_vfs_handle : Vfs::Vfs_handle
		{
			Vfs_handle &vfs_handle;

			Dir_snap_vfs_handle(Directory_service &ds,
			                    File_io_service   &fs,
			                    Genode::Allocator &alloc,
			                    Vfs::Vfs_handle   &vfs_handle)
			: Vfs_handle(ds, fs, alloc, 0), vfs_handle(vfs_handle) { }

			~Dir_snap_vfs_handle()
			{
				vfs_handle.close();
			}
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

			try {
				Snapshot_file_system &fs = _snap_reg.by_path(path);
				return fs.open(path, mode, out_handle, alloc);
			} catch (Snapshot_registry::Invalid_path) { }

			return OPEN_ERR_UNACCESSIBLE;
		}

		Opendir_result opendir(char const       *path,
		                       bool              create,
		                       Vfs::Vfs_handle **out_handle,
		                       Allocator        &alloc) override
		{
			if (create) {
				return OPENDIR_ERR_PERMISSION_DENIED;
			}

			bool const top = _top_dir(path);
			if (_root_dir(path) || top) {
				_snap_reg.update(_vfs_env);

				*out_handle = new (alloc) Dir_vfs_handle(*this, *this, alloc,
				                                         _snap_reg, top);
				return OPENDIR_OK;
			} else {
				char const *sub_path = _sub_path(path);
				if (!sub_path) {
					return OPENDIR_ERR_LOOKUP_FAILED;
				}
				try {
					Snapshot_file_system &fs = _snap_reg.by_path(sub_path);
					Vfs::Vfs_handle *handle = nullptr;
					Opendir_result const res = fs.opendir(sub_path, create, &handle, alloc);
					if (res != OPENDIR_OK) {
						return OPENDIR_ERR_LOOKUP_FAILED;
					}
					*out_handle = new (alloc) Dir_snap_vfs_handle(*this, *this,
					                                              alloc, *handle);
					return OPENDIR_OK;
				} catch (Snapshot_registry::Invalid_path) { }
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

			try {
				Snapshot_file_system &fs = _snap_reg.by_path(path);
				Stat_result const res = fs.stat(path, out_stat);
				return res;
			} catch (Snapshot_registry::Invalid_path) { }

			return STAT_ERR_NO_ENTRY;
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
			if (_top_dir(path) || _root_dir(path)) {
				_snap_reg.update(_vfs_env);
				file_size const num = _snap_reg.number_of_snapshots();
				return num;
			}
			_snap_reg.update(_vfs_env);

			path = _sub_path(path);
			if (!path) {
				return 0;
			}
			try {
				Snapshot_file_system &fs = _snap_reg.by_path(path);
				file_size const num = fs.num_dirent(path);
				return num;
			} catch (Snapshot_registry::Invalid_path) {
				return 0;
			}
		}

		bool directory(char const *path) override
		{
			if (_root_dir(path)) {
				return true;
			}

			path = _sub_path(path);
			if (!path) {
				return false;
			}
			try {
				Snapshot_file_system &fs = _snap_reg.by_path(path);
				return fs.directory(path);
			} catch (Snapshot_registry::Invalid_path) { }

			return false;
		}

		char const *leaf_path(char const *path) override
		{
			path = _sub_path(path);
			if (!path) {
				return nullptr;
			}

			if (strlen(path) == 0 || strcmp(path, "") == 0) {
				return path;
			}

			try {
				Snapshot_file_system &fs = _snap_reg.by_path(path);
				char const *leaf_path = fs.leaf_path(path);
				if (leaf_path) {
					return leaf_path;
				}
			} catch (Snapshot_registry::Invalid_path) { }

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

		bool queue_read(Vfs::Vfs_handle *vfs_handle, file_size size) override
		{
			Dir_snap_vfs_handle *dh =
				dynamic_cast<Dir_snap_vfs_handle*>(vfs_handle);
			if (dh) {
				return dh->vfs_handle.fs().queue_read(&dh->vfs_handle,
				                                      size);
			}

			return true;
		}

		Read_result complete_read(Vfs::Vfs_handle *vfs_handle,
		                          char *dst, file_size count,
		                          file_size & out_count) override
		{
			Snap_vfs_handle *sh =
				dynamic_cast<Snap_vfs_handle*>(vfs_handle);
			if (sh) {
				Read_result const res = sh->read(dst, count, out_count);
				return res;
			}

			Dir_snap_vfs_handle *dh =
				dynamic_cast<Dir_snap_vfs_handle*>(vfs_handle);
			if (dh) {
				return dh->vfs_handle.fs().complete_read(&dh->vfs_handle,
				                                         dst, count, out_count);
			}

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
	Rekey_file_system             _rekeying_fs;
	Create_snapshot_file_system   _create_snapshot_fs;
	Discard_snapshot_file_system  _discard_snapshot_fs;
	Extend_file_system            _extend_fs;

	Control_local_factory(Vfs::Env     &env,
	                      Xml_node      config,
	                      Wrapper      &cbe)
	:
		_rekeying_fs(cbe),
		_create_snapshot_fs(cbe),
		_discard_snapshot_fs(cbe),
		_extend_fs(cbe)
	{ }

	Vfs::File_system *create(Vfs::Env&, Xml_node node) override
	{
		if (node.has_type(Rekey_file_system::type_name())) {
			return &_rekeying_fs;
		}

		if (node.has_type(Create_snapshot_file_system::type_name())) {
			return &_create_snapshot_fs;
		}

		if (node.has_type(Discard_snapshot_file_system::type_name())) {
			return &_discard_snapshot_fs;
		}

		if (node.has_type(Extend_file_system::type_name())) {
			return &_extend_fs;
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
				xml.node("rekey", [&] () { });
				xml.node("extend", [&] () { });
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

				xml.attribute("name",
				              node.attribute_value("name",
				                                   Name("cbe")));

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
