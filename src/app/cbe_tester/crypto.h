/*
 * \brief  Implementation of the Crypto module API using the Crypto VFS API
 * \author Martin Stein
 * \date   2020-10-29
 */

/*
 * Copyright (C) 2020 Genode Labs GmbH
 *
 * This file is part of the Genode OS framework, which is distributed
 * under the terms of the GNU Affero General Public License version 3.
 */

#ifndef _CBE_TESTER__CRYPTO_H_
#define _CBE_TESTER__CRYPTO_H_

/* CBE tester includes */
#include <vfs_utilities.h>

class Crypto
{
	public:

		enum class Operation
		{
			INVALID,
			DECRYPT_BLOCK,
			ENCRYPT_BLOCK
		};

		enum class Result
		{
			SUCCEEDED,
			FAILED,
			RETRY_LATER
		};

	private:

		using Read_result = Vfs::File_io_service::Read_result;
		using Open_result = Vfs::Directory_service::Open_result;
		using Vfs_handle = Vfs::Vfs_handle;
		using uint32_t = Genode::uint32_t;
		using Xml_node = Genode::Xml_node;
		using Signal_context_capability = Genode::Signal_context_capability;
		using Key = Cbe::Key;
		using Crypto_cipher_buffer = Cbe::Crypto_cipher_buffer;
		using Crypto_plain_buffer = Cbe::Crypto_plain_buffer;

		template <Genode::size_t CAPACITY>
		using String = Genode::String<CAPACITY>;

		struct Key_directory
		{
			Vfs_handle *encrypt_handle;
			Vfs_handle *decrypt_handle;
			uint32_t    key_id;
		};

		enum class Job_state
		{
			SUBMITTED,
			OP_WRITTEN_TO_VFS_HANDLE,
			READING_VFS_HANDLE_SUCCEEDED,
			COMPLETE
		};

		struct Job
		{
			Cbe::Request                 request;
			Vfs_handle                  *handle;
			Job_state                    state;
			Operation                    op;
			Crypto_cipher_buffer::Index  cipher_buf_idx;
			Crypto_plain_buffer::Index   plain_buf_idx;
		};

		Vfs::Env                &_env;
		String<32>        const  _path;
		Vfs_handle              &_add_key_handle;
		Vfs_handle              &_remove_key_handle;
		Vfs_io_response_handler  _vfs_io_response_handler;
		Key_directory            _key_dirs[2] {

			{
				.encrypt_handle = nullptr,
				.decrypt_handle = nullptr,
				.key_id = 0
			}, {
				.encrypt_handle = nullptr,
				.decrypt_handle = nullptr,
				.key_id = 0
			}
		};
		Job _job {

			.request        = Cbe::Request { },
			.handle         = nullptr,
			.state          = Job_state::SUBMITTED,
			.op             = Operation::INVALID,
			.cipher_buf_idx = Cbe::Crypto_cipher_buffer::Index { 0 },
			.plain_buf_idx  = Cbe::Crypto_plain_buffer::Index  { 0 }
		};

		Key_directory &_get_unused_key_dir()
		{
			for (Key_directory &key_dir : _key_dirs) {
				if (key_dir.key_id == 0) {
					return key_dir;
				}
			}
			class Failed { };
			throw Failed { };
		}

		Key_directory &_lookup_key_dir(uint32_t key_id)
		{
			for (Key_directory &key_dir : _key_dirs) {
				if (key_dir.key_id == key_id) {
					return key_dir;
				}
			}
			class Failed { };
			throw Failed { };
		}

		void _execute_decrypt_block(Job                  &job,
		                            Crypto_plain_buffer  &plain_buf,
		                            Crypto_cipher_buffer &cipher_buf,
		                            bool                 &progress);

		void _execute_encrypt_block(Job                  &job,
		                            Crypto_plain_buffer  &plain_buf,
		                            Crypto_cipher_buffer &cipher_buf,
		                            bool                 &progress);

	public:

		Crypto(Vfs::Env                  &env,
		       Xml_node            const &crypto,
		       Signal_context_capability  sigh);

		bool request_acceptable() const;

		Result add_key(Key const &key);

		Result remove_key(Cbe::Key::Id key_id);

		void submit_request(Cbe::Request          const &request,
		                    Operation                    op,
		                    Crypto_plain_buffer::Index   plain_buf_idx,
		                    Crypto_cipher_buffer::Index  cipher_buf_idx);

		Cbe::Request peek_completed_encryption_request() const;

		Cbe::Request peek_completed_decryption_request() const;

		void drop_completed_request();

		void execute(Crypto_plain_buffer  &plain_buf,
		             Crypto_cipher_buffer &cipher_buf,
		             bool                 &progress);
};

#endif /* _CBE_TESTER__CRYPTO_H_ */
