/*
 * Copyright (C) 2020 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* cbe_crypto includes */
#include <cbe_crypto/interface.h>

/* cbe includes */
#include <cbe/external_crypto.h>


extern "C" void print_u8(unsigned char const u) { Genode::log(u); }


/*
 * The SPARK compiler might generate a call to memcmp when it wants to
 * compare objects. For the time being we implement here and hopefully
 * any other memcmp symbol has at least the same semantics.
 */
extern "C" int memcmp(const void *s1, const void *s2, Genode::size_t n)
{
	return Genode::memcmp(s1, s2, n);
}


namespace {

using namespace Genode;

struct Crypto : Cbe_crypto::Interface
{
	struct Buffer_too_small        : Genode::Exception { };
	struct Key_value_size_mismatch : Genode::Exception { };

	External::Crypto _external_crypto { };

	Crypto()
	{
		Cbe::assert_valid_object_size<External::Crypto>();

		external_crypto_cxx_init();
	}

	/***************
	 ** interface **
	 ***************/

	bool execute() override
	{
		return _external_crypto.execute();
	}

	bool add_key(uint32_t const  id,
	             char const     *value,
	             size_t          value_len) override
	{
		External::Crypto::Key_data data { };

		size_t const data_len = sizeof (data.value);
		if (value_len != data_len) {
			error("key value size mismatch, expected: ",
			      data_len, " got: ", value_len);
			throw Key_value_size_mismatch();
		}

		memcpy(data.value, value, data_len);

		if (!_slots.store(id)) {
			return false;
		}

		log("Add key: id " , id);
		_external_crypto.add_key(Cbe::Key::Id { id }, data);
		return true;
	}

	bool remove_key(uint32_t const id) override
	{
		log("Remove key: id " , id);
		_external_crypto.remove_key(Cbe::Key::Id { id });
		_slots.remove(id);
		return true;
	}

	bool submit_encryption_request(uint64_t const  block_number,
	                               uint32_t const  key_id,
	                               char     const *src,
	                               size_t   const  src_len) override
	{
		if (!_external_crypto.encryption_request_acceptable()) {
			return false;
		}

		Cbe::Request const request(Cbe::Request::Operation::WRITE,
		                           false, block_number, 0, 1, key_id, 0);

		if (src_len < sizeof (Cbe::Block_data)) {
			error("buffer too small");
			throw Buffer_too_small();
		}

		Cbe::Block_data const &block_data =
			*reinterpret_cast<Cbe::Block_data const*>(src);

		_external_crypto.submit_encryption_request(request, block_data, key_id);
		return true;
	}

	Complete_request encryption_request_complete(char *dst, size_t const dst_len) override
	{
		Cbe::Request const request =
			_external_crypto.peek_completed_encryption_request();

		if (!request.valid()) {
			return Complete_request { .valid = false, .block_number = 0 };
		}

		if (dst_len < sizeof (Cbe::Block_data)) {
			error("buffer too small");
			throw Buffer_too_small();
		}

		Cbe::Block_data &block_data =
			*reinterpret_cast<Cbe::Block_data*>(dst);
		return Complete_request {
			.valid        = _external_crypto.supply_cipher_data(request, block_data),
			.block_number = request.block_number() };
	}

	bool submit_decryption_request(uint64_t const  block_number,
	                               uint32_t const  key_id,
	                               char     const *src,
	                               size_t   const  src_len) override
	{
		if (!_external_crypto.decryption_request_acceptable()) {
			return false;
		}

		Cbe::Request const request(Cbe::Request::Operation::READ,
		                           false, block_number, 0, 1, key_id, 0);

		if (src_len < sizeof (Cbe::Block_data)) {
			error("buffer too small");
			throw Buffer_too_small();
		}

		Cbe::Block_data const &block_data =
			*reinterpret_cast<Cbe::Block_data const*>(src);

		_external_crypto.submit_decryption_request(request, block_data, key_id);
		return true;
	}

	Complete_request decryption_request_complete(char *dst, size_t dst_len) override
	{
		Cbe::Request const request =
			_external_crypto.peek_completed_decryption_request();

		if (!request.valid()) {
			return Complete_request { .valid = false, .block_number = 0 };
		}

		Cbe::Block_data &block_data =
			*reinterpret_cast<Cbe::Block_data*>(dst);

		return Complete_request {
			.valid        = _external_crypto.supply_plain_data(request, block_data),
			.block_number = request.block_number() };
	}
};

} /* anonymous namespace */


Cbe_crypto::Interface &Cbe_crypto::get_interface()
{
	static Crypto inst;
	return inst;
}
