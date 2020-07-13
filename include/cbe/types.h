/*
 * Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

#ifndef _CBE_TYPES_H_
#define _CBE_TYPES_H_

/* Genode includes */
#include <base/stdint.h>
#include <base/output.h>
#include <block/request_stream.h>

namespace Cbe {

	using namespace Genode;
	using Number_of_primitives   = size_t;
	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;
	using Height                 = uint32_t;
	using Number_of_leaves       = uint64_t;
	using Number_of_leafs        = uint64_t;
	using Number_of_blocks       = uint64_t;
	using Degree                 = uint32_t;

	static constexpr uint32_t BLOCK_SIZE = 4096;
	static constexpr uint32_t NR_OF_SNAPSHOTS = 48;


	class Request
	{
		public:

			enum class Operation : uint32_t {
				INVALID = 0,
				READ = 1,
				WRITE = 2,
				SYNC = 3,
				CREATE_SNAPSHOT = 4,
				DISCARD_SNAPSHOT = 5,
				REKEY = 6,
				EXTEND_VBD = 7,
				EXTEND_FT = 8,
				RESUME_REKEYING = 10,
				DEINITIALIZE = 11,
				INITIALIZE = 12,
			};

		private:

			Operation        _operation;
			bool             _success;
			uint64_t         _block_number;
			uint64_t         _offset;
			Number_of_blocks _count;
			uint32_t         _key_id;
			uint32_t         _tag;

		public:

			Request(Operation        operation,
			        bool             success,
			        uint64_t         block_number,
			        uint64_t         offset,
			        Number_of_blocks count,
			        uint32_t         key_id,
			        uint32_t         tag)
			:
				_operation    { operation    },
				_success      { success      },
				_block_number { block_number },
				_offset       { offset       },
				_count        { count        },
				_key_id       { key_id       },
				_tag          { tag          }
			{ }

			Request()
			:
				_operation    { Operation::INVALID },
				_success      { false },
				_block_number { 0 },
				_offset       { 0 },
				_count        { 0 },
				_key_id       { 0 },
				_tag          { 0 }
			{ }

			bool valid() const
			{
				return _operation != Operation::INVALID;
			}

			void print(Genode::Output &out) const;


			/***************
			 ** Accessors **
			 ***************/

			bool read()             const { return _operation == Operation::READ; }
			bool write()            const { return _operation == Operation::WRITE; }
			bool sync()             const { return _operation == Operation::SYNC; }
			bool create_snapshot()  const { return _operation == Operation::CREATE_SNAPSHOT; }
			bool discard_snapshot() const { return _operation == Operation::DISCARD_SNAPSHOT; }
			bool rekey()            const { return _operation == Operation::REKEY; }
			bool extend_vbd()       const { return _operation == Operation::EXTEND_VBD; }
			bool extend_ft()        const { return _operation == Operation::EXTEND_FT; }
			bool resume_rekeying()  const { return _operation == Operation::RESUME_REKEYING; }
			bool deinitialize()     const { return _operation == Operation::DEINITIALIZE; }
			bool initialize()       const { return _operation == Operation::INITIALIZE; }

			Operation        operation()    const { return _operation; }
			bool             success()      const { return _success; }
			uint64_t         block_number() const { return _block_number; }
			uint64_t         offset()       const { return _offset; }
			Number_of_blocks count()        const { return _count; }
			uint32_t         key_id()       const { return _key_id; }
			uint32_t         tag()          const { return _tag; }

			void success(bool arg) { _success = arg; }
			void tag(uint32_t arg)    { _tag = arg; }

	} __attribute__((packed));


	struct Block_data
	{
		char values[BLOCK_SIZE];

		void print(Genode::Output &out) const
		{
			using namespace Genode;
			for (char const c : values) {
				Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD), " ");
			}
			Genode::print(out, "\n");
		}
	} __attribute__((packed));


	class Io_buffer
	{
		private:

			Block_data items[1];

		public:

			struct Bad_index : Genode::Exception { };

			struct Index
			{
				uint32_t value;

				explicit Index(uint32_t value) : value(value) { }

			} __attribute__((packed));

			Block_data &item(Index const idx)
			{
				if (idx.value >= sizeof(items) / sizeof(items[0])) {
					throw Bad_index();
				}
				return items[idx.value];
			}

	} __attribute__((packed));


	class Crypto_plain_buffer
	{
		private:

			Block_data items[1];

		public:

			struct Bad_index : Genode::Exception { };

			struct Index
			{
				uint32_t value;

				explicit Index(uint32_t value) : value(value) { }

			} __attribute__((packed));

			Block_data &item(Index const idx)
			{
				if (idx.value >= sizeof(items) / sizeof(items[0])) {
					throw Bad_index();
				}
				return items[idx.value];
			}

	} __attribute__((packed));


	class Crypto_cipher_buffer
	{
		private:

			Block_data items[1];

		public:

			struct Bad_index : Genode::Exception { };

			struct Index
			{
				uint32_t value;

				explicit Index(uint32_t value) : value(value) { }

			} __attribute__((packed));

			Block_data &item(Index const idx)
			{
				if (idx.value >= sizeof(items) / sizeof(items[0])) {
					throw Bad_index();
				}
				return items[idx.value];
			}
	} __attribute__((packed));


	struct Key
	{
		enum { KEY_SIZE = 32 };
		char value[KEY_SIZE];

		struct Id { uint32_t value; };
		Id id;

		using String = Genode::String<sizeof(value) * 2 + 3>;

		void print(Genode::Output &out) const
		{
			using namespace Genode;
			Genode::print(out, "[", id.value, ", ");
			for (uint32_t i = 0; i < 4; i++) {
				Genode::print(out, Hex(value[i], Hex::OMIT_PREFIX, Hex::PAD));
			}
			Genode::print(out, "...]");
		}
	} __attribute__((packed));


	struct Active_snapshot_ids
	{
		uint64_t values[NR_OF_SNAPSHOTS];
	} __attribute__((packed));
}


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


char const *to_string(Cbe::Request::Operation op)
{
	struct Unknown_operation_type : Genode::Exception { };
	switch (op) {
	case Cbe::Request::Operation::INVALID: return "invalid";
	case Cbe::Request::Operation::READ: return "read";
	case Cbe::Request::Operation::WRITE: return "write";
	case Cbe::Request::Operation::SYNC: return "sync";
	case Cbe::Request::Operation::CREATE_SNAPSHOT: return "create_snapshot";
	case Cbe::Request::Operation::DISCARD_SNAPSHOT: return "discard_snapshot";
	case Cbe::Request::Operation::REKEY: return "rekey";
	case Cbe::Request::Operation::EXTEND_VBD: return "extend_vbd";
	case Cbe::Request::Operation::EXTEND_FT: return "extend_ft";
	case Cbe::Request::Operation::RESUME_REKEYING: return "resume_rekeying";
	case Cbe::Request::Operation::DEINITIALIZE: return "deinitialize";
	case Cbe::Request::Operation::INITIALIZE: return "initialize";
	}
	throw Unknown_operation_type();
}


namespace Cbe {

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

}


void Cbe::Request::print(Genode::Output &out) const
{
	if (!valid()) {
		Genode::print(out, "<invalid>");
		return;
	}
	Genode::print(out, "tag: ", _tag);
	Genode::print(out, " block_number: ", _block_number);
	Genode::print(out, " count: ", _count);
	Genode::print(out, " key_id: ", _key_id);
	Genode::print(out, " offset: ", _offset);
	Genode::print(out, " op: ", to_string (_operation));
	Genode::print(out, " success: ", _success);
}

#endif /* _CBE_TYPES_H_ */
