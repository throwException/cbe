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

	/*
	 * Type definitions
	 *
	 * (Tp prevent implicit conversions better use a structured
	 *  type.)
	 */

	static constexpr uint32_t BLOCK_SIZE = 4096;

	using Number_of_primitives = size_t;

	enum {
		INVALID_GEN = 18446744073709551615ULL,
		INVALID_PBA = 18446744073709551615ULL,
		INVALID_VBA = 18446744073709551615ULL,
	};

	using Physical_block_address = uint64_t;
	using Virtual_block_address  = uint64_t;
	using Generation             = uint64_t;
	using Height                 = uint32_t;
	using Number_of_leaves       = uint64_t;
	using Number_of_leafs        = uint64_t;
	using Number_of_blocks       = uint64_t;
	using Degree                 = uint32_t;
	using Timestamp              = uint64_t;
	using Snapshot_index         = uint32_t;
	using Number_of_superblocks  = uint64_t;

	struct Snapshot_ID
	{
		uint64_t value;
		bool valid;

		Snapshot_ID(uint64_t value, bool valid)
		: value(value), valid(valid) { }

		void print(Genode::Output &out) const
		{
			Genode::print(out, "[", value, ",", valid, "]");
		}
	};

	struct Token
	{
		uint64_t value;

		Token(uint64_t value) : value(value) { }

		void print(Genode::Output &out) const
		{
			Genode::print(out, "[", value, "]");
		}
	};

	struct Index {
		enum { INVALID = 18446744073709551615ULL, };
		uint64_t value;
	};


	struct Timeout_request {
		bool      valid   { false };
		Timestamp timeout { 0 };
	};


	/*
	 * List of special tags used throughout the CBE.
	 *
	 * (The type is currently specified by
	 *  - bits [23:16] contain the meta-type (affiliation to meta-module)
	 *  - bits [15: 8] contain the type (affiliation to a module)
	 *  - bits [ 7: 0] contain the sub-type
	 *  but is more adhoc than really designed.)
	 */

	enum class Tag : Genode::uint32_t {
		INVALID_TAG        = 0x00,
		IO_TAG             = 0x10,
		CACHE_TAG          = 0x20,
		CACHE_FLUSH_TAG    = CACHE_TAG | 0x1,
		CRYPTO_TAG         = 0x30,
		CRYPTO_TAG_DECRYPT = CRYPTO_TAG | 0x1,
		CRYPTO_TAG_ENCRYPT = CRYPTO_TAG | 0x2,
		POOL_TAG           = 0x40,
		SPLITTER_TAG       = 0x50,
		TRANSLATION_TAG    = 0x60,
		WRITE_BACK_TAG     = 0x70,
		SYNC_SB_TAG        = 0x80,
		RECLAIM_TAG        = 0x90,

		VBD_TAG             = 0x100,
		VBD_CACHE_TAG       = VBD_TAG | CACHE_TAG,
		FREE_TREE_TAG       = 0x200,
		FREE_TREE_TAG_IO    = FREE_TREE_TAG | IO_TAG,
		FREE_TREE_TAG_CACHE = FREE_TREE_TAG | CACHE_TAG,
		FREE_TREE_TAG_WB    = FREE_TREE_TAG | WRITE_BACK_TAG,
	};


	/*
	 * The Request is a loose reimagination of the Block::Request
	 * type and is used to seperate the C++ and SPARK even more, e.g.,
	 * it is packed whereas the Block::Request is not.
	 *
	 * (It stands to reason if this type is strictly necessary by now as
	 *  it also lacks certain operations like TRIM.)
	 */
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

			enum class Success : uint32_t { FALSE = 0, TRUE = 1 };

		private:

			Operation        _operation;
			Success          _success;
			uint64_t         _block_number;
			uint64_t         _offset;
			Number_of_blocks _count;
			uint32_t         _key_id;
			uint32_t         _tag;

		public:

			Request(Operation        operation,
			        Success          success,
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
				_success      { Success::FALSE },
				_block_number { 0 },
				_offset       { 0 },
				_count        { 0 },
				_key_id       { 0 },
				_tag          { 0 }
			{ }

			bool valid() const
			{
				switch (_operation) {
				case Operation::INVALID         : return false;
				case Operation::READ            : return true;
				case Operation::WRITE           : return true;
				case Operation::SYNC            : return true;
				case Operation::CREATE_SNAPSHOT : return true;
				case Operation::DISCARD_SNAPSHOT: return true;
				case Operation::REKEY           : return true;
				case Operation::EXTEND_VBD      : return true;
				case Operation::EXTEND_FT       : return true;
				case Operation::RESUME_REKEYING : return true;
				case Operation::DEINITIALIZE    : return true;
				case Operation::INITIALIZE      : return true;
				}
				return false;
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
			Success          success()      const { return _success; }
			uint64_t         block_number() const { return _block_number; }
			uint64_t         offset()       const { return _offset; }
			Number_of_blocks count()        const { return _count; }
			uint32_t         key_id()       const { return _key_id; }
			uint32_t         tag()          const { return _tag; }

			void success(Success arg) { _success = arg; }
			void tag(uint32_t arg)    { _tag = arg; }

	} __attribute__((packed));


	/*
	 * The Primitive is the primary data structure within the CBE
	 * and encapsulates a CBE operation.
	 */
	struct Primitive
	{
		using Number = uint64_t;
		using Index  = uint64_t;

		enum class Operation : uint32_t { INVALID, READ, WRITE, SYNC };
		enum class Success   : uint32_t { FALSE, TRUE };

		Tag tag;

		Operation operation;
		Success   success;

		Number block_number;
		Index  index;

		bool read()  const { return operation == Operation::READ; }
		bool write() const { return operation == Operation::WRITE; }
		bool sync()  const { return operation == Operation::SYNC; }

		bool valid() const
		{
			return operation == Operation::READ
			    || operation == Operation::WRITE
			    || operation == Operation::SYNC;
		}

		bool equal(Primitive const &rhs) const
		{
			return tag          == rhs.tag
			    && block_number == rhs.block_number
			    && operation    == rhs.operation;
		}

		/* debug */
		void print(Genode::Output &out) const
		{
			if (!valid()) {
				Genode::print(out, "<invalid>");
				return;
			}

			auto tag_string = [](Tag const tag) {
				switch (tag) {
				case Tag::INVALID_TAG: return "INVALID_TAG";
				case Tag::IO_TAG: return "IO_TAG";
				case Tag::CACHE_TAG: return "CACHE_TAG";
				case Tag::CACHE_FLUSH_TAG: return "CACHE_FLUSH_TAG";
				case Tag::CRYPTO_TAG: return "CRYPTO_TAG";
				case Tag::CRYPTO_TAG_DECRYPT: return "CRYPTO_TAG_DECRYPT";
				case Tag::CRYPTO_TAG_ENCRYPT: return "CRYPTO_TAG_ENCRYPT";
				case Tag::POOL_TAG: return "POOL_TAG";
				case Tag::SPLITTER_TAG: return "SPLITTER_TAG";
				case Tag::TRANSLATION_TAG: return "TRANSLATION_TAG";
				case Tag::WRITE_BACK_TAG: return "WRITE_BACK_TAG";
				case Tag::SYNC_SB_TAG: return "SYNC_SB_TAG";
				case Tag::RECLAIM_TAG: return "RECLAIM_TAG";

				case Tag::VBD_TAG: return "VBD_TAG";
				case Tag::VBD_CACHE_TAG: return "VBD_CACHE_TAG";
				case Tag::FREE_TREE_TAG: return "FREE_TREE_TAG";
				case Tag::FREE_TREE_TAG_IO: return "FREE_TREE_TAG_IO";
				case Tag::FREE_TREE_TAG_CACHE: return "FREE_TREE_TAG_CACHE";
				case Tag::FREE_TREE_TAG_WB: return "FREE_TREE_TAG_WB";
				}
				return "<invalid>";
			};
			Genode::print(out, "tag: ", tag_string(tag));

			Genode::print(out, " block_number: ", block_number);
			Genode::print(out, " index: ", index);
			Genode::print(out, " op: ");
			switch (operation) {
			case Operation::READ:  Genode::print(out, "READ"); break;
			case Operation::WRITE: Genode::print(out, "WRITE"); break;
			case Operation::SYNC:  Genode::print(out, "SYNC"); break;
			case Operation::INVALID: [[fallthrough]]
			default: break;
			}
			Genode::print(out, " success: ");
			switch (success) {
			case Success::FALSE: Genode::print(out, "no"); break;
			case Success::TRUE:  Genode::print(out, "yes"); break;
			}
		}
	} __attribute__((packed));

	/*
	 * The Tree_helper makes the information about used
	 * tree available.
	 */
	enum {
		TREE_MIN_DEGREE = 1,
		TREE_MIN_HEIGHT = 1,
		TREE_MAX_HEIGHT = 6,
	};
	struct Tree_helper
	{
		struct Object_size_mismatch { };

		static inline uint32_t _log2(uint32_t const value)
		{
			if (!value) { return -1; }

			for (int i = 8 * sizeof(value) - 1; i >= 0; --i) {
				if (((uint32_t)1 << i) & value) { return i; }
			}

			return -1; 
		}

		static constexpr Genode::uint32_t bytes() { return sizeof(Tree_helper); }

		Degree const _degree;
		Height const _height;
		Number_of_leaves const _leafs;

		Degree const _degree_log2 { _log2(_degree) };
		Degree const _degree_mask { (1u << _degree_log2) - 1 };

		Tree_helper(Degree           const degree,
		            Height           const height,
		            Number_of_leaves const leafs)
		: _degree(degree), _height(height), _leafs(leafs) { }

		uint32_t index(Virtual_block_address const vba,
		               uint32_t                   const level) const
		{
			return (vba >> (_degree_log2 * (level - 1)) & _degree_mask);
		}

		Height height()          const { return _height; }
		Degree degree()          const { return _degree; }
		Number_of_leaves leafs() const { return _leafs; }
	};


	/*
	 * The Block_data encapsulates the data of a complete on
	 * disk sector.
	 */
	struct Block_data
	{
		char values[BLOCK_SIZE];

		/* debug */
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


	/*
	 * The Hash contains the hash of a node.
	 */
	struct Hash
	{
		enum { MAX_LENGTH = 32, };
		char values[MAX_LENGTH];

		/* hash as hex value plus "0x" prefix and terminating null */
		using String = Genode::String<sizeof(values) * 2 + 3>;

		/* debug */
		void print(Genode::Output &out) const
		{
			using namespace Genode;
			Genode::print(out, "0x");
			bool leading_zero = true;
			for (char const c : values) {
				if (leading_zero) {
					if (c) {
						leading_zero = false;
						Genode::print(out, Hex(c, Hex::OMIT_PREFIX));
					}
				} else {
					Genode::print(out, Hex(c, Hex::OMIT_PREFIX, Hex::PAD));
				}
			}
			if (leading_zero) {
				Genode::print(out, "0");
			}
		}
	};


	/*
	 * The Key contains the key-material that is used to
	 * process cipher-blocks.
	 *
	 * (For now it is not used but the ID field is already referenced
	 *  by type 2 nodes.)
	 */
	struct Key
	{
		enum { KEY_SIZE = 32 };
		char value[KEY_SIZE];

		struct Id { uint32_t value; };
		Id id;

		using String = Genode::String<sizeof(value) * 2 + 3>;

		/* debug */
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


	/*
	 * The Snapshot stores the information about given tree within
	 * the CBE.
	 */
	struct Snapshot
	{
		struct /* Snapshot_info */{
			Hash                   hash;
			Physical_block_address pba;
			Generation             gen;
			Number_of_leaves       leaves;
			Height                 height;
			uint8_t                _valid;
		};
		uint32_t id;
		enum {
			FLAGS_CLEAR = 0u,
			FLAG_KEEP   = 1u<<0,
		};
		uint32_t flags;

		bool keep()  const { return flags & FLAG_KEEP; }

		bool valid() const
		{
			struct Bad_value : Exception { };
			if (_valid == 1) {
				return true;
			} else if (_valid == 0) {
				return false;
			} else {
				throw Bad_value();
			}
		}

		void valid(bool v)
		{
			if (v) {
				_valid = 1;
			} else {
				_valid = 0;
			}
		}

		/* debug */
		void print(Genode::Output &out) const
		{
			if (!valid()) {
				Genode::print(out, "<invalid>");
				return;
			}

			Genode::print(out, "id: ", id, " gen: ", gen,
			              " pba: ", pba, " leafs: ", leaves,
			              " height: ", height, " hash: <", hash, ">");
		}
	} __attribute__((packed));


	/*
	 * The Superblocks_index
	 *
	 * (It stands to reason if the type is needed.)
	 */
	struct Superblocks_index
	{
		uint64_t value;

		Superblocks_index(uint64_t val) : value(val) { }

		void print(Genode::Output &out) const
		{
			Genode::print(out, value);
		}
	};


	/*
	 * The Superblock contains all information of a CBE
	 * instance including the list of active snapshots. For now
	 * the super-blocks are stored consecutively at the beginning
	 * of the block device, i.e., there is a 1:1 mapping between
	 * the physical-block-address and the SB id.
	 *
	 * Per super-block we have a fixed number of snapshots (about
	 * the amount we can store within one disk sector). Whenever
	 * a generation is sealed, a new snapshot will be created
	 * automatically. If a snapshot is flagged as KEEP, it will never
	 * be overriden.
	 */
	enum : Snapshot_index        { NUM_SNAPSHOTS = 48 };
	enum : Number_of_superblocks { NUM_SUPER_BLOCKS = 8 };

	struct Superblock
	{
		enum { INVALID_SNAPSHOT_SLOT = NUM_SNAPSHOTS, };

		enum class State : uint8_t {
			NORMAL = 0,
			REKEYING = 1,
			EXTENDING_VBD = 2,
			EXTENDING_FT = 3};

		State                  state;
		Virtual_block_address  rekeying_vba;
		Number_of_blocks       resizing_nr_of_pbas;
		Number_of_blocks       resizing_nr_of_leaves;

		// XXX w/o snapshots about 265 bytes,
		//     snapshots about 68 bytes each, all in all 3529 bytes
		Key previous_key;
		Key current_key;

		/*
		 * (At the moment we just check the active snapshots of
		 *  the active super-block but should it not make sense
		 *  to iterate overall super-blocks when trying to determine
		 *  if a block may be safely freed? Because if the most
		 *  recent SB is corrupted and we try to use an older one,
		 *  chances are that the snapshot in the corrupt SB has
		 *  reused blocks reference by a snapshot in the older SB.)
		 */
		Snapshot               snapshots[NUM_SNAPSHOTS];
		Generation             last_secured_generation;
		Snapshot_index         curr_snap;
		Degree                 degree;
		Physical_block_address first_pba;
		Number_of_blocks       nr_of_pbas;
		Generation             free_gen;
		Physical_block_address free_number;
		Hash                   free_hash;
		Height                 free_height;
		Degree                 free_degree;
		Number_of_leaves       free_leaves;
		Generation             meta_gen;
		Physical_block_address meta_number;
		Hash                   meta_hash;
		Height                 meta_height;
		Degree                 meta_degree;
		Number_of_leaves       meta_leaves;
		char                   padding[383];

		void print(Genode::Output &out) const
		{
			Genode::print(out,
			              "current_key: ", current_key, " "
			              "previous_key: ", previous_key, " "
			              "last_sec_gen: ", last_secured_generation, " "
			              "curr_snap: ", curr_snap, " "
			              "snap: (gen: ", snapshots[curr_snap].gen, " "
			              "root: ",       snapshots[curr_snap].pba, " "
			              "height: ",     snapshots[curr_snap].height, " "
			              "dregree: ",    degree, " "
			              "leaves: ",     snapshots[curr_snap].leaves, ") ",
			              "FT: (",
			              free_gen, ", ",
			              free_number, ", ",
			              free_height, ", ",
			              free_degree, ", ",
			              free_leaves, ") ",
			              "MT: (",
			              meta_gen, ", ",
			              meta_number, ", ",
			              meta_height, ", ",
			              meta_degree, ", ",
			              meta_leaves, ")");
		}

		/**
		 * Get index into snapshot array for the last snapshot
		 *
		 * \return  if found the slot number is returned, otherwise
		 *          a invalid number
		 */
		Snapshot_index snapshot_slot() const
		{
			Snapshot_index snap_slot = INVALID_SNAPSHOT_SLOT;
			for (Snapshot_index i = 0; i < NUM_SNAPSHOTS; i++) {
				Snapshot const &snap = snapshots[i];
				if (!snap.valid()) { continue; }

				if (snap.id == curr_snap) {
					snap_slot = i;
					break;
				}
			}
			return snap_slot;
		}

		bool valid() const
		{
			return last_secured_generation != INVALID_GEN;
		}

	} __attribute__((packed));

	static_assert(sizeof(Superblock) == BLOCK_SIZE);


	struct Active_snapshot_ids
	{
		uint64_t values[NUM_SNAPSHOTS];
	} __attribute__((packed));

	struct Superblocks
	{
		Superblock block[NUM_SUPER_BLOCKS];

	} __attribute__((packed));

	static_assert(
		sizeof(Superblocks) == NUM_SUPER_BLOCKS * BLOCK_SIZE);

	/*
	 * (Strictly speaking the following node types are not the
	 * node itself but just defined a entry in the node. It
	 * would be better to have a Type_1_node that contains a
	 * Type_1_node_entry array.)
	 */

	/*
	 * The Type_i_node contains the on-disk type 1 inner node
	 * information. This node is the primary tree node and as such
	 * used by the virtual-block-device as well as the free-tree.
	 *
	 * In case of the VBD its leaf nodes point to the physical
	 * on disk sectors.
	 */
	struct Type_i_node
	{
		enum { MAX_NODE_SIZE = 64u, };

		Physical_block_address pba;
		Generation             gen;
		Hash                   hash;
		char                        padding[16];

	} __attribute__((packed));

	static_assert(sizeof(Type_i_node) == Type_i_node::MAX_NODE_SIZE);

	constexpr size_t TYPE_1_PER_BLOCK = BLOCK_SIZE / sizeof (Type_i_node);


	/*
	 * The Type_1_node_info contains the in-memory type 1 node
	 * information.
	 */
	struct Type_1_node_info
	{
		Physical_block_address pba;
		Generation             gen;
		Hash                   hash;
	};


	/*
	 * The Type_i_node contains the on-disk type 2 inner node
	 * information. This node is only used in the free-tree at the
	 * level directly above the leaf nodes.
	 */
	struct Type_ii_node
	{
		enum { MAX_NODE_SIZE = 64u, };

		Physical_block_address pba;
		Virtual_block_address  last_vba;
		Generation             alloc_gen;
		Generation             free_gen;
		Key::Id                last_key_id;
		bool                   reserved;
		char                   padding[27];

	} __attribute__((packed));

	static_assert(sizeof (Type_ii_node) == Type_ii_node::MAX_NODE_SIZE);

	constexpr size_t TYPE_2_PER_BLOCK = BLOCK_SIZE / sizeof (Type_ii_node);

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
		return Cbe::Request(
			convert_op(p.operation),
			Cbe::Request::Success::FALSE,
			p.block_number,
			0,
			1,
			0,
			0);
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
		auto convert_success = [&] (Cbe::Request::Success s) {
			return s == Cbe::Request::Success::TRUE ? true : false;
		};
		return Block::Request {
			.operation = {
				.type         = convert_op(r.operation()),
				.block_number = r.block_number(),
				.count        = r.count(),
			},
			.success   = convert_success(r.success()),
			.offset    = (Block::off_t)r.offset(),
			.tag       = { .value = r.tag() },
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

		return Cbe::Request(
			convert_op(r.operation.type),
			convert_success(r.success),
			r.operation.block_number,
			(Genode::uint64_t)r.offset,
			(Number_of_blocks)r.operation.count,
			0,
			(Genode::uint32_t)r.tag.value);
	}

} /* namespace Cbe */


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
	Genode::print(out, " success: ");
	switch (_success) {
	case Success::FALSE: Genode::print(out, "no"); break;
	case Success::TRUE:  Genode::print(out, "yes"); break;
	}
}

#endif /* _CBE_TYPES_H_ */
