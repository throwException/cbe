--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with Interfaces;

package CBE
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Superblock_Slots : constant := 8;
   Max_Number_Of_Requests_In_Pool : constant := 16;
   Superblock_Nr_Of_Snapshots : constant := 48;
   Block_Size_Bytes : constant := 4096;
   Type_1_Node_Storage_Size_Bytes : constant := 64;
   Type_2_Node_Storage_Size_Bytes : constant := 64;
   Hash_Size_Bytes : constant := 32;
   Key_Value_Size_Bytes : constant := 32;
   Key_Storage_Size_Bytes : constant := Key_Value_Size_Bytes + 4;
   Snapshot_Storage_Size_Bytes : constant := 72;
   Tree_Min_Degree_Log_2 : constant := 0;
   Tree_Max_Degree_Log_2 : constant := 6;
   Tree_Max_Max_Level : constant := 6;
   Tree_Min_Max_Level : constant := 1;
   Free_Tree_Min_Max_Level : constant := 2;

   Block_Size : constant := Block_Size_Bytes * 8;
   Tree_Min_Degree : constant := 2**Tree_Min_Degree_Log_2;
   Tree_Max_Degree : constant := 2**Tree_Max_Degree_Log_2;

   Type_1_Node_Storage_Size : constant := Type_1_Node_Storage_Size_Bytes * 8;
   Type_1_Nodes_Per_Block : constant :=
      Block_Size_Bytes / Type_1_Node_Storage_Size_Bytes;

   Type_2_Node_Storage_Size : constant := Type_2_Node_Storage_Size_Bytes * 8;
   Type_2_Nodes_Per_Block : constant :=
      Block_Size_Bytes / Type_2_Node_Storage_Size_Bytes;

   Superblock_Snapshots_Storage_Size_Bytes : constant :=
      Superblock_Nr_Of_Snapshots * Snapshot_Storage_Size_Bytes;

   Tree_Max_Number_Of_Leafs : constant :=
      Tree_Max_Degree**(Tree_Max_Max_Level - 1);

   type Byte_Type is range 0 .. 2**8 - 1 with Size => 8;
   type Block_Data_Index_Type is range 0 .. Block_Size_Bytes - 1;

   type Block_Data_Type
   is array (Block_Data_Index_Type) of Byte_Type with Size => Block_Size;

   type Translation_Data_Type is array (0 .. 0) of Block_Data_Type;
   type Number_Of_Primitives_Type is range 0 .. Tree_Max_Number_Of_Leafs;

   type Index_Type is range 0 .. 2**32 - 1;
   type Index_Slot_Type is private;

   type Generation_Type is mod 2**64;
   type Block_Number_Type is mod 2**64;
   type Physical_Block_Address_Type is mod 2**64;
   type Token_Type is mod 2**64;

   type Virtual_Block_Address_Type is range 0 .. Tree_Max_Number_Of_Leafs;
   type Timestamp_Type is mod 2**64;

   type Tree_Level_Index_Type is range 0 .. Tree_Max_Max_Level;

   type Tree_Number_Of_Leafs_Type
   is range 0 .. Tree_Max_Number_Of_Leafs;

   type Tree_Degree_Type is range Tree_Min_Degree .. Tree_Max_Degree;
   type Tree_Degree_Log_2_Type
   is range Tree_Min_Degree_Log_2 .. Tree_Max_Degree_Log_2;

   type Tree_Degree_Mask_Type
   is range 2**Tree_Min_Degree_Log_2 - 1 .. 2**Tree_Max_Degree_Log_2 - 1;

   type Tree_Child_Index_Type is range 0 .. Tree_Max_Degree - 1;
   type Number_Of_Blocks_Type is mod 2**64;
   type Number_Of_Requests_Type is range 0 .. 2**32 - 1;
   type Snapshot_ID_Type is range 0 .. 2**32 - 1;
   type Key_ID_Type is range 0 .. 2**32 - 1;

   type Request_Operation_Type is (
      Initialize,
      Deinitialize,
      Read,
      Write,
      Sync,
      Create_Snapshot,
      Discard_Snapshot,
      Rekey,
      Extend_VBD,
      Extend_FT,
      Resume_Rekeying);

   type Primitive_Operation_Type is (Read, Write, Sync);

   type Hash_Index_Type is range 0 .. Hash_Size_Bytes - 1;
   type Hash_Type is array (Hash_Index_Type) of Byte_Type;

   type Tree_Geometry_Type is record
      Max_Level : Tree_Level_Index_Type;
      Edges     : Tree_Degree_Type;
      Leafs     : Tree_Number_Of_Leafs_Type;
   end record;

   type Type_1_Node_Type is record
      PBA      : Physical_Block_Address_Type;
      Gen      : Generation_Type;
      Hash     : Hash_Type;
   end record;

   subtype Type_1_Node_Block_Index_Type is
      Standard.Integer range 0 .. Type_1_Nodes_Per_Block - 1;

   type Type_1_Node_Block_Type
   is array (Type_1_Node_Block_Index_Type) of Type_1_Node_Type;

   --
   --  The CBE::Type_i_node contains the on-disk type 2 inner node
   --  information. This node is only used in the free-tree at the
   --  level directly above the leaf nodes.
   --
   type Type_2_Node_Type is record
      PBA         : Physical_Block_Address_Type;
      Last_VBA    : Virtual_Block_Address_Type;
      Alloc_Gen   : Generation_Type;
      Free_Gen    : Generation_Type;
      Last_Key_ID : Key_ID_Type;
      Reserved    : Boolean;
   end record;

   subtype Type_2_Node_Block_Index_Type is
      Standard.Integer range 0 .. Type_2_Nodes_Per_Block - 1;

   type Type_2_Node_Block_Type
   is array (Type_2_Node_Block_Index_Type) of Type_2_Node_Type;

   type Query_Data_Type is array (0 .. 0) of Block_Data_Type;

   --
   --  The CBE::Snapshot stores the information about a given tree within
   --  the CBE.
   --
   type Snapshot_Type is record
      Hash        : Hash_Type;
      PBA         : Physical_Block_Address_Type;
      Gen         : Generation_Type;
      Nr_Of_Leafs : Tree_Number_Of_Leafs_Type;
      Max_Level   : Tree_Level_Index_Type;
      Valid       : Boolean;
      ID          : Snapshot_ID_Type;
      Keep        : Boolean;
   end record;

   type Snapshots_Index_Type is range 0 .. Superblock_Nr_Of_Snapshots - 1;
   type Snapshots_Type is array (Snapshots_Index_Type) of Snapshot_Type;

   type Active_Snapshot_IDs_Type
   is array (Snapshots_Index_Type) of Generation_Type;

   type Type_1_Node_Walk_Type
   is array (Tree_Level_Index_Type) of Type_1_Node_Type;

   type Dump_Cfg_Max_Superblocks_Type is range 0 .. 2**32 - 1;
   type Dump_Cfg_Max_Snapshots_Type is range 0 .. 2**32 - 1;

   type Dump_Configuration_Type is record
      Unused_Nodes           : Boolean;
      Max_Superblocks        : Dump_Cfg_Max_Superblocks_Type;
      Max_Snapshots          : Dump_Cfg_Max_Snapshots_Type;
      VBD                    : Boolean;
      VBD_PBA_Filter_Enabled : Boolean;
      VBD_PBA_Filter         : Physical_Block_Address_Type;
      VBD_VBA_Filter_Enabled : Boolean;
      VBD_VBA_Filter         : Virtual_Block_Address_Type;
      Free_Tree              : Boolean;
      Meta_Tree              : Boolean;
      Hashes                 : Boolean;
   end record;

   function Dump_Configuration_Default
   return Dump_Configuration_Type
   is (
      Unused_Nodes => True,
      Max_Superblocks => Dump_Cfg_Max_Superblocks_Type'Last,
      Max_Snapshots => Dump_Cfg_Max_Snapshots_Type'Last,
      VBD => True,
      VBD_PBA_Filter_Enabled => False,
      VBD_PBA_Filter => 0,
      VBD_VBA_Filter_Enabled => False,
      VBD_VBA_Filter => 0,
      Free_Tree => True,
      Meta_Tree => True,
      Hashes => True);

   function Type_1_Node_Valid (Node : Type_1_Node_Type)
   return Boolean
   is (Node.PBA /= 0);

   function Type_2_Node_Valid (Node : Type_2_Node_Type)
   return Boolean
   is (Node.PBA /= 0);

   function Type_1_Node_Invalid
   return Type_1_Node_Type
   is (
      PBA  => 0,
      Gen  => 0,
      Hash => (others => 0));

   function Type_2_Node_Invalid return Type_2_Node_Type
   is (
      PBA         => 0,
      Last_VBA    => 0,
      Alloc_Gen   => 0,
      Free_Gen    => 0,
      Last_Key_ID => 0,
      Reserved    => False);

   function Initial_Generation return Generation_Type
   is (0);

   function VBA_Invalid return Virtual_Block_Address_Type
   is (Virtual_Block_Address_Type'Last);

   function PBA_Invalid return Physical_Block_Address_Type
   is (Physical_Block_Address_Type'Last);

   function Snapshot_Invalid
   return Snapshot_Type
   is (
      Hash        => (others => 0),
      PBA         => PBA_Invalid,
      Gen         => Generation_Type'Last,
      Nr_Of_Leafs => Tree_Number_Of_Leafs_Type'Last,
      Max_Level   => Tree_Level_Index_Type'Last,
      Valid       => False,
      ID          => Snapshot_ID_Type'Last,
      Keep        => False);

   type Key_Value_Index_Type is range 0 .. Key_Value_Size_Bytes - 1;
   type Key_Value_Plaintext_Type is array (Key_Value_Index_Type) of Byte_Type;
   type Key_Value_Ciphertext_Type is array (Key_Value_Index_Type) of Byte_Type;

   --
   --  The CBE::Key contains the key-material that is used to
   --  process cipher-blocks.
   --
   --  (For now it is not used but the ID field is already referenced
   --  by type 2 nodes.)
   --
   type Key_Plaintext_Type is record
      Value : Key_Value_Plaintext_Type;
      ID    : Key_ID_Type;
   end record;

   type Key_Ciphertext_Type is record
      Value : Key_Value_Ciphertext_Type;
      ID    : Key_ID_Type;
   end record;

   --
   --  Key_Ciphertext_Invalid
   --
   function Key_Ciphertext_Invalid
   return Key_Ciphertext_Type;

   --
   --  Key_ID_Invalid
   --
   function Key_ID_Invalid
   return Key_ID_Type
   is (Key_ID_Type'First);

   --
   --  Key_Plaintext_Invalid
   --
   function Key_Plaintext_Invalid
   return Key_Plaintext_Type;

   --
   --  Key_Plaintext_Valid
   --
   function Key_Plaintext_Valid (ID : Key_ID_Type)
   return Key_Plaintext_Type;

   type Superblock_State_Type is (
      Invalid,
      Normal,
      Rekeying,
      Extending_VBD,
      Extending_FT);

   type Superblock_Type is record
      State                   : Superblock_State_Type;
      Rekeying_VBA            : Virtual_Block_Address_Type;
      Resizing_Nr_Of_PBAs     : Number_Of_Blocks_Type;
      Resizing_Nr_Of_Leaves   : Tree_Number_Of_Leafs_Type;
      Previous_Key            : Key_Plaintext_Type;
      Current_Key             : Key_Plaintext_Type;
      Snapshots               : Snapshots_Type;
      Last_Secured_Generation : Generation_Type;
      Curr_Snap               : Snapshots_Index_Type;
      Degree                  : Tree_Degree_Type;
      First_PBA               : Physical_Block_Address_Type;
      Nr_Of_PBAs              : Number_Of_Blocks_Type;
      Free_Gen                : Generation_Type;
      Free_Number             : Physical_Block_Address_Type;
      Free_Hash               : Hash_Type;
      Free_Max_Level          : Tree_Level_Index_Type;
      Free_Degree             : Tree_Degree_Type;
      Free_Leafs              : Tree_Number_Of_Leafs_Type;
      Meta_Gen                : Generation_Type;
      Meta_Number             : Physical_Block_Address_Type;
      Meta_Hash               : Hash_Type;
      Meta_Max_Level          : Tree_Level_Index_Type;
      Meta_Degree             : Tree_Degree_Type;
      Meta_Leafs              : Tree_Number_Of_Leafs_Type;
   end record;

   type Superblock_Ciphertext_Type is record
      State                   : Superblock_State_Type;
      Rekeying_VBA            : Virtual_Block_Address_Type;
      Resizing_Nr_Of_PBAs     : Number_Of_Blocks_Type;
      Resizing_Nr_Of_Leaves   : Tree_Number_Of_Leafs_Type;
      Previous_Key            : Key_Ciphertext_Type;
      Current_Key             : Key_Ciphertext_Type;
      Snapshots               : Snapshots_Type;
      Last_Secured_Generation : Generation_Type;
      Curr_Snap               : Snapshots_Index_Type;
      Degree                  : Tree_Degree_Type;
      First_PBA               : Physical_Block_Address_Type;
      Nr_Of_PBAs              : Number_Of_Blocks_Type;
      Free_Gen                : Generation_Type;
      Free_Number             : Physical_Block_Address_Type;
      Free_Hash               : Hash_Type;
      Free_Max_Level          : Tree_Level_Index_Type;
      Free_Degree             : Tree_Degree_Type;
      Free_Leafs              : Tree_Number_Of_Leafs_Type;
      Meta_Gen                : Generation_Type;
      Meta_Number             : Physical_Block_Address_Type;
      Meta_Hash               : Hash_Type;
      Meta_Max_Level          : Tree_Level_Index_Type;
      Meta_Degree             : Tree_Degree_Type;
      Meta_Leafs              : Tree_Number_Of_Leafs_Type;
   end record;

   --
   --  Superblock_Ciphertext_Invalid
   --
   function Superblock_Ciphertext_Invalid
   return Superblock_Ciphertext_Type;

   --
   --  Superblock_Ciphertext_Valid
   --
   function Superblock_Ciphertext_Valid (SB : Superblock_Ciphertext_Type)
   return Boolean;

   --
   --  Superblock_Invalid
   --
   function Superblock_Invalid
   return Superblock_Type;

   --
   --  Superblock_Valid
   --
   function Superblock_Valid (SB : Superblock_Type)
   return Boolean;

   type Superblocks_Index_Type is range 0 .. Nr_Of_Superblock_Slots - 1;
   type Superblocks_Type is array (Superblocks_Index_Type) of Superblock_Type;
   type Superblocks_Ciphertext_Type is array (Superblocks_Index_Type)
      of Superblock_Ciphertext_Type;

   type Info_Type is record
      Valid         : Boolean;
      Rekeying      : Boolean;
      Extending_FT  : Boolean;
      Extending_VBD : Boolean;
   end record;

   type Timeout_Request_Type is record
      Valid   : Boolean;
      Timeout : Timestamp_Type;
   end record;

   procedure Block_Data_From_Type_2_Node_Block (
      Data  : out Block_Data_Type;
      Nodes :     Type_2_Node_Block_Type);

   procedure Type_2_Node_Block_From_Block_Data (
      Nodes : out Type_2_Node_Block_Type;
      Data  :     Block_Data_Type);

   function Type_2_Node_XML_Tag (
      Node     : Type_2_Node_Type;
      Node_Idx : Type_2_Node_Block_Index_Type)
   return String;

   function Type_1_Node_XML_Attributes (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String;

   function Type_1_Node_XML_Tag (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String;

   function Type_1_Node_XML_Tag_Open (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String;

   function Type_1_Node_XML_Tag_Close
   return String;

   function Superblock_XML_Tag_Open (SB : Superblock_Type)
   return String;

   function Superblock_XML_Tag_Invalid
   return String;

   function Superblock_XML_Tag_Close
   return String;

   function Snap_XML_Tag_Invalid (Snap_Idx : Snapshots_Index_Type)
   return String;

   function Snap_XML_Tag_Open (
      Snap      : Snapshot_Type;
      Snap_Idx  : Snapshots_Index_Type;
      Show_Hash : Boolean)
   return String;

   function Snap_XML_Tag_Close
   return String;

   function Free_Tree_XML_Tag_Open (
      SB        : Superblock_Type;
      Show_Hash : Boolean)
   return String;

   function Free_Tree_XML_Tag_Close
   return String;

   function Meta_Tree_XML_Tag_Open (
      SB        : Superblock_Type;
      Show_Hash : Boolean)
   return String;

   function Meta_Tree_XML_Tag_Close
   return String;

   procedure Block_Data_From_Type_1_Node_Block (
      Data  : out Block_Data_Type;
      Nodes :     Type_1_Node_Block_Type);

   procedure Type_1_Node_Block_From_Block_Data (
      Nodes : out Type_1_Node_Block_Type;
      Data  :     Block_Data_Type);

   --
   --  Block_Data_From_Superblock
   --
   procedure Block_Data_From_Superblock (
      Data  : out Block_Data_Type;
      SB    :     Superblock_Type);

   --
   --  Block_Data_From_Superblock_Ciphertext
   --
   procedure Block_Data_From_Superblock_Ciphertext (
      Data  : out Block_Data_Type;
      SB    :     Superblock_Ciphertext_Type);

   type Pool_Index_Type is range 1 .. Max_Number_Of_Requests_In_Pool;
   type Pool_Index_Slot_Type is private;

   function Pool_Idx_Slot_Valid (Slot : Pool_Index_Slot_Type)
   return Boolean;

   function Pool_Idx_Slot_Content (Slot : Pool_Index_Slot_Type)
   return Pool_Index_Type;

   function Pool_Idx_Slot_Valid (Cont : Pool_Index_Type)
   return Pool_Index_Slot_Type;

   function Pool_Idx_Slot_Invalid
   return Pool_Index_Slot_Type;

   function Idx_Slot_Valid (Slot : Index_Slot_Type)
   return Boolean;

   function Idx_Slot_Content (Slot : Index_Slot_Type)
   return Index_Type;

   function Idx_Slot_Valid (Cont : Index_Type)
   return Index_Slot_Type;

   function Idx_Slot_Invalid
   return Index_Slot_Type;

   --
   --  Superblock_From_Block_Data
   --
   procedure Superblock_From_Block_Data (
      SB   : out Superblock_Type;
      Data :     Block_Data_Type);

   --
   --  Superblock_Ciphertext_From_Block_Data
   --
   procedure Superblock_Ciphertext_From_Block_Data (
      SB   : out Superblock_Ciphertext_Type;
      Data :     Block_Data_Type);

   generic
      type T is (<>);
   function Advance_Index (I : T) return T;

   --
   --  Newest_Snapshot_Idx
   --
   function Newest_Snapshot_Idx (Snapshots : Snapshots_Type)
   return Snapshots_Index_Type;

private

   procedure Snapshot_From_Block_Data (
      Snap     : out Snapshot_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type);

   procedure Snapshots_From_Block_Data (
      Snaps    : out Snapshots_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type);

   procedure Key_Plaintext_From_Block_Data (
      Key      : out Key_Plaintext_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type);

   --
   --  Key_Ciphertext_From_Block_Data
   --
   procedure Key_Ciphertext_From_Block_Data (
      Key      : out Key_Ciphertext_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type);

   procedure Block_Data_From_Unsigned_64 (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Int  :        Interfaces.Unsigned_64);

   procedure Block_Data_From_Unsigned_32 (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Int  :        Interfaces.Unsigned_32);

   procedure Block_Data_From_Boolean (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Bool :        Boolean);

   procedure Block_Data_Zero_Fill (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Size :        Block_Data_Index_Type);

   procedure Block_Data_From_Hash (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Hash   :        Hash_Type);

   procedure Block_Data_From_Type_1_Node (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Node   :        Type_1_Node_Type);

   procedure Block_Data_From_Type_2_Node (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Node   :        Type_2_Node_Type);

   function Boolean_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Boolean;

   function Unsigned_32_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Interfaces.Unsigned_32;

   function Unsigned_64_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Interfaces.Unsigned_64;

   function Hash_From_Block_Data (
      Data : Block_Data_Type;
      Base : Block_Data_Index_Type)
   return Hash_Type;

   procedure Block_Data_From_Key_Plaintext (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Key      :        Key_Plaintext_Type);

   --
   --  Block_Data_From_Key_Ciphertext
   --
   procedure Block_Data_From_Key_Ciphertext (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Key      :        Key_Ciphertext_Type);

   procedure Block_Data_From_Snapshot (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Snap     :        Snapshot_Type);

   procedure Block_Data_From_Snapshots (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Snaps    :        Snapshots_Type);

   type Pool_Index_Slot_Type is record
      Valid   : Boolean;
      Content : Pool_Index_Type;
   end record;

   type Index_Slot_Type is record
      Valid   : Boolean;
      Content : Index_Type;
   end record;

   --
   --  Prim_Op_To_Req_Op
   --
   function Prim_Op_To_Req_Op (Input : Primitive_Operation_Type)
   return Request_Operation_Type
   is (
      case Input is
      when Read => Read,
      when Write => Write,
      when Sync => Sync);

   --
   --  Prim_Op_From_Request_Op
   --
   function Prim_Op_From_Req_Op (Input : Request_Operation_Type)
   return Primitive_Operation_Type;

   --
   --  SB_State_From_Block_Data
   --
   function SB_State_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Superblock_State_Type;

   --
   --  Block_Data_From_SB_State
   --
   procedure Block_Data_From_SB_State (
      Data  : in out Block_Data_Type;
      Off   :        Block_Data_Index_Type;
      State :        Superblock_State_Type);

end CBE;
