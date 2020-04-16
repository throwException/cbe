--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with Interfaces;

package CBE.VBD_Rekeying
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Jobs : constant := 1;

   type Rekeying_Type is private;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   --
   --  Initialize_Rekeying
   --
   procedure Initialize_Rekeying (Rkg : out Rekeying_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Rkg : Rekeying_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      VBA              :        Virtual_Block_Address_Type;
      Snapshots        :        Snapshots_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      Old_Key_ID       :        Key_ID_Type;
      New_Key_ID       :        Key_ID_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Rkg      : in out Rekeying_Type;
      Progress : in out Boolean);

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Blk_IO_Primitive
   --
   function Peek_Generated_Blk_IO_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Completed
   --
   procedure Mark_Generated_Prim_Completed (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Completed_Blk_Data
   --
   procedure Mark_Generated_Prim_Completed_Blk_Data (
      Rkg      : in out Rekeying_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type);

private

   type Job_Operation_Type is (
      Invalid,
      Rekey_VBA
   );

   type Job_State_Type is (
      Submitted,
      Read_Root_Node_Pending,
      Read_Root_Node_In_Progress,
      Read_Root_Node_Completed,
      Read_Inner_Node_Pending,
      Read_Inner_Node_In_Progress,
      Read_Inner_Node_Completed,
      Read_Leaf_Node_Pending,
      Read_Leaf_Node_In_Progress,
      Read_Leaf_Node_Completed,
      Decrypt_Leaf_Node_Pending,
      Decrypt_Leaf_Node_In_Progress,
      Decrypt_Leaf_Node_Completed,
      Alloc_New_Leaf_Node_PBA_Pending,
      Alloc_New_Leaf_Node_PBA_In_Progress,
      Alloc_New_Leaf_Node_PBA_Completed,
      Encrypt_Leaf_Node_Pending,
      Encrypt_Leaf_Node_In_Progress,
      Encrypt_Leaf_Node_Completed,
      Write_Leaf_Node_Pending,
      Write_Leaf_Node_In_Progress,
      Write_Leaf_Node_Completed,
      Alloc_New_Inner_Node_PBA_Pending,
      Alloc_New_Inner_Node_PBA_In_Progress,
      Alloc_New_Inner_Node_PBA_Completed,
      Write_Inner_Node_Pending,
      Write_Inner_Node_In_Progress,
      Write_Inner_Node_Completed,
      Write_Root_Node_Pending,
      Write_Root_Node_In_Progress,
      Write_Root_Node_Completed,
      Completed
   );

   subtype Type_1_Node_Blocks_Index_Type is
      Tree_Level_Index_Type range
         Tree_Level_Index_Type'First + 1 .. Tree_Level_Index_Type'Last;

   type Type_1_Node_Blocks_Type
   is array (Type_1_Node_Blocks_Index_Type) of Type_1_Node_Block_Type;

   type Type_1_Node_Blocks_PBAs_Type
   is array (Type_1_Node_Blocks_Index_Type) of Physical_Block_Address_Type;

   type Job_Type is record
      Operation         : Job_Operation_Type;
      State             : Job_State_Type;
      Submitted_Prim    : Primitive.Object_Type;
      Generated_Prim    : Primitive.Object_Type;
      Snapshots         : Snapshots_Type;
      Snapshots_Degree  : Tree_Degree_Type;
      Snapshot_Idx      : Snapshots_Index_Type;
      First_Snapshot    : Boolean;
      Old_Key_ID        : Key_ID_Type;
      New_Key_ID        : Key_ID_Type;
      T1_Blks           : Type_1_Node_Blocks_Type;
      T1_Blks_Old_PBAs  : Type_1_Node_Blocks_PBAs_Type;
      T1_Blk_Idx        : Type_1_Node_Blocks_Index_Type;
      Data_Blk          : Block_Data_Type;
      Data_Blk_Old_PBA  : Physical_Block_Address_Type;
      VBA               : Virtual_Block_Address_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Rekeying_Type is record
      Jobs : Jobs_Type;
   end record;

   --
   --  Execute_Rekey_VBA
   --
   procedure Execute_Rekey_VBA (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Child_Idx_For_VBA
   --
   function Child_Idx_For_VBA (
      VBA  : Virtual_Block_Address_Type;
      Lvl  : Type_1_Node_Blocks_Index_Type;
      Degr : Tree_Degree_Type)
   return Type_1_Node_Block_Index_Type;

   --
   --  Execute_Rekey_VBA_Read_Node_Completed
   --
   procedure Execute_Rekey_VBA_Read_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Hash     :        Hash_Type;
      Progress : in out Boolean);

   --
   --  Log_2
   --
   function  Log_2 (Value : Interfaces.Unsigned_32)
   return Interfaces.Unsigned_32;

end CBE.VBD_Rekeying;
