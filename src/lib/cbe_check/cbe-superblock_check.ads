--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Superblock_Check
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialize_Object (Obj : out Object_Type);

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Execute (Obj : in out Object_Type);

   function Execute_Progress (Obj : Object_Type)
   return Boolean;

   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   function Peek_Generated_Max_Lvl_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type;

   function Peek_Generated_Max_Child_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Child_Index_Type;

   function Peek_Generated_Nr_Of_Leafs (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type;

   function Peek_Generated_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type;

   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Mark_Generated_Blk_IO_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type);

   procedure Mark_Generated_VBD_Check_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Mark_Generated_FT_Check_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      FT   :        Type_1_Node_Type);

private

   type SB_Slot_State_Type is (
      Init,
      Read_Started,
      Read_Dropped,
      Read_Done,
      VBD_Check_Started,
      VBD_Check_Dropped,
      VBD_Check_Done,
      FT_Check_Started,
      FT_Check_Dropped,
      FT_Check_Done,
      Done);

   type Object_Type is record
      Execute_Progress : Boolean;
      SB_Slot_State    : SB_Slot_State_Type;
      SB_Slot_Idx      : Superblocks_Index_Type;
      SB_Slot          : Superblock_Type;
      Snap_Idx         : Snapshots_Index_Type;
      VBD              : Type_1_Node_Type;
      FT               : Type_1_Node_Type;
      Generated_Prim   : Primitive.Object_Type;
      Submitted_Prim   : Primitive.Object_Type;
   end record;

end CBE.Superblock_Check;
