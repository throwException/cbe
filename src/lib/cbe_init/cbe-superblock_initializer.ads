--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Superblock_Initializer
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialize_Object (Obj : out Object_Type);

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   procedure Submit_Primitive (
      Obj             : in out Object_Type;
      Prim            :        Primitive.Object_Type;
      VBD_Max_Lvl_Idx :        Tree_Level_Index_Type;
      VBD_Degree      :        Tree_Degree_Type;
      VBD_Nr_Of_Leafs :        Tree_Number_Of_Leafs_Type;
      FT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      FT_Degree       :        Tree_Degree_Type;
      FT_Nr_Of_Leafs  :        Tree_Number_Of_Leafs_Type;
      MT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      MT_Degree       :        Tree_Degree_Type;
      MT_Nr_Of_Leafs  :        Tree_Number_Of_Leafs_Type);

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

   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   function Peek_Generated_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

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

   procedure Mark_Generated_Blk_IO_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Mark_Generated_VBD_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      VBD  :        Type_1_Node_Type);

   procedure Mark_Generated_FT_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      FT   :        Type_1_Node_Type);

   procedure Mark_Generated_MT_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      MT   :        Type_1_Node_Type);

private

   function Valid_Snap_Slot (Obj : Object_Type)
   return Snapshot_Type;

   function Valid_SB_Slot (Obj : Object_Type)
   return Superblock_Type;

   type SB_Slot_State_Type is (
      Init,
      VBD_Request_Started,
      VBD_Request_Dropped,
      VBD_Request_Done,
      FT_Request_Started,
      FT_Request_Dropped,
      FT_Request_Done,
      MT_Request_Started,
      MT_Request_Dropped,
      MT_Request_Done,
      Write_Request_Started,
      Write_Request_Dropped,
      Write_Request_Done,
      Done);

   type Object_Type is record
      Execute_Progress : Boolean;
      SB_Slot_State    : SB_Slot_State_Type;
      SB_Slot_Idx      : Superblocks_Index_Type;
      SB_Slot          : Superblock_Type;
      VBD              : Type_1_Node_Type;
      VBD_Max_Lvl_Idx  : Tree_Level_Index_Type;
      VBD_Degree       : Tree_Degree_Type;
      VBD_Nr_Of_Leafs  : Tree_Number_Of_Leafs_Type;
      FT               : Type_1_Node_Type;
      FT_Max_Lvl_Idx   : Tree_Level_Index_Type;
      FT_Degree        : Tree_Degree_Type;
      FT_Nr_Of_Leafs   : Tree_Number_Of_Leafs_Type;
      MT               : Type_1_Node_Type;
      MT_Max_Lvl_Idx   : Tree_Level_Index_Type;
      MT_Degree        : Tree_Degree_Type;
      MT_Nr_Of_Leafs   : Tree_Number_Of_Leafs_Type;
      Generated_Prim   : Primitive.Object_Type;
      Submitted_Prim   : Primitive.Object_Type;
   end record;

end CBE.Superblock_Initializer;
