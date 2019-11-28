--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.VBD_Check
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialize_Object (Obj : out Object_Type);

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   procedure Submit_Primitive (
      Obj           : in out Object_Type;
      Prim          :        Primitive.Object_Type;
      Max_Lvl_Idx   :        Tree_Level_Index_Type;
      Max_Child_Idx :        Tree_Child_Index_Type;
      Nr_Of_Leafs   :        Tree_Number_Of_Leafs_Type;
      Root          :        Type_1_Node_Type);

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   function Peek_Completed_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type;

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

   procedure Mark_Generated_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type);

private

   type Child_State_Type is (Read_Block, Check_Hash, Done);

   type Children_State_Type is
      array (Type_1_Node_Block_Index_Type) of Child_State_Type;

   type Type_1_Level_Type is record
      Children       : Type_1_Node_Block_Type;
      Children_State : Children_State_Type;
   end record;

   subtype Type_1_Level_Index_Type is
      Tree_Level_Index_Type range
         Tree_Level_Index_Type'First + 1 .. Tree_Level_Index_Type'Last;

   type Type_1_Levels_Type
   is array (Type_1_Level_Index_Type) of Type_1_Level_Type;

   type Object_Type is record
      Root             : Type_1_Node_Type;
      Root_State       : Child_State_Type;
      T1_Lvls          : Type_1_Levels_Type;
      Leaf_Lvl         : Block_Data_Type;
      Max_Lvl_Idx      : Tree_Level_Index_Type;
      Max_Child_Idx    : Tree_Child_Index_Type;
      Nr_Of_Leafs      : Tree_Number_Of_Leafs_Type;
      Gen_Prim_Dropped : Boolean;
      Gen_Prim         : Primitive.Object_Type;
      Subm_Prim        : Primitive.Object_Type;
      Lvl_To_Read      : Tree_Level_Index_Type;
      Execute_Progress : Boolean;
   end record;

   function Hash_Of_Type_1_Node_Block (Block : Type_1_Node_Block_Type)
   return Hash_Type;

   function Hash_Of_Block_Data (CBE_Data : Block_Data_Type)
   return Hash_Type;

   procedure Execute_Leaf_Child (
      Progress     : in out Boolean;
      Prim         : in out Primitive.Object_Type;
      Prim_Dropped : in out Boolean;
      Lvl_To_Read  : in out Tree_Level_Index_Type;
      Child        :        Type_1_Node_Type;
      Child_Lvl    :        Block_Data_Type;
      Child_State  : in out Child_State_Type;
      Nr_Of_Leafs  : in out Tree_Number_Of_Leafs_Type;
      Lvl_Idx      :        Type_1_Level_Index_Type;
      Child_Idx    :        Type_1_Node_Block_Index_Type);

   procedure Execute_Inner_T1_Child (
      Progress     : in out Boolean;
      Prim         : in out Primitive.Object_Type;
      Prim_Dropped : in out Boolean;
      Lvl_To_Read  : in out Tree_Level_Index_Type;
      Child        :        Type_1_Node_Type;
      Child_Lvl    : in out Type_1_Level_Type;
      Child_State  : in out Child_State_Type;
      Nr_Of_Leafs  :        Tree_Number_Of_Leafs_Type;
      Lvl_Idx      :        Type_1_Level_Index_Type;
      Child_Idx    :        Type_1_Node_Block_Index_Type);

   function Max_T1_Lvl_Idx (Obj : Object_Type)
   return Type_1_Level_Index_Type
   is (Type_1_Level_Index_Type (Obj.Max_Lvl_Idx));

   function Max_T1_Child_Idx (Obj : Object_Type)
   return Type_1_Node_Block_Index_Type
   is (Type_1_Node_Block_Index_Type (Obj.Max_Child_Idx));

end CBE.VBD_Check;
