--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Tree_Helper;
with CBE.Translation;

package CBE.Virtual_Block_Device
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Trans_Inhibit_Translation
   --
   procedure Trans_Inhibit_Translation (Obj : in out Object_Type);

   --
   --  Trans_Resume_Translation
   --
   procedure Trans_Resume_Translation (Obj : in out Object_Type);

   --
   --  Trans_Get_Virtual_Block_Address
   --
   function Trans_Get_Virtual_Block_Address (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Number_Type;

   --
   --  Trans_Can_Get_Type_1_Node_Walk
   --
   function Trans_Can_Get_Type_1_Node_Walk (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean;

   --
   --  Trans_Get_Type_1_Node_Walk
   --
   procedure Trans_Get_Type_1_Node_Walk (
      Obj  :        Object_Type;
      Walk : in out Type_1_Node_Walk_Type);

   --
   --  Tree_Max_Level
   --
   function Tree_Max_Level (Obj : Object_Type)
   return Tree_Level_Index_Type;

   --
   --  Index_For_Level
   --
   function Index_For_Level (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Index_Type)
   return Tree_Child_Index_Type;

   --
   --  Get_Tree_Helper
   --
   function Get_Tree_Helper (Obj : Object_Type)
   return Tree_Helper.Object_Type;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj             : in out Object_Type;
      PBA             :        Physical_Block_Address_Type;
      Gen             :        Generation_Type;
      Hash            :        Hash_Type;
      Max_Level       :        Tree_Level_Index_Type;
      Degree          :        Tree_Degree_Type;
      Leafs           :        Tree_Number_Of_Leafs_Type;
      Prim            :        Primitive.Object_Type;
      Rekeying        :        Boolean;
      Rekeying_VBA    :        Virtual_Block_Address_Type;
      Previous_Key_ID :        Key_ID_Type;
      Current_Key_ID  :        Key_ID_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   function Peek_Completed_Hash (Obj : Object_Type)
   return Hash_Type;

   function Peek_Completed_Generation (Obj : Object_Type)
   return Generation_Type;

   --
   --  Peek_Completed_Key_ID
   --
   function Peek_Completed_Key_ID (Obj : Object_Type)
   return Key_ID_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj        : in out Object_Type;
      Trans_Data : in out Translation_Data_Type);

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Cache_Lvl
   --
   function Peek_Generated_Cache_Lvl (Obj : Object_Type)
   return Tree_Level_Index_Type;

   --
   --  Peek_Generated_Cache_Data
   --
   function Peek_Generated_Cache_Data (Obj : Object_Type)
   return Block_Data_Type;

   --
   --  Drop_Generated_Cache_Primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Cache_Primitive_Complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type);

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean;

   function To_String (Obj : Object_Type) return String;

private

   type Cache_Primitive_State_Type is (Invalid, Generated, Dropped, Complete);

   type Object_Type is record
      Trans_Helper     : Tree_Helper.Object_Type;
      Trans            : Translation.Object_Type;
      Execute_Progress : Boolean;
      Cache_Prim       : Primitive.Object_Type;
      Cache_Prim_State : Cache_Primitive_State_Type;
      Cache_Prim_Data  : Block_Data_Type;
      Key_ID           : Key_ID_Type;
   end record;

end CBE.Virtual_Block_Device;
