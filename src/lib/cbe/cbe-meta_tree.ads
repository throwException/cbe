--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;
with CBE.Primitive;

--
--  The meta-tree module handles the allocation and freeing of meta-data
--  free-tree inner nodes.
--
package CBE.Meta_Tree
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialized_Object (Obj : out Object_Type);

   ------------------------
   --  Module interface  --
   ------------------------

   --
   --  Check if the module can accept a new request
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit new request
   --
   procedure Submit_Request (
      Obj         : in out Object_Type;
      Root_Node   :        Type_1_Node_Type;
      Tree_Geom   :        Tree_Geometry_Type;
      Current_Gen :        Generation_Type;
      Old_PBA     :        Physical_Block_Address_Type);

   --
   --  Execute module
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Check for any completed primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get the current root node
   --
   function Peek_Completed_Root_Node (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type;

   --
   --  Discard given completed primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Get generated meta-data tree primitive
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get generated cache data
   --
   function Peek_Generated_Cache_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Discard generated meta-data tree primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark generated meta-data tree primitive as complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj        : in out Object_Type;
      Prim       :        Primitive.Object_Type;
      Block_Data : in     Block_Data_Type);

   -----------------
   --  Accessors  --
   -----------------

   --  function To_String (Obj : Object_Type) return String;

private

   function Node_Volatile (
      T   : Type_1_Node_Type;
      Gen : Generation_Type)
   return Boolean is (T.Gen = 0 or else T.Gen /= Gen);

   type Node_Index_Type is new Byte_Type;

   function Node_Index_Invalid return Node_Index_Type
   is (Node_Index_Type'Last);

   type Info_State_Type is (
      Invalid,
      Read,
      Read_Complete,
      Write,
      Write_Complete,
      Complete);

   function To_String (S : Info_State_Type) return String;

   type Type_1_Info_Type is record
      State    : Info_State_Type;
      Node     : Type_1_Node_Type;
      Entries  : Type_1_Node_Block_Type;
      Index    : Node_Index_Type;
      Dirty    : Boolean;
      Volatile : Boolean;
   end record;

   function Type_1_Info_Invalid return Type_1_Info_Type
   is (
      State    => Invalid,
      Node     => Type_1_Node_Invalid,
      Entries  => (others => Type_1_Node_Invalid),
      Index    => Node_Index_Invalid,
      Dirty    => False,
      Volatile => False);

   function To_String (T : Type_1_Node_Type) return String;

   function To_String (T : Type_1_Info_Type) return String;

   type Type_2_Info_Type is record
      State    : Info_State_Type;
      Node     : Type_1_Node_Type;
      Entries  : Type_2_Node_Block_Type;
      Index    : Node_Index_Type;
      Volatile : Boolean;
   end record;

   function Type_2_Info_Invalid return Type_2_Info_Type
   is (
      State    => Invalid,
      Node     => Type_1_Node_Invalid,
      Entries  => (others => Type_2_Node_Invalid),
      Index    => Node_Index_Invalid,
      Volatile => False);

   function To_String (T : Type_2_Info_Type) return String;

   type State_Type is (
      Invalid,
      Update,
      Complete,
      Tree_Hash_Mismatch);

   function To_String (S : State_Type) return String;

   Type_2_Level : constant := 1;

   subtype Type_1_Info_Array_Index_Type is Tree_Level_Index_Type'Base
      range 2 .. Tree_Level_Index_Type'Last;

   type Type_1_Info_Array_Type is array
      (Type_1_Info_Array_Index_Type) of
         Type_1_Info_Type;

   procedure Initialize_Type_1_Info_Array (
      A : out Type_1_Info_Array_Type);

   type Cache_Primitive_State_Type is (
      Invalid, Pending, In_Progress);

   type Cache_Request_Type is record
      State      : Cache_Primitive_State_Type;
      Prim       : Primitive.Object_Type;
      Level      : Tree_Level_Index_Type;
      Block_Data : Block_Data_Type;
   end record;

   function Invalid_Cache_Request return Cache_Request_Type
   is (
      State      => Invalid,
      Prim       => Primitive.Invalid_Object,
      Level      => 0,
      Block_Data => (others => 0));

   function New_Cache_Request (
      PBA        : Physical_Block_Address_Type;
      Op         : Primitive_Operation_Type;
      Level      : Tree_Level_Index_Type;
      Block_Data : Block_Data_Type)
   return Cache_Request_Type
   is (
      State      => Pending,
      Prim       => Primitive.Valid_Object_No_Pool_Idx (
         Op     => Op,
         Succ   => Request.Success_Type (False),
         Tg     => Primitive.Tag_MT_Cache,
         Blk_Nr => Block_Number_Type (PBA),
         Idx    => 0),
      Level      => Level,
      Block_Data => Block_Data);

   type Object_Type is record
      State         : State_Type;
      Root_Node     : Type_1_Node_Type;
      Root_Dirty    : Boolean;
      Tree_Geom     : Tree_Geometry_Type;
      Current_Gen   : Generation_Type;
      Old_PBA       : Physical_Block_Address_Type;
      Complete_Prim : Primitive.Object_Type;
      Finished      : Boolean;

      Cache_Request : Cache_Request_Type;

      Level_N_Nodes : Type_1_Info_Array_Type;
      Level_1_Node  : Type_2_Info_Type;
   end record;

   procedure Exchange_NV_Level_1_Node (
      Obj       : in out Object_Type;
      T2_Entry  : in out Type_2_Node_Type;
      Exchanged :    out Boolean);

   procedure Exchange_NV_Inner_Nodes (
      Obj       : in out Object_Type;
      T2_Entry  : in out Type_2_Node_Type;
      Exchanged :    out Boolean);

   procedure Exchange_Request_PBA (
      Obj      : in out Object_Type;
      T2_Entry : in out Type_2_Node_Type);

   procedure Handle_Level_0_Nodes (
      Obj     : in out Object_Type;
      Handled :    out Boolean);

   procedure Handle_Level_1_Node (
      Obj     : in out Object_Type;
      Handled :    out Boolean);

   procedure Handle_Level_N_Nodes (
      Obj     : in out Object_Type;
      Handled :    out Boolean);

   procedure Update_Parent (
      Node       :    out Type_1_Node_Type;
      Block_Data : in     Block_Data_Type;
      Gen        :        Generation_Type;
      PBA        :        Physical_Block_Address_Type);

   procedure Execute_Update (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   function Check_Level_0_Usable (
      Gen  : Generation_Type;
      Node : Type_2_Node_Type)
   return Boolean;

   function Check_Node_Hash (
      Block_Data : Block_Data_Type;
      Node_Hash  : Hash_Type)
   return Boolean;

   function Block_From_Level_1_Node (Entries : Type_2_Node_Block_Type)
   return Block_Data_Type;

   function Block_From_Level_N_Node (Entries : Type_1_Node_Block_Type)
   return Block_Data_Type;

end CBE.Meta_Tree;
