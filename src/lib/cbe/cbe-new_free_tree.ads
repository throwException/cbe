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
with CBE.Write_Back;
with Interfaces;

use  Interfaces;

--
--  The Free_tree meta-module handles the allocation and freeing, i.e.,
--  reservation, of nodes. It is vital to implement the CoW semantics.
--
package CBE.New_Free_Tree
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   type Write_Back_Data_Type is record
      Prim           : Primitive.Object_Type;
      Gen            : Generation_Type;
      VBA            : Virtual_Block_Address_Type;
      Tree_Max_Level : Tree_Level_Index_Type;
      New_PBAs       : Write_Back.New_PBAs_Type;
      Old_PBAs       : Type_1_Node_Walk_Type;
   end record;

   procedure Initialized_Object (Obj : out Object_Type);

   ------------------------
   --  Module interface  --
   ------------------------

   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj              : in out Object_Type;
      Root_Node        :        Type_1_Node_Type;
      Tree_Geom        :        Tree_Geometry_Type;
      Current_Gen      :        Generation_Type;
      Free_Gen         :        Generation_Type;
      Requested_Blocks :        Number_Of_Blocks_Type;
      New_Blocks       :        Write_Back.New_PBAs_Type;
      Old_Blocks       :        Type_1_Node_Walk_Type;
      Max_Level        :        Tree_Level_Index_Type;
      Req_Prim         :        Primitive.Object_Type;
      VBA              :        Virtual_Block_Address_Type;
      VBD_Degree       :        Tree_Degree_Type;
      Key_ID           :        Key_ID_Type;
      Rekeying         :        Boolean;
      Previous_Key_ID  :        Key_ID_Type;
      Rekeying_VBA     :        Virtual_Block_Address_Type);

   procedure Retry_Allocation (Obj : in out Object_Type);

   --
   --  Execute module
   --
   procedure Execute (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean);

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
   --  Get write-back Data belonging to a completed primitive
   --
   function Peek_Completed_WB_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Write_Back_Data_Type;

   --
   --  Discard given completed primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Get generated meta-data tree primitive
   --
   function Peek_Generated_Meta_Tree_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Discard generated meta-data tree primitive
   --
   procedure Drop_Generated_Meta_Tree_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark generated meta-data tree primitive as complete
   --
   procedure Mark_Generated_Meta_Tree_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      PBA  :        Physical_Block_Address_Type);

   --
   --  Get generated cache primitive
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
   --  Discard generated cache primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark generated cache primitive as complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj         : in out Object_Type;
      Prim        :        Primitive.Object_Type;
      Block_Data  :        Block_Data_Type);

   -----------------
   --  Accessors  --
   -----------------

   function To_String (Obj : Object_Type) return String;

private

   type Tree_Degree_Log_2_Type
   is range Tree_Min_Degree_Log_2 .. Tree_Max_Degree_Log_2;

   function Node_Volatile (
      T   : Type_1_Node_Type;
      Gen : Generation_Type)
   return Boolean is (T.Gen = 0 or else T.Gen = Gen);

   type Node_Index_Type is new Byte_Type;

   function Node_Index_Invalid return Node_Index_Type
   is (Node_Index_Type'Last);

   type Info_State_Type is (
      Invalid, Available, Read, Write, Complete);

   function To_String (S : Info_State_Type) return String;

   type Type_1_Info_Type is record
      State    : Info_State_Type;
      Node     : Type_1_Node_Type;
      Index    : Node_Index_Type;
      Volatile : Boolean;
   end record;

   function Type_1_Info_Invalid return Type_1_Info_Type
   is (
      State    => Invalid,
      Node     => Type_1_Node_Invalid,
      Index    => Node_Index_Invalid,
      Volatile => False);

   function To_String (T : Type_1_Info_Type) return String;

   package Type_1_Info_Stack
   is
         type Object_Type is private;

         function Initialized_Object return Object_Type;

         function Empty    (Obj : Object_Type) return Boolean;
         function Full     (Obj : Object_Type) return Boolean;
         function Peek_Top (Obj : Object_Type) return Type_1_Info_Type;

         procedure Reset (Obj : in out Object_Type);
         procedure Pop   (Obj : in out Object_Type);

         procedure Push (
            Obj : in out Object_Type;
            Val :        Type_1_Info_Type);

         procedure Update_Top (
            Obj : in out Object_Type;
            Val :        Type_1_Info_Type);

   private

      type Stack_Array is array (Natural range <>) of Type_1_Info_Type;
      Min : constant := 1;
      Max : constant := Tree_Max_Degree;

      type Object_Type is record
         Container : Stack_Array (Min .. Max);
         Top       : Natural := Min - 1;
      end record;
   end Type_1_Info_Stack;

   type Type_2_Info_Type is record
      State    : Info_State_Type;
      Node     : Type_2_Node_Type;
      Index    : Node_Index_Type;
   end record;

   function Type_2_Info_Invalid return Type_2_Info_Type
   is (
      State    => Invalid,
      Node     => Type_2_Node_Invalid,
      Index    => Node_Index_Invalid);

   function To_String (T : Type_2_Info_Type) return String;

   package Type_2_Info_Stack
   is
         type Object_Type is private;

         function Initialized_Object return Object_Type;

         function Empty    (Obj : Object_Type) return Boolean;
         function Full     (Obj : Object_Type) return Boolean;
         function Peek_Top (Obj : Object_Type) return Type_2_Info_Type;

         procedure Reset (Obj : in out Object_Type);
         procedure Pop   (Obj : in out Object_Type);

         procedure Push (
            Obj : in out Object_Type;
            Val :        Type_2_Info_Type);

         procedure Update_Top (
            Obj : in out Object_Type;
            Val :        Type_2_Info_Type);

   private

      type Stack_Array is array (Natural range <>) of Type_2_Info_Type;
      Min : constant := 1;
      Max : constant := Tree_Max_Degree;

      type Object_Type is record
         Container : Stack_Array (Min .. Max);
         Top       : Natural := Min - 1;
      end record;
   end Type_2_Info_Stack;

   package Node_Queue
   is
      type Node_Queue_Index_Type is range 1 .. Tree_Max_Degree;
      type Used_Type is range 0 .. (Node_Queue_Index_Type'Last - 1);
      type Queue_Array is array (Node_Queue_Index_Type) of Type_2_Info_Type;

      type Node_Queue_Type is private;

      function Empty_Node_Queue return Node_Queue_Type;

      procedure Enqueue (
         Obj  : in out Node_Queue_Type;
         Node :        Type_2_Info_Type);

      procedure Dequeue_Head (Obj : in out Node_Queue_Type);

      function Head  (Obj : Node_Queue_Type) return Type_2_Info_Type;
      function Empty (Obj : Node_Queue_Type) return Boolean;
      function Full  (Obj : Node_Queue_Type) return Boolean;

      procedure Dump (Obj : Node_Queue_Type);

   private

      type Node_Queue_Type is record
         Head      : Node_Queue_Index_Type;
         Tail      : Node_Queue_Index_Type;
         Container : Queue_Array;
         Used      : Used_Type;
      end record;
   end Node_Queue;

   type State_Type is (
      Invalid,
      Scan,
      Scan_Complete,
      Update,
      Update_Complete,
      Complete,
      Not_Enough_Free_Blocks,
      Tree_Hash_Mismatch);

   function To_String (S : State_Type) return String;

   subtype Type_1_Info_Stack_Array_Index_Type is Tree_Level_Index_Type'Base
      range 1 .. Tree_Level_Index_Type'Last;

   type Type_1_Info_Stack_Array_Type is array
      (Type_1_Info_Stack_Array_Index_Type) of
         Type_1_Info_Stack.Object_Type;

   procedure Initialize_Type_1_Info_Stack_Array (
      A : out Type_1_Info_Stack_Array_Type);

   type Type_1_Node_Block_Array_Type is array
      (Type_1_Info_Stack_Array_Index_Type) of
         Type_1_Node_Block_Type;

   procedure Initialize_Type_1_Node_Block_Array (
      A : out Type_1_Node_Block_Array_Type);

   type Meta_Tree_Request_State_Type is (
      Invalid, Pending, In_Progress, Complete);

   type Meta_Tree_Request_Type is record
      State : Meta_Tree_Request_State_Type;
      Prim  : Primitive.Object_Type;
      PBA   : Physical_Block_Address_Type;
   end record;

   function Invalid_Meta_Tree_Request return Meta_Tree_Request_Type
   is (
      State => Invalid,
      Prim  => Primitive.Invalid_Object,
      PBA   => PBA_Invalid);

   function New_Meta_Tree_Request (PBA : Physical_Block_Address_Type)
      return Meta_Tree_Request_Type
   is (
      State => Pending,
      Prim => Primitive.Valid_Object_No_Pool_Idx (
         Op     => Read,
         Succ   => Request.Success_Type (False),
         Tg     => Primitive.Tag_FT_MT,
         Blk_Nr => Block_Number_Type (PBA),
         Idx    => 0),
      PBA  => PBA);

   type Cache_Request_State_Type is (
      Invalid, Pending, In_Progress, Complete);

   type Cache_Request_Type is record
      State : Cache_Request_State_Type;
      Prim  : Primitive.Object_Type;
      Level : Tree_Level_Index_Type;
   end record;

   function Invalid_Cache_Request return Cache_Request_Type
   is (
      State => Invalid,
      Prim  => Primitive.Invalid_Object,
      Level => 0);

   function New_Cache_Request (
      PBA   : Physical_Block_Address_Type;
      Op    : Primitive_Operation_Type;
      Level : Tree_Level_Index_Type)
   return Cache_Request_Type
   is (
      State => Pending,
      Prim  => Primitive.Valid_Object_No_Pool_Idx (
         Op     => Op,
         Succ   => Request.Success_Type (False),
         Tg     => Primitive.Tag_FT_Cache,
         Blk_Nr => Block_Number_Type (PBA),
         Idx    => 0),
      Level  => Level);

   type Object_Type is record

      State            : State_Type;
      Root_Node        : Type_1_Node_Type;
      Tree_Geom        : Tree_Geometry_Type;
      Current_Gen      : Generation_Type;
      Free_Gen         : Generation_Type;
      Requested_Blocks : Number_Of_Blocks_Type;

      Needed_Blocks : Number_Of_Blocks_Type;
      Found_Blocks  : Number_Of_Blocks_Type;

      Exchanged_Blocks : Number_Of_Blocks_Type;

      Meta_Tree_Request : Meta_Tree_Request_Type;
      Cache_Request     : Cache_Request_Type;
      Cache_Block_Data  : Block_Data_Type;

      Level_N_Stacks : Type_1_Info_Stack_Array_Type;
      Level_0_Stack  : Type_2_Info_Stack.Object_Type;

      Level_N_Nodes : Type_1_Node_Block_Array_Type;

      Level_N_Node  : Type_1_Node_Block_Type;
      Level_0_Node  : Type_2_Node_Block_Type;

      Type_2_Leafs  : Node_Queue.Node_Queue_Type;

      --
      --  As we might need to write multiple branches back to the disk
      --  make sure we have enough entries available.
      --
      WB_Data : Write_Back_Data_Type;

      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      Key_ID           : Key_ID_Type;

      Rekeying         : Boolean;
      Previous_Key_ID  : Key_ID_Type;
      Rekeying_VBA     : Virtual_Block_Address_Type;

   end record;

   function Write_Back_Data_Invalid return Write_Back_Data_Type
   is (
      Prim           => Primitive.Invalid_Object,
      Gen            => 0,
      VBA            => VBA_Invalid,
      Tree_Max_Level => 0,
      New_PBAs       => (others => PBA_Invalid),
      Old_PBAs       => (others => Type_1_Node_Invalid));

   procedure Reset_Block_State (Obj : in out Object_Type);

   --
   --  Check_Type_2_Leaf_Usable
   --
   function Check_Type_2_Leaf_Usable (
      Snapshots        : Snapshots_Type;
      Last_Secured_Gen : Generation_Type;
      Node             : Type_2_Node_Type;
      Rekeying         : Boolean;
      Previous_Key_ID  : Key_ID_Type;
      Rekeying_VBA     : Virtual_Block_Address_Type)
   return Boolean;

   function Check_Node_Hash (
      Block_Data : Block_Data_Type;
      Node_Hash  : Hash_Type)
   return Boolean;

   function Top_Volatile (S : Type_1_Info_Stack.Object_Type) return Boolean;

   function Block_From_Level_0_Node (Entries : Type_2_Node_Block_Type)
   return Block_Data_Type;

   --
   --  Populate_Level_0_Stack
   --
   procedure Populate_Level_0_Stack (
      Stack           : in out Type_2_Info_Stack.Object_Type;
      Entries         :    out Type_2_Node_Block_Type;
      Block_Data      :        Block_Data_Type;
      Active_Snaps    :        Snapshots_Type;
      Secured_Gen     :        Generation_Type;
      Rekeying        :        Boolean;
      Previous_Key_ID :        Key_ID_Type;
      Rekeying_VBA    :        Virtual_Block_Address_Type);

   function Block_From_Level_N_Node (Entries : Type_1_Node_Block_Type)
   return Block_Data_Type;

   procedure Populate_Lower_N_Stack (
      Stack       : in out Type_1_Info_Stack.Object_Type;
      Entries     :    out Type_1_Node_Block_Type;
      Block_Data  :        Block_Data_Type;
      Current_Gen :        Generation_Type);

   procedure Update_Upper_N_Stack (
      T          :        Type_1_Info_Type;
      Gen        :        Generation_Type;
      Block_Data : in     Block_Data_Type;
      Entries    : in out Type_1_Node_Block_Type);

   procedure Check_Type_2_Stack (
      Stack      : in out Type_2_Info_Stack.Object_Type;
      Stack_Next : in out Type_1_Info_Stack.Object_Type;
      Leafs      : in out Node_Queue.Node_Queue_Type;
      Found      : in out Number_Of_Blocks_Type);

   procedure Execute_Scan (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean);

   procedure Exchange_Type_2_Leafs (
      Free_Gen         :        Generation_Type;
      Max_Level        :        Tree_Level_Index_Type;
      Old_Blocks       : in     Type_1_Node_Walk_Type;
      New_Blocks       : in out Write_Back.New_PBAs_Type;
      VBA              :        Virtual_Block_Address_Type;
      VBD_Degree_Log_2 :        Tree_Degree_Log_2_Type;
      Tag              :        Primitive.Tag_Type;
      Stack            : in out Type_2_Info_Stack.Object_Type;
      Entries          : in out Type_2_Node_Block_Type;
      Exchanged        :    out Number_Of_Blocks_Type;
      Handled          :    out Boolean;
      Key_ID           :        Key_ID_Type);

   procedure Execute_Update (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean);

   --
   --  Log_2
   --
   function  Log_2 (Value : Unsigned_32)
   return Unsigned_32;

   --
   --  VBD_Inner_Node_VBA
   --
   function VBD_Inner_Node_VBA (
      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      VBD_Level        : Tree_Level_Index_Type;
      VBD_Leaf_VBA     : Virtual_Block_Address_Type)
   return Virtual_Block_Address_Type;

end CBE.New_Free_Tree;
