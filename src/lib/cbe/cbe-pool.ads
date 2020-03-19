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

package CBE.Pool
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj     : in out Object_Type;
      Req     :        Request.Object_Type;
      Snap_ID :        Snapshot_ID_Type);

   --
   --  Peek_Pending_Request
   --
   function Peek_Pending_Request (Obj : Pool.Object_Type)
   return Pool_Index_Slot_Type;

   --
   --  Drop_Pending_Request
   --
   procedure Drop_Pending_Request (Obj : in out Object_Type);

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Peek_Completed_Request
   --
   function Peek_Completed_Request (Obj : Pool.Object_Type)
   return Request.Object_Type;

   --
   --  Drop_Completed_Request
   --
   procedure Drop_Completed_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Request_For_Index
   --
   function Request_For_Index (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Request.Object_Type;

   --
   --  Peek_Generated_Discard_Snap_Primitive
   --
   function Peek_Generated_Discard_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Create_Snap_Primitive
   --
   function Peek_Generated_Create_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Sync_Primitive
   --
   function Peek_Generated_Sync_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_VBD_Primitive
   --
   function Peek_Generated_VBD_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_VBD_Primitive_ID
   --
   function Peek_Generated_VBD_Primitive_ID (Obj : Object_Type)
   return Snapshot_ID_Type;

   --
   --  Drop_Generated_VBD_Primitive
   --
   procedure Drop_Generated_VBD_Primitive (Obj : in out Object_Type);

private

   package Index_Queue
   with SPARK_Mode
   is
      type Queue_Index_Type is new Pool_Index_Type;
      type Used_Type is range 0 .. Max_Number_Of_Requests_In_Pool;
      type Item_Indices_Type
      is array (Queue_Index_Type) of Pool_Index_Type;

      type Index_Queue_Type is private;

      function Empty_Index_Queue
      return Index_Queue_Type;

      procedure Enqueue (
         Obj : in out Index_Queue_Type;
         Idx   :      Pool_Index_Type);

      function Head (Obj : Index_Queue_Type)
      return Pool_Index_Type;

      procedure Dequeue_Head (Obj : in out Index_Queue_Type);

      function Empty (Obj : Index_Queue_Type)
      return Boolean;

      function Full (Obj : Index_Queue_Type)
      return Boolean;

      function Avail (
         Obj : Index_Queue_Type;
         Num : Natural)
      return Boolean;

   private

      type Index_Queue_Type is record
         Head    : Queue_Index_Type;
         Tail    : Queue_Index_Type;
         Used    : Used_Type;
         Indices : Item_Indices_Type;
      end record;

   end Index_Queue;

   type Item_State_Type is (Invalid, Pending, In_Progress, Complete);

   type Item_Type is record
      State                 : Item_State_Type;
      Req                   : Request.Object_Type;
      Snap_ID               : Snapshot_ID_Type;
      Nr_Of_Prims_Completed : Number_Of_Primitives_Type;
   end record;

   type Items_Type is array (Pool_Index_Type) of Item_Type;

   type Object_Type is record
      Items   : Items_Type;
      Indices : Index_Queue.Index_Queue_Type;
   end record;

   --
   --  Item_Invalid
   --
   function Item_Invalid
   return Item_Type;

   --
   --  Item_Mark_Completed_Primitive
   --
   procedure Item_Mark_Completed_Primitive (
      Itm  : in out Item_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Item_Nr_Of_Prims
   --
   function Item_Nr_Of_Prims (Itm : Item_Type)
   return Number_Of_Primitives_Type;

end CBE.Pool;
