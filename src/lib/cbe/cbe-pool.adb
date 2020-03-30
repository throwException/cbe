--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;
pragma Unreferenced (CBE.Debug);

package body CBE.Pool
with SPARK_Mode
is
   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (
      not Index_Queue.Full (Obj.Indices));

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj     : in out Object_Type;
      Req     :        Request.Object_Type;
      Snap_ID :        Snapshot_ID_Type)
   is
   begin
      Loop_Items :
      for Idx in Obj.Items'Range loop

         if Obj.Items (Idx).State = Invalid then
            Obj.Items (Idx) := (
               State                 => Pending,
               Req                   => Req,
               Snap_ID               => Snap_ID,
               Nr_Of_Prims_Completed => 0);

            Request.Success (Obj.Items (Idx).Req, True);
            Index_Queue.Enqueue (Obj.Indices, Idx);
            return;

         end if;

      end loop Loop_Items;

      raise Program_Error;

   end Submit_Request;

   --
   --  Peek_Generated_Discard_Snap_Primitive
   --
   function Peek_Generated_Discard_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Primitive.Invalid_Object;
      end if;

      Declare_Item :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Itm : constant Item_Type := Obj.Items (Idx);
      begin
         if Itm.State /= Pending or else
            Request.Operation (Itm.Req) /= Discard_Snapshot
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Read,
               Request.Success (Itm.Req),
               Primitive.Tag_Pool_Discard_Snap,
               Idx,
               Request.Block_Number (Itm.Req) +
                  Block_Number_Type (Itm.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Itm.Nr_Of_Prims_Completed));

      end Declare_Item;
   end Peek_Generated_Discard_Snap_Primitive;

   --
   --  Peek_Generated_Create_Snap_Primitive
   --
   function Peek_Generated_Create_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Primitive.Invalid_Object;
      end if;

      Declare_Item :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Itm : constant Item_Type := Obj.Items (Idx);
      begin
         if Itm.State /= Pending or else
            Request.Operation (Itm.Req) /= Create_Snapshot
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Read,
               Request.Success (Itm.Req),
               Primitive.Tag_Pool_Create_Snap,
               Idx,
               Request.Block_Number (Itm.Req) +
                  Block_Number_Type (Itm.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Itm.Nr_Of_Prims_Completed));

      end Declare_Item;
   end Peek_Generated_Create_Snap_Primitive;

   --
   --  Peek_Generated_Sync_Primitive
   --
   function Peek_Generated_Sync_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Primitive.Invalid_Object;
      end if;

      Declare_Item :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Itm : constant Item_Type := Obj.Items (Idx);
      begin
         if Itm.State /= Pending or else
            Request.Operation (Itm.Req) /= Sync
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Sync,
               Request.Success (Itm.Req),
               Primitive.Tag_Pool_Sync,
               Idx,
               Request.Block_Number (Itm.Req) +
                  Block_Number_Type (Itm.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Itm.Nr_Of_Prims_Completed));

      end Declare_Item;
   end Peek_Generated_Sync_Primitive;

   --
   --  Peek_Generated_VBD_Primitive
   --
   function Peek_Generated_VBD_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Primitive.Invalid_Object;
      end if;

      Declare_Item :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Itm : constant Item_Type := Obj.Items (Idx);
      begin
         if Itm.State /= Pending or else (
               Request.Operation (Itm.Req) /= Read and then
               Request.Operation (Itm.Req) /= Write)
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Prim_Op_From_Req_Op (Request.Operation (Itm.Req)),
               Request.Success (Itm.Req),
               Primitive.Tag_Pool_VBD,
               Idx,
               Request.Block_Number (Itm.Req) +
                  Block_Number_Type (Itm.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Itm.Nr_Of_Prims_Completed));

      end Declare_Item;
   end Peek_Generated_VBD_Primitive;

   --
   --  Peek_Generated_VBD_Primitive_ID
   --
   function Peek_Generated_VBD_Primitive_ID (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Snapshot_ID_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx or else
         Obj.Items (Idx).State /= Pending or else (
            Request.Operation (Obj.Items (Idx).Req) /= Read and then
            Request.Operation (Obj.Items (Idx).Req) /= Write)
      then
         raise Program_Error;
      end if;

      return Obj.Items (Idx).Snap_ID;

   end Peek_Generated_VBD_Primitive_ID;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Idx  :        Pool_Index_Type)
   is
   begin
      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx or else
         Obj.Items (Idx).State /= Pending
      then
         raise Program_Error;
      end if;

      Obj.Items (Idx).State := In_Progress;

   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj     : in out Object_Type;
      Idx     :        Pool_Index_Type;
      Success :        Boolean)
   is
   begin
      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx or else
         Obj.Items (Idx).State /= In_Progress
      then
         raise Program_Error;
      end if;

      Request.Success (Obj.Items (Idx).Req, Success);
      Obj.Items (Idx).Nr_Of_Prims_Completed :=
         Obj.Items (Idx).Nr_Of_Prims_Completed + 1;

      if Obj.Items (Idx).Nr_Of_Prims_Completed <
            Item_Nr_Of_Prims (Obj.Items (Idx))
      then
         Obj.Items (Idx).State := Pending;
      else
         Obj.Items (Idx).State := Complete;
         Index_Queue.Dequeue (Obj.Indices, Idx);
      end if;

   end Mark_Generated_Primitive_Complete;

   --
   --  Peek_Completed_Request
   --
   function Peek_Completed_Request (Obj : Pool.Object_Type)
   return Request.Object_Type
   is
   begin
      For_Each_Item_Idx :
      for Idx in Obj.Items'Range loop
         if Obj.Items (Idx).State = Complete then
            return Obj.Items (Idx).Req;
         end if;
      end loop For_Each_Item_Idx;
      return Request.Invalid_Object;
   end Peek_Completed_Request;

   --
   --  Drop_Completed_Request
   --
   procedure Drop_Completed_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      For_Each_Item_Idx :
      for Idx in Obj.Items'Range loop
         if Request.Equal (Obj.Items (Idx).Req, Req) then
            if Obj.Items (Idx).State /= Complete then
               raise Program_Error;
            end if;
            Obj.Items (Idx) := Item_Invalid;
            return;
         end if;
      end loop For_Each_Item_Idx;
      raise Program_Error;
   end Drop_Completed_Request;

   --
   --  Request_For_Index
   --
   function Request_For_Index (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Request.Object_Type
   is
   begin
      if Obj.Items (Idx).State = Invalid then
         raise Program_Error;
      end if;
      return Obj.Items (Idx).Req;
   end Request_For_Index;

   --
   --  Item_Nr_Of_Prims
   --
   function Item_Nr_Of_Prims (Itm : Item_Type)
   return Number_Of_Primitives_Type
   is (
      case Request.Operation (Itm.Req) is
      when Read | Write =>
         Number_Of_Primitives_Type (Request.Count (Itm.Req)),
      when Sync | Create_Snapshot | Discard_Snapshot | Rekey =>
         1);

   --
   --  Item_Invalid
   --
   function Item_Invalid
   return Item_Type
   is (
      State                 => Invalid,
      Req                   => Request.Invalid_Object,
      Snap_ID               => 0,
      Nr_Of_Prims_Completed => 0);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Items   => (others => Item_Invalid),
      Indices => Index_Queue.Empty_Queue);

end CBE.Pool;
