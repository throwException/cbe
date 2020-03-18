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
   --  Item_Invalid
   --
   function Item_Invalid
   return Item_Type
   is (
      State            => Invalid,
      Req              => Request.Invalid_Object,
      Snap_ID          => 0,
      Nr_Of_Prims      => 0,
      Nr_Of_Done_Prims => 0);

   --
   --  Item_To_String
   --
   function Item_To_String (Itm : Item_Type)
   return String
   is (
      case Itm.State is
      when Invalid     => "invalid",
      when Pending     => "pending",
      when In_Progress => "in_progress",
      when Complete    => "complete");

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj := Initialized_Object;
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Items => (others => Item_Invalid),
      Indices => Index_Queue.Empty_Index_Queue,
      Splitter => (
         Pool_Idx_Slot => Pool_Idx_Slot_Invalid,
         Curr_Req      => Request.Invalid_Object,
         Curr_Blk_Nr   => 0,
         Curr_Idx      => 0,
         Nr_Of_Prims   => 0,
         Snap_ID       => 0));

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type) return Boolean
   is
   begin
      if not Index_Queue.Full (Obj.Indices)
         and then Index_Queue.Avail (Obj.Indices, 2)
      then
         return True;
      else
         return False;
      end if;
   end Request_Acceptable;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj         : in out Object_Type;
      Req         :        Request.Object_Type;
      ID          :        Snapshot_ID_Type;
      Nr_Of_Prims :        Number_Of_Primitives_Type)
   is
      Req_Buf : Request.Object_Type := Req;
   begin

      Items_Loop :
      for Item_Id in Obj.Items'Range loop

         if Obj.Items (Item_Id).State = Invalid then

            Request.Success (Req_Buf, True);
            Obj.Items (Item_Id) := (
               State            => Pending,
               Req              => Req_Buf,
               Snap_ID          => ID,
               Nr_Of_Prims      => Nr_Of_Prims,
               Nr_Of_Done_Prims => 0);

            Index_Queue.Enqueue (Obj.Indices, Item_Id);

            exit Items_Loop;

         end if;

      end loop Items_Loop;

   end Submit_Request;

   --
   --  Peek_Pending_Request
   --
   function Peek_Pending_Request (Obj : Object_Type)
   return Pool_Index_Slot_Type
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         return Pool_Idx_Slot_Invalid;
      end if;

      if not Request.Valid (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      is
      when Read | Write =>
         return Pool_Idx_Slot_Invalid;

      when Sync =>
         return Pool_Idx_Slot_Valid (Index_Queue.Head (Obj.Indices));

      when Create_Snapshot =>
         return Pool_Idx_Slot_Valid (Index_Queue.Head (Obj.Indices));

      when Discard_Snapshot =>
         return Pool_Idx_Slot_Valid (Index_Queue.Head (Obj.Indices));

      end case;

   end Peek_Pending_Request;

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin

      if Index_Queue.Empty (Obj.Indices) then
         return;
      end if;

      if not Request.Valid (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      is
      when Read | Write =>

         if not Request.Valid (Obj.Splitter.Curr_Req) then

            Obj.Splitter := (
               Pool_Idx_Slot =>
                  Pool_Idx_Slot_Valid (Index_Queue.Head (Obj.Indices)),
               Curr_Req      => Obj.Items (Index_Queue.Head (Obj.Indices)).Req,
               Curr_Blk_Nr   =>
                  Request.Block_Number (
                     Obj.Items (Index_Queue.Head (Obj.Indices)).Req),
               Curr_Idx      => 0,
               Nr_Of_Prims   =>
                  Number_Of_Primitives_Type (
                     Request.Count (
                        Obj.Items (Index_Queue.Head (Obj.Indices)).Req)),
               Snap_ID       =>
                  Snap_ID_For_Request (
                     Obj,
                     Obj.Items (Index_Queue.Head (Obj.Indices)).Req));

               Obj.Items (Index_Queue.Head (Obj.Indices)).State := In_Progress;

            if not Request.Valid (Obj.Splitter.Curr_Req) then
               raise Program_Error;
            end if;

            Progress := True;

         end if;

      when Sync => return;
      when Create_Snapshot => return;
      when Discard_Snapshot => return;
      end case;

   end Execute;

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

      if not Request.Valid (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Items (Index_Queue.Head (Obj.Indices)).Req)
      is
      when Read | Write =>

         if not Request.Valid (Obj.Splitter.Curr_Req) then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Request.Operation (Obj.Splitter.Curr_Req),
               Request.Success (Obj.Splitter.Curr_Req),
               Primitive.Tag_Splitter,
               Pool_Idx_Slot_Content (Obj.Splitter.Pool_Idx_Slot),
               Obj.Splitter.Curr_Blk_Nr,
               Obj.Splitter.Curr_Idx);

      when Sync =>
         return Primitive.Invalid_Object;

      when Create_Snapshot =>
         return Primitive.Invalid_Object;

      when Discard_Snapshot =>
         return Primitive.Invalid_Object;

      end case;

   end Peek_Generated_VBD_Primitive;

   --
   --  Number_Of_Primitives
   --
   function Number_Of_Primitives (Req : Request.Object_Type)
   return Number_Of_Primitives_Type
   is
   begin

      return Number_Of_Primitives_Type (Request.Count (Req));
   end Number_Of_Primitives;

   --
   --  Peek_Generated_Primitive_ID
   --
   function Peek_Generated_VBD_Primitive_ID (Obj : Object_Type)
   return Snapshot_ID_Type
   is
   begin

      return Obj.Splitter.Snap_ID;
   end Peek_Generated_VBD_Primitive_ID;

   --
   --  Drop_Generated_VBD_Primitive
   --
   procedure Drop_Generated_VBD_Primitive (Obj : in out Object_Type)
   is
      use type Primitive.Index_Type;
   begin
      Obj.Splitter.Curr_Blk_Nr := Obj.Splitter.Curr_Blk_Nr + 1;
      Obj.Splitter.Curr_Idx    := Obj.Splitter.Curr_Idx    + 1;

      if Number_Of_Primitives_Type (Obj.Splitter.Curr_Idx) =
            Obj.Splitter.Nr_Of_Prims
      then
         Obj.Splitter.Pool_Idx_Slot := Pool_Idx_Slot_Invalid;
         Obj.Splitter.Curr_Req      := Request.Invalid_Object;
         Obj.Splitter.Curr_Blk_Nr   := 0;
         Obj.Splitter.Curr_Idx      := 0;
         Obj.Splitter.Nr_Of_Prims   := 0;
         Obj.Splitter.Snap_ID       := 0;

         Drop_Pending_Request (Obj);
      end if;

   end Drop_Generated_VBD_Primitive;

   --
   --  Drop_Pending_Request
   --
   procedure Drop_Pending_Request (Obj : in out Object_Type)
   is
   begin
      if Index_Queue.Empty (Obj.Indices) then
         raise Program_Error;
      end if;

      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
      begin
         Obj.Items (Idx).State := In_Progress;
      end;

      Index_Queue.Dequeue_Head (Obj.Indices);
   end Drop_Pending_Request;

   --
   --  Item_Mark_Completed_Primitive
   --
   procedure Item_Mark_Completed_Primitive (
      Itm  : in out Item_Type;
      Prim :        Primitive.Object_Type)
   is
   begin

      if not Primitive.Success (Prim) then
         Request.Success (Itm.Req, False);
      end if;

      Itm.Nr_Of_Done_Prims := Itm.Nr_Of_Done_Prims + 1;

      if Itm.Nr_Of_Done_Prims = Itm.Nr_Of_Prims then
         Itm.State := Complete;
      end if;

   end Item_Mark_Completed_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Item_Mark_Completed_Primitive (
         Obj.Items (Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim))),
         Prim);

   end Mark_Completed_Primitive;

   --
   --  Peek_Completed_Request
   --
   function Peek_Completed_Request (Obj : Pool.Object_Type)
   return Request.Object_Type
   is
   begin
      for Idx in Obj.Items'Range loop
         if Obj.Items (Idx).State = Complete then
            return Obj.Items (Idx).Req;
         end if;
      end loop;
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

      For_Each_Item :
      for Idx in Obj.Items'Range loop
         if Request.Equal (Obj.Items (Idx).Req, Req) then
            if Obj.Items (Idx).State /= Complete then
               raise Program_Error;
            end if;
            Obj.Items (Idx) := Item_Invalid;
            return;
         end if;
      end loop For_Each_Item;
      raise Program_Error;
   end Drop_Completed_Request;

   --
   --  Snap_ID_For_Request
   --
   function Snap_ID_For_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Snapshot_ID_Type
   is
   begin
      for Itm of Obj.Items loop
         if Itm.State /= Invalid then
            if Request.Equal (Itm.Req, Req) then
               return Itm.Snap_ID;
            end if;
         end if;
      end loop;
      raise Program_Error;
   end Snap_ID_For_Request;

   --
   --  Dump pool state
   --
   procedure Dump_Pool_State (Obj : Object_Type)
   is
   begin
      for I in Obj.Items'Range loop
         if Obj.Items (I).State /= Invalid then
            pragma Debug (Debug.Print_String ("Request_Pool: "
               & Debug.To_String (Debug.Uint64_Type (I))
               & ": "
               & "Req: " & Request.To_String (Obj.Items (I).Req)
               & " State: "
               & Item_To_String (Obj.Items (I))));
            null;
         end if;
      end loop;
   end Dump_Pool_State;

   --
   --  Check if a overlapping request is already in progress
   --
   function Overlapping_Request_In_Progress (
      Obj    : Object_Type;
      Blk_Nr : Block_Number_Type)
   return Boolean
   is
      Result : Boolean := False;
   begin
      Loop_In_Progress_Items :
      for Itm of Obj.Items loop
         if Itm.State = In_Progress then

            --  The overlap range is [lower, upper).
            declare
               Lower : constant Block_Number_Type :=
                  Request.Block_Number (Itm.Req);
               Upper : constant Block_Number_Type :=
                  Lower + Block_Number_Type (Request.Count (Itm.Req));
            begin
               if Blk_Nr >= Lower and then Blk_Nr < Upper then
                  pragma Debug (Debug.Print_String ("Overlap: Blk_Nr: "
                     & Debug.To_String (Debug.Uint64_Type (Blk_Nr))
                     & " Lower: "
                     & Debug.To_String (Debug.Uint64_Type (Lower))
                     & " Upper: "
                     & Debug.To_String (Debug.Uint64_Type (Upper))));
                  Result := True;
                  exit Loop_In_Progress_Items;
               end if;
            end;
         end if;
      end loop Loop_In_Progress_Items;

      return Result;
   end Overlapping_Request_In_Progress;

   --
   --  Request_For_Index
   --
   function Request_For_Index (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Request.Object_Type
   is
   begin
      if not Request.Valid (Obj.Items (Idx).Req) then
         raise Program_Error;
      end if;
      return Obj.Items (Idx).Req;
   end Request_For_Index;

   function Index_For_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Pool_Index_Type
   is
   begin
      For_Each_Item : for Idx in Obj.Items'Range loop
         if Request.Equal (Obj.Items (Idx).Req, Req) then
            return Idx;
         end if;
      end loop For_Each_Item;
      raise Program_Error;
   end Index_For_Request;

   package body Index_Queue
   with SPARK_Mode
   is
      function Empty_Index_Queue
      return Index_Queue_Type
      is (
         Head   => Queue_Index_Type'First,
         Tail   => Queue_Index_Type'First,
         Used   => Used_Type'First,
         Indices => (others => Pool_Index_Type'First));

      procedure Enqueue (
         Obj : in out Index_Queue_Type;
         Idx :        Pool_Index_Type)
      is
      begin
         Obj.Indices (Obj.Tail) := Idx;

         if Obj.Tail < Queue_Index_Type'Last then
            Obj.Tail := Queue_Index_Type'Succ (Obj.Tail);
         else
            Obj.Tail := Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Succ (Obj.Used);
      end Enqueue;

      function Head (Obj : Index_Queue_Type)
      return Pool_Index_Type
      is (Obj.Indices (Obj.Head));

      procedure Dequeue_Head (Obj : in out Index_Queue_Type)
      is
      begin
         if Obj.Head < Queue_Index_Type'Last then
            Obj.Head := Queue_Index_Type'Succ (Obj.Head);
         else
            Obj.Head := Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Pred (Obj.Used);
      end Dequeue_Head;

      function Empty (Obj : Index_Queue_Type)
      return Boolean
      is (Obj.Used = Used_Type'First);

      function Full (Obj : Index_Queue_Type)
      return Boolean
      is (Obj.Used = Used_Type'Last);

      function Avail (
         Obj : Index_Queue_Type;
         Num : Natural)
      return Boolean
      is
      begin
         if Obj.Used <= Used_Type'Last - Used_Type (Num) then
            return True;
         else
            return False;
         end if;
      end Avail;
   end Index_Queue;

end CBE.Pool;
