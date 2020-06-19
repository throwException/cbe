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

            case Request.Operation (Req) is
            when
               Rekey | Extend_VBD | Extend_FT | Decrypt_Keys | Deinitialize
            =>

               Obj.Items (Idx).State := Submitted;
               Obj.Items (Idx).Req   := Req;
               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            when Resume_Rekeying =>

               Obj.Items (Idx).State := Submitted_Resume_Rekeying;
               Obj.Items (Idx).Req   := Request.Valid_Object (
                  Op     => Rekey,
                  Succ   => False,
                  Blk_Nr => Block_Number_Type (0),
                  Off    => 0,
                  Cnt    => 0,
                  Key    => 0,
                  Tg     => 0);

               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            when Read | Write | Sync | Create_Snapshot | Discard_Snapshot =>

               Obj.Items (Idx) := (
                  State                   => Pending,
                  Req                     => Req,
                  Snap_ID                 => Snap_ID,
                  Prim                    => Primitive.Invalid_Object,
                  Request_Finished        => Boolean'First,
                  Nr_Of_Requests_Preponed => 0,
                  Nr_Of_Prims_Completed   => 0);

               Request.Success (Obj.Items (Idx).Req, True);
               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            end case;

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
   --  Peek_Generated_SB_Ctrl_Primitive
   --
   function Peek_Generated_SB_Ctrl_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      if not Index_Queue.Empty (Obj.Indices) then

         Declare_Item :
         declare
            Itm : constant Item_Type :=
               Obj.Items (Index_Queue.Head (Obj.Indices));
         begin

            case Itm.State is
            when
               Rekey_Init_Pending |
               Rekey_VBA_Pending |
               VBD_Extension_Step_Pending |
               FT_Extension_Step_Pending |
               Decrypt_Keys_Pending |
               Deinitialize_SB_Ctrl_Pending
            =>

               return Itm.Prim;

            when others =>

               return Primitive.Invalid_Object;

            end case;

         end Declare_Item;

      end if;
      return Primitive.Invalid_Object;

   end Peek_Generated_SB_Ctrl_Primitive;

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
   --  Peek_Generated_Nr_Of_Blks
   --
   function Peek_Generated_Nr_Of_Blks (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type
   is
      Idx : constant Pool_Index_Type :=
         Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim));
   begin

      case Obj.Items (Idx).State is
      when
         VBD_Extension_Step_Pending |
         FT_Extension_Step_Pending
      =>

         if Primitive.Equal (Prim, Obj.Items (Idx).Prim) then
            return Number_Of_Blocks_Type (
               Request.Count (Obj.Items (Idx).Req));
         else
            raise Program_Error;
         end if;

      when others =>

         raise Program_Error;

      end case;

   end Peek_Generated_Nr_Of_Blks;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Idx  :        Pool_Index_Type)
   is
   begin
      if not Index_Queue.Empty (Obj.Indices) and then
         Index_Queue.Head (Obj.Indices) = Idx
      then
         case Obj.Items (Idx).State is
         when Pending =>
            Obj.Items (Idx).State := In_Progress;
            return;
         when Rekey_Init_Pending =>
            Obj.Items (Idx).State := Rekey_Init_In_Progress;
            return;
         when Rekey_VBA_Pending =>
            Obj.Items (Idx).State := Rekey_VBA_In_Progress;
            return;
         when VBD_Extension_Step_Pending =>
            Obj.Items (Idx).State := VBD_Extension_Step_In_Progress;
            return;
         when FT_Extension_Step_Pending =>
            Obj.Items (Idx).State := FT_Extension_Step_In_Progress;
            return;
         when Decrypt_Keys_Pending =>
            Obj.Items (Idx).State := Decrypt_Keys_In_Progress;
            return;
         when Deinitialize_SB_Ctrl_Pending =>
            Obj.Items (Idx).State := Deinitialize_SB_Ctrl_In_Progress;
            return;
         when others =>
            raise Program_Error;
         end case;
      end if;
      raise Program_Error;
   end Drop_Generated_Primitive;

   --
   --  Execute_Extend_VBD
   --
   procedure Execute_Extend_VBD (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Items (Idx).State is
      when Submitted =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := VBD_Extension_Step_Pending;
         Progress := True;

      when VBD_Extension_Step_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         if Items (Idx).Request_Finished then

            Request.Success (Items (Idx).Req, True);
            Items (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Items (Idx).Nr_Of_Requests_Preponed := 0;
            Items (Idx).State := Prepone_Requests_Pending;
            Progress := True;

         end if;

      when Prepone_Requests_Pending =>

         Declare_Requests_Preponed :
         declare
            Requests_Preponed : Boolean := False;
         begin

            Try_Prepone_Requests :
            loop

               exit Try_Prepone_Requests when
                  Items (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Items (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Items (Idx).Nr_Of_Requests_Preponed :=
                        Items (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Items (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := VBD_Extension_Step_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Extend_VBD;

   --
   --  Execute_Extend_FT
   --
   procedure Execute_Extend_FT (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Items (Idx).State is
      when Submitted =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := FT_Extension_Step_Pending;
         Progress := True;

      when FT_Extension_Step_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         if Items (Idx).Request_Finished then

            Request.Success (Items (Idx).Req, True);
            Items (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Items (Idx).Nr_Of_Requests_Preponed := 0;
            Items (Idx).State := Prepone_Requests_Pending;
            Progress := True;

         end if;

      when Prepone_Requests_Pending =>

         Declare_Requests_Preponed :
         declare
            Requests_Preponed : Boolean := False;
         begin

            Try_Prepone_Requests :
            loop

               exit Try_Prepone_Requests when
                  Items (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Items (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Items (Idx).Nr_Of_Requests_Preponed :=
                        Items (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Items (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := FT_Extension_Step_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Extend_FT;

   --
   --  Execute_Rekey
   --
   procedure Execute_Rekey (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Items (Idx).State is
      when Submitted =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Init_Rekey,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := Rekey_Init_Pending;
         Progress := True;

      when Submitted_Resume_Rekeying =>

         Items (Idx).Prim := Primitive.Invalid_Object;

         Items (Idx).Nr_Of_Requests_Preponed := 0;
         Items (Idx).State := Prepone_Requests_Pending;
         Progress := True;

      when Rekey_Init_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         Items (Idx).Nr_Of_Requests_Preponed := 0;
         Items (Idx).State := Prepone_Requests_Pending;
         Progress := True;

      when Rekey_VBA_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         if Items (Idx).Request_Finished then

            Request.Success (Items (Idx).Req, True);
            Items (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Items (Idx).Nr_Of_Requests_Preponed := 0;
            Items (Idx).State := Prepone_Requests_Pending;
            Progress := True;

         end if;

      when Prepone_Requests_Pending =>

         Declare_Requests_Preponed :
         declare
            Requests_Preponed : Boolean := False;
         begin

            Try_Prepone_Requests :
            loop

               exit Try_Prepone_Requests when
                  Items (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Items (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Items (Idx).Nr_Of_Requests_Preponed :=
                        Items (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Items (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Rekey_VBA,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := Rekey_VBA_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Rekey;

   --
   --  Execute_Decrypt_Keys
   --
   procedure Execute_Decrypt_Keys (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Items (Idx).State is
      when Submitted =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Decrypt_Keys,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := Decrypt_Keys_Pending;
         Progress := True;

      when Decrypt_Keys_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         Items (Idx).State := Complete;
         Index_Queue.Dequeue (Indices, Idx);
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Decrypt_Keys;

   --
   --  Execute_Deinitialize
   --
   procedure Execute_Deinitialize (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Items (Idx).State is
      when Submitted =>

         Items (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Deinitialize,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Items (Idx).State := Deinitialize_SB_Ctrl_Pending;
         Progress := True;

      when Deinitialize_SB_Ctrl_Complete =>

         if not Primitive.Success (Items (Idx).Prim) then
            raise Program_Error;
         end if;

         Request.Success (Items (Idx).Req, True);
         Items (Idx).State := Complete;
         Index_Queue.Dequeue (Indices, Idx);
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Deinitialize;

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin

      if not Index_Queue.Empty (Obj.Indices) then

         Declare_Idx :
         declare
            Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         begin

            case Request.Operation (Obj.Items (Idx).Req) is
            when Rekey =>

               Execute_Rekey (Obj.Items, Obj.Indices, Idx, Progress);

            when Extend_VBD =>

               Execute_Extend_VBD (Obj.Items, Obj.Indices, Idx, Progress);

            when Extend_FT =>

               Execute_Extend_FT (Obj.Items, Obj.Indices, Idx, Progress);

            when Decrypt_Keys =>

               Execute_Decrypt_Keys (Obj.Items, Obj.Indices, Idx, Progress);

            when Deinitialize =>

               Execute_Deinitialize (Obj.Items, Obj.Indices, Idx, Progress);

            when others =>

               null;

            end case;

         end Declare_Idx;

      end if;

   end Execute;

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
         Index_Queue.Head (Obj.Indices) /= Idx
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Items (Idx).Req) is
      when Extend_VBD | Extend_FT | Resume_Rekeying =>

         raise Program_Error;

      when Rekey =>

         case Obj.Items (Idx).State is
         when Rekey_Init_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := Rekey_Init_Complete;

         when others =>

            raise Program_Error;

         end case;

      when Decrypt_Keys =>

         case Obj.Items (Idx).State is
         when Decrypt_Keys_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := Decrypt_Keys_Complete;

         when others =>

            raise Program_Error;

         end case;

      when Deinitialize =>

         case Obj.Items (Idx).State is
         when Deinitialize_SB_Ctrl_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := Deinitialize_SB_Ctrl_Complete;

         when others =>

            raise Program_Error;

         end case;

      when Read | Write | Sync | Create_Snapshot | Discard_Snapshot =>

         if Obj.Items (Idx).State /= In_Progress then
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

      end case;

   end Mark_Generated_Primitive_Complete;

   --
   --  Mark_Generated_Primitive_Complete_Req_Fin
   --
   procedure Mark_Generated_Primitive_Complete_Req_Fin (
      Obj              : in out Object_Type;
      Idx              :        Pool_Index_Type;
      Success          :        Boolean;
      Request_Finished :        Boolean)
   is
   begin

      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Items (Idx).Req) is
      when Rekey =>

         case Obj.Items (Idx).State is
         when Rekey_VBA_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := Rekey_VBA_Complete;
            Obj.Items (Idx).Request_Finished := Request_Finished;

         when others =>

            raise Program_Error;

         end case;

      when Extend_VBD =>

         case Obj.Items (Idx).State is
         when VBD_Extension_Step_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := VBD_Extension_Step_Complete;
            Obj.Items (Idx).Request_Finished := Request_Finished;

         when others =>

            raise Program_Error;

         end case;

      when Extend_FT =>

         case Obj.Items (Idx).State is
         when FT_Extension_Step_In_Progress =>

            Primitive.Success (Obj.Items (Idx).Prim, Success);
            Obj.Items (Idx).State := FT_Extension_Step_Complete;
            Obj.Items (Idx).Request_Finished := Request_Finished;

         when others =>

            raise Program_Error;

         end case;

      when others =>

         raise Program_Error;

      end case;

   end Mark_Generated_Primitive_Complete_Req_Fin;

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
         if Request.Valid (Obj.Items (Idx).Req)
            and then Request.Equal (Obj.Items (Idx).Req, Req)
         then
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
      when
         Sync |
         Create_Snapshot |
         Discard_Snapshot |
         Rekey |
         Extend_VBD |
         Extend_FT |
         Decrypt_Keys |
         Deinitialize |
         Resume_Rekeying
      =>
         1);

   --
   --  Item_Invalid
   --
   function Item_Invalid
   return Item_Type
   is (
      State                   => Invalid,
      Req                     => Request.Invalid_Object,
      Snap_ID                 => 0,
      Prim                    => Primitive.Invalid_Object,
      Request_Finished        => Boolean'First,
      Nr_Of_Requests_Preponed => 0,
      Nr_Of_Prims_Completed   => 0);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Items   => (others => Item_Invalid),
      Indices => Index_Queue.Empty_Queue);

end CBE.Pool;
