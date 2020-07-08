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

package body CBE.Request_Pool
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
      Loop_Jobs :
      for Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Idx).State = Invalid then

            case Request.Operation (Req) is
            when Initialize =>

               raise Program_Error;

            when
               Rekey |
               Extend_VBD |
               Extend_FT |
               Deinitialize
            =>

               Obj.Jobs (Idx).State := Submitted;
               Obj.Jobs (Idx).Req   := Req;
               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            when Resume_Rekeying =>

               Obj.Jobs (Idx).State := Submitted_Resume_Rekeying;
               Obj.Jobs (Idx).Req   := Request.Valid_Object (
                  Op     => Rekey,
                  Succ   => False,
                  Blk_Nr => 0,
                  Off    => 0,
                  Cnt    => 0,
                  Key    => 0,
                  Tg     => 0);

               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            when Read | Write | Sync | Create_Snapshot | Discard_Snapshot =>

               Obj.Jobs (Idx) := (
                  State                   => Pending,
                  Req                     => Req,
                  Snap_ID                 => Snap_ID,
                  Prim                    => Primitive.Invalid_Object,
                  Request_Finished        => Boolean'First,
                  Nr_Of_Requests_Preponed => 0,
                  Nr_Of_Prims_Completed   => 0,
                  SB_State                => Superblock_State_Type'First);

               Request.Success (Obj.Jobs (Idx).Req, True);
               Index_Queue.Enqueue (Obj.Indices, Idx);
               return;

            end case;

         end if;

      end loop Loop_Jobs;

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

      Declare_Job :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Job : constant Job_Type := Obj.Jobs (Idx);
      begin
         if Job.State /= Pending or else
            Request.Operation (Job.Req) /= Discard_Snapshot
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Read,
               Request.Success (Job.Req),
               Primitive.Tag_Pool_Discard_Snap,
               Idx,
               Request.Block_Number (Job.Req) +
                  Block_Number_Type (Job.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Job.Nr_Of_Prims_Completed));

      end Declare_Job;
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

      Declare_Job :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Job : constant Job_Type := Obj.Jobs (Idx);
      begin
         if Job.State /= Pending or else
            Request.Operation (Job.Req) /= Create_Snapshot
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Read,
               Request.Success (Job.Req),
               Primitive.Tag_Pool_Create_Snap,
               Idx,
               Request.Block_Number (Job.Req) +
                  Block_Number_Type (Job.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Job.Nr_Of_Prims_Completed));

      end Declare_Job;
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

      Declare_Job :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Job : constant Job_Type := Obj.Jobs (Idx);
      begin
         if Job.State /= Pending or else
            Request.Operation (Job.Req) /= Sync
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Sync,
               Request.Success (Job.Req),
               Primitive.Tag_Pool_Sync,
               Idx,
               Request.Block_Number (Job.Req) +
                  Block_Number_Type (Job.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Job.Nr_Of_Prims_Completed));

      end Declare_Job;
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

      Declare_Job :
      declare
         Idx : constant Pool_Index_Type := Index_Queue.Head (Obj.Indices);
         Job : constant Job_Type := Obj.Jobs (Idx);
      begin
         if Job.State /= Pending or else (
               Request.Operation (Job.Req) /= Read and then
               Request.Operation (Job.Req) /= Write)
         then
            return Primitive.Invalid_Object;
         end if;

         return
            Primitive.Valid_Object (
               Prim_Op_From_Req_Op (Request.Operation (Job.Req)),
               Request.Success (Job.Req),
               Primitive.Tag_Pool_VBD,
               Idx,
               Request.Block_Number (Job.Req) +
                  Block_Number_Type (Job.Nr_Of_Prims_Completed),
               Primitive.Index_Type (Job.Nr_Of_Prims_Completed));

      end Declare_Job;
   end Peek_Generated_VBD_Primitive;

   --
   --  Peek_Generated_SB_Ctrl_Primitive
   --
   function Peek_Generated_SB_Ctrl_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      if not Index_Queue.Empty (Obj.Indices) then

         Declare_Job :
         declare
            Job : constant Job_Type :=
               Obj.Jobs (Index_Queue.Head (Obj.Indices));
         begin

            case Job.State is
            when
               Rekey_Init_Pending |
               Rekey_VBA_Pending |
               VBD_Extension_Step_Pending |
               FT_Extension_Step_Pending |
               Initialize_SB_Ctrl_Pending |
               Deinitialize_SB_Ctrl_Pending
            =>

               return Job.Prim;

            when others =>

               return Primitive.Invalid_Object;

            end case;

         end Declare_Job;

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
         Obj.Jobs (Idx).State /= Pending or else (
            Request.Operation (Obj.Jobs (Idx).Req) /= Read and then
            Request.Operation (Obj.Jobs (Idx).Req) /= Write)
      then
         raise Program_Error;
      end if;

      return Obj.Jobs (Idx).Snap_ID;

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

      case Obj.Jobs (Idx).State is
      when
         VBD_Extension_Step_Pending |
         FT_Extension_Step_Pending
      =>

         if Primitive.Equal (Prim, Obj.Jobs (Idx).Prim) then
            return Number_Of_Blocks_Type (
               Request.Count (Obj.Jobs (Idx).Req));
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
         case Obj.Jobs (Idx).State is
         when Pending =>
            Obj.Jobs (Idx).State := In_Progress;
            return;
         when Rekey_Init_Pending =>
            Obj.Jobs (Idx).State := Rekey_Init_In_Progress;
            return;
         when Rekey_VBA_Pending =>
            Obj.Jobs (Idx).State := Rekey_VBA_In_Progress;
            return;
         when VBD_Extension_Step_Pending =>
            Obj.Jobs (Idx).State := VBD_Extension_Step_In_Progress;
            return;
         when FT_Extension_Step_Pending =>
            Obj.Jobs (Idx).State := FT_Extension_Step_In_Progress;
            return;
         when Initialize_SB_Ctrl_Pending =>
            Obj.Jobs (Idx).State := Initialize_SB_Ctrl_In_Progress;
            return;
         when Deinitialize_SB_Ctrl_Pending =>
            Obj.Jobs (Idx).State := Deinitialize_SB_Ctrl_In_Progress;
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
      Jobs    : in out Jobs_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Jobs (Idx).State is
      when Submitted =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := VBD_Extension_Step_Pending;
         Progress := True;

      when VBD_Extension_Step_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         if Jobs (Idx).Request_Finished then

            Request.Success (Jobs (Idx).Req, True);
            Jobs (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Jobs (Idx).Nr_Of_Requests_Preponed := 0;
            Jobs (Idx).State := Prepone_Requests_Pending;
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
                  Jobs (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Jobs (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Jobs (Idx).Nr_Of_Requests_Preponed :=
                        Jobs (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Jobs (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := VBD_Extension_Step_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Extend_VBD;

   --
   --  Execute_Extend_FT
   --
   procedure Execute_Extend_FT (
      Jobs    : in out Jobs_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Jobs (Idx).State is
      when Submitted =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := FT_Extension_Step_Pending;
         Progress := True;

      when FT_Extension_Step_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         if Jobs (Idx).Request_Finished then

            Request.Success (Jobs (Idx).Req, True);
            Jobs (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Jobs (Idx).Nr_Of_Requests_Preponed := 0;
            Jobs (Idx).State := Prepone_Requests_Pending;
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
                  Jobs (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Jobs (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Jobs (Idx).Nr_Of_Requests_Preponed :=
                        Jobs (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Jobs (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := FT_Extension_Step_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Extend_FT;

   --
   --  Execute_Rekey
   --
   procedure Execute_Rekey (
      Jobs    : in out Jobs_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Jobs (Idx).State is
      when Submitted =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Init_Rekey,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := Rekey_Init_Pending;
         Progress := True;

      when Submitted_Resume_Rekeying =>

         Jobs (Idx).Prim := Primitive.Invalid_Object;

         Jobs (Idx).Nr_Of_Requests_Preponed := 0;
         Jobs (Idx).State := Prepone_Requests_Pending;
         Progress := True;

      when Rekey_Init_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         Jobs (Idx).Nr_Of_Requests_Preponed := 0;
         Jobs (Idx).State := Prepone_Requests_Pending;
         Progress := True;

      when Rekey_VBA_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         if Jobs (Idx).Request_Finished then

            Request.Success (Jobs (Idx).Req, True);
            Jobs (Idx).State := Complete;
            Index_Queue.Dequeue (Indices, Idx);
            Progress := True;

         else

            Jobs (Idx).Nr_Of_Requests_Preponed := 0;
            Jobs (Idx).State := Prepone_Requests_Pending;
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
                  Jobs (Idx).Nr_Of_Requests_Preponed >=
                     Max_Nr_Of_Requests_Preponed_At_A_Time or else
                  Index_Queue.Item_Is_Tail (Indices, Idx);

               declare
                  Next_Idx : constant Pool_Index_Type :=
                     Index_Queue.Next_Item (Indices, Idx);
               begin

                  case Request.Operation (Jobs (Next_Idx).Req) is
                  when Read | Write | Sync | Discard_Snapshot =>

                     Index_Queue.Move_One_Item_Towards_Tail (Indices, Idx);
                     Jobs (Idx).Nr_Of_Requests_Preponed :=
                        Jobs (Idx).Nr_Of_Requests_Preponed + 1;

                     Requests_Preponed := True;
                     Progress := True;

                  when others =>

                     exit Try_Prepone_Requests;

                  end case;

               end;

            end loop Try_Prepone_Requests;

            if not Requests_Preponed then

               Jobs (Idx).State := Prepone_Requests_Complete;
               Progress := True;

            end if;

         end Declare_Requests_Preponed;

      when Prepone_Requests_Complete =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Rekey_VBA,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := Rekey_VBA_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Rekey;

   --
   --  Execute_Initialize
   --
   procedure Execute_Initialize (
      Jobs    : in out Jobs_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Jobs (Idx).State is
      when Submitted =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Initialize,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := Initialize_SB_Ctrl_Pending;
         Progress := True;

      when Initialize_SB_Ctrl_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         case Jobs (Idx).SB_State is
         when Normal =>

            Index_Queue.Dequeue (Indices, Idx);
            Jobs (Idx) := Job_Invalid;
            Progress := True;

         when Rekeying =>

            Jobs (Idx).State := Submitted_Resume_Rekeying;
            Jobs (Idx).Req   := Request.Valid_Object (
               Op     => Rekey,
               Succ   => False,
               Blk_Nr => 0,
               Off    => 0,
               Cnt    => 0,
               Key    => 0,
               Tg     => 0);

            Index_Queue.Enqueue (Indices, Idx);
            Progress := True;

         when Extending_VBD =>

            Jobs (Idx).State := Submitted;
            Jobs (Idx).Req   :=
               Request.Valid_Object (
                  Op     => Extend_VBD,
                  Succ   => False,
                  Blk_Nr => 0,
                  Off    => 0,
                  Cnt    => 0,
                  Key    => 0,
                  Tg     => 0);

            Index_Queue.Enqueue (Indices, Idx);
            Progress := True;

         when Extending_FT =>

            Jobs (Idx).State := Submitted;
            Jobs (Idx).Req   :=
               Request.Valid_Object (
                  Op     => Extend_FT,
                  Succ   => False,
                  Blk_Nr => 0,
                  Off    => 0,
                  Cnt    => 0,
                  Key    => 0,
                  Tg     => 0);

            Index_Queue.Enqueue (Indices, Idx);
            Progress := True;

         end case;

      when others =>

         null;

      end case;

   end Execute_Initialize;

   --
   --  Execute_Deinitialize
   --
   procedure Execute_Deinitialize (
      Jobs    : in out Jobs_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Jobs (Idx).State is
      when Submitted =>

         Jobs (Idx).Prim := Primitive.Valid_Object (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Pool_SB_Ctrl_Deinitialize,
            Pl_Idx => Idx,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type'First);

         Jobs (Idx).State := Deinitialize_SB_Ctrl_Pending;
         Progress := True;

      when Deinitialize_SB_Ctrl_Complete =>

         if not Primitive.Success (Jobs (Idx).Prim) then
            raise Program_Error;
         end if;

         Request.Success (Jobs (Idx).Req, True);
         Jobs (Idx).State := Complete;
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

            case Request.Operation (Obj.Jobs (Idx).Req) is
            when Rekey =>

               Execute_Rekey (Obj.Jobs, Obj.Indices, Idx, Progress);

            when Extend_VBD =>

               Execute_Extend_VBD (Obj.Jobs, Obj.Indices, Idx, Progress);

            when Extend_FT =>

               Execute_Extend_FT (Obj.Jobs, Obj.Indices, Idx, Progress);

            when Initialize =>

               Execute_Initialize (Obj.Jobs, Obj.Indices, Idx, Progress);

            when Deinitialize =>

               Execute_Deinitialize (Obj.Jobs, Obj.Indices, Idx, Progress);

            when others =>

               null;

            end case;

         end Declare_Idx;

      end if;

   end Execute;

   --
   --  Mark_Generated_Primitive_Complete_SB_State
   --
   procedure Mark_Generated_Primitive_Complete_SB_State (
      Obj      : in out Object_Type;
      Idx      :        Pool_Index_Type;
      Success  :        Boolean;
      SB_State :        Superblock_State_Type)
   is
   begin

      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Jobs (Idx).Req) is
      when Initialize =>

         case Obj.Jobs (Idx).State is
         when Initialize_SB_Ctrl_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := Initialize_SB_Ctrl_Complete;
            Obj.Jobs (Idx).SB_State := SB_State;

         when others =>

            raise Program_Error;

         end case;

      when others =>

         raise Program_Error;

      end case;

   end Mark_Generated_Primitive_Complete_SB_State;

   --
   --  Mark_Generated_Primitive_Complete_Generation
   --
   procedure Mark_Generated_Primitive_Complete_Generation (
      Obj     : in out Object_Type;
      Idx     :        Pool_Index_Type;
      Success :        Boolean;
      Gen     :        Generation_Type)
   is
   begin

      if Index_Queue.Empty (Obj.Indices) or else
         Index_Queue.Head (Obj.Indices) /= Idx
      then
         raise Program_Error;
      end if;

      case Request.Operation (Obj.Jobs (Idx).Req) is
      when Create_Snapshot =>

         if Obj.Jobs (Idx).State /= In_Progress then
            raise Program_Error;
         end if;

         Request.Success (Obj.Jobs (Idx).Req, Success);
         Request.Offset (Obj.Jobs (Idx).Req, Request.Offset_Type (Gen));

         Obj.Jobs (Idx).Nr_Of_Prims_Completed :=
            Obj.Jobs (Idx).Nr_Of_Prims_Completed + 1;

         if Obj.Jobs (Idx).Nr_Of_Prims_Completed <
               Job_Nr_Of_Prims (Obj.Jobs (Idx))
         then
            Obj.Jobs (Idx).State := Pending;
         else
            Obj.Jobs (Idx).State := Complete;
            Index_Queue.Dequeue (Obj.Indices, Idx);
         end if;

      when others =>

         raise Program_Error;

      end case;

   end Mark_Generated_Primitive_Complete_Generation;

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

      case Request.Operation (Obj.Jobs (Idx).Req) is
      when Rekey =>

         case Obj.Jobs (Idx).State is
         when Rekey_Init_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := Rekey_Init_Complete;

         when others =>

            raise Program_Error;

         end case;

      when Deinitialize =>

         case Obj.Jobs (Idx).State is
         when Deinitialize_SB_Ctrl_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := Deinitialize_SB_Ctrl_Complete;

         when others =>

            raise Program_Error;

         end case;

      when Read | Write | Sync | Discard_Snapshot =>

         if Obj.Jobs (Idx).State /= In_Progress then
            raise Program_Error;
         end if;

         Request.Success (Obj.Jobs (Idx).Req, Success);
         Obj.Jobs (Idx).Nr_Of_Prims_Completed :=
            Obj.Jobs (Idx).Nr_Of_Prims_Completed + 1;

         if Obj.Jobs (Idx).Nr_Of_Prims_Completed <
               Job_Nr_Of_Prims (Obj.Jobs (Idx))
         then
            Obj.Jobs (Idx).State := Pending;
         else
            Obj.Jobs (Idx).State := Complete;
            Index_Queue.Dequeue (Obj.Indices, Idx);
         end if;

      when others =>

         raise Program_Error;

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

      case Request.Operation (Obj.Jobs (Idx).Req) is
      when Rekey =>

         case Obj.Jobs (Idx).State is
         when Rekey_VBA_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := Rekey_VBA_Complete;
            Obj.Jobs (Idx).Request_Finished := Request_Finished;

         when others =>

            raise Program_Error;

         end case;

      when Extend_VBD =>

         case Obj.Jobs (Idx).State is
         when VBD_Extension_Step_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := VBD_Extension_Step_Complete;
            Obj.Jobs (Idx).Request_Finished := Request_Finished;

         when others =>

            raise Program_Error;

         end case;

      when Extend_FT =>

         case Obj.Jobs (Idx).State is
         when FT_Extension_Step_In_Progress =>

            Primitive.Success (Obj.Jobs (Idx).Prim, Success);
            Obj.Jobs (Idx).State := FT_Extension_Step_Complete;
            Obj.Jobs (Idx).Request_Finished := Request_Finished;

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
   function Peek_Completed_Request (Obj : Object_Type)
   return Request.Object_Type
   is
   begin
      For_Each_Job_Idx :
      for Idx in Obj.Jobs'Range loop
         if Obj.Jobs (Idx).State = Complete then
            return Obj.Jobs (Idx).Req;
         end if;
      end loop For_Each_Job_Idx;
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
      For_Each_Job_Idx :
      for Idx in Obj.Jobs'Range loop
         if Request.Valid (Obj.Jobs (Idx).Req)
            and then Request.Equal (Obj.Jobs (Idx).Req, Req)
         then
            if Obj.Jobs (Idx).State /= Complete then
               raise Program_Error;
            end if;
            Obj.Jobs (Idx) := Job_Invalid;
            return;
         end if;
      end loop For_Each_Job_Idx;
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
      if Obj.Jobs (Idx).State = Invalid then
         raise Program_Error;
      end if;
      return Obj.Jobs (Idx).Req;
   end Request_For_Index;

   --
   --  Job_Nr_Of_Prims
   --
   function Job_Nr_Of_Prims (Job : Job_Type)
   return Number_Of_Primitives_Type
   is (
      case Request.Operation (Job.Req) is
      when Read | Write =>
         Number_Of_Primitives_Type (Request.Count (Job.Req)),
      when
         Sync |
         Create_Snapshot |
         Discard_Snapshot |
         Rekey |
         Extend_VBD |
         Extend_FT |
         Initialize |
         Deinitialize |
         Resume_Rekeying
      =>
         1);

   --
   --  Job_Invalid
   --
   function Job_Invalid
   return Job_Type
   is (
      State                   => Invalid,
      Req                     => Request.Invalid_Object,
      Snap_ID                 => 0,
      Prim                    => Primitive.Invalid_Object,
      Request_Finished        => Boolean'First,
      Nr_Of_Requests_Preponed => 0,
      Nr_Of_Prims_Completed   => 0,
      SB_State                => Superblock_State_Type'First);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is
      Obj : Object_Type;
      Idx : constant Pool_Index_Type := Pool_Index_Type'First;
   begin

      Obj := (
         Jobs   => (others => Job_Invalid),
         Indices => Index_Queue.Empty_Queue);

      Obj.Jobs (Idx).State := Submitted;
      Obj.Jobs (Idx).Req   :=
         Request.Valid_Object (
            Op     => Initialize,
            Succ   => False,
            Blk_Nr => 0,
            Off    => 0,
            Cnt    => 0,
            Key    => 0,
            Tg     => 0);

      Index_Queue.Enqueue (Obj.Indices, Idx);
      return Obj;

   end Initialized_Object;

end CBE.Request_Pool;
