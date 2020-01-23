--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Tree_Helper;
with CBE.Debug;
with SHA256_4K;

package body CBE.Library
with SPARK_Mode
is
   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type)
   is
      SHA_Idx : SHA256_4K.Hash_Index_Type := SHA256_4K.Hash_Index_Type'First;
   begin
      for CBE_Idx in CBE_Hash'Range loop
         CBE_Hash (CBE_Idx) := Byte_Type (SHA_Hash (SHA_Idx));
         if CBE_Idx < CBE_Hash'Last then
            SHA_Idx := SHA_Idx + 1;
         end if;
      end loop;
   end CBE_Hash_From_SHA256_4K_Hash;

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type)
   is
      CBE_Idx : Block_Data_Index_Type := Block_Data_Index_Type'First;
   begin
      for SHA_Idx in SHA_Data'Range loop
         SHA_Data (SHA_Idx) := SHA256_4K.Byte (CBE_Data (CBE_Idx));
         if SHA_Idx < SHA_Data'Last then
            CBE_Idx := CBE_Idx + 1;
         end if;
      end loop;
   end SHA256_4K_Data_From_CBE_Data;

   procedure Try_Discard_Snapshot (
      Snaps     : in out Snapshots_Type;
      Keep_Snap :        Snapshots_Index_Type;
      Success   :    out Boolean)
   is
      Discard_Idx       : Snapshots_Index_Type := Snapshots_Index_Type'First;
      Discard_Idx_Valid : Boolean              := False;
   begin
      For_Snapshots :
      for Idx in Snapshots_Index_Type loop
         if
            Idx /= Keep_Snap and then
            Snaps (Idx).Valid and then
            not Snaps (Idx).Keep and then (
               not Discard_Idx_Valid or else
               Snaps (Idx).ID < Snaps (Discard_Idx).ID)
         then
            Discard_Idx       := Idx;
            Discard_Idx_Valid := True;
         end if;
      end loop For_Snapshots;
      if Discard_Idx_Valid then
         Snaps (Discard_Idx) := Snapshot_Invalid;
      end if;
      Success := Discard_Idx_Valid;
   end Try_Discard_Snapshot;

   procedure Discard_Snapshot (
      Obj     : in out Object_Type;
      Snap_ID :        Generation_Type;
      Success :    out Boolean)
   is
   begin
      Success := False;

      Loop_Discard_Snapshot :
      for I in Snapshots_Index_Type loop
         if Obj.Superblock.Snapshots (I).Valid and then
            Obj.Superblock.Snapshots (I).Keep and then
            Obj.Superblock.Snapshots (I).Gen = Snap_ID
         then
            Obj.Superblock.Snapshots (I).Valid := False;
            Success := True;
            Obj.Secure_Superblock := True;
            exit Loop_Discard_Snapshot;
         end if;
      end loop Loop_Discard_Snapshot;
   end Discard_Snapshot;

   procedure Try_Flush_Cache_If_Dirty (
      Obj   : in out Object_Type;
      Dirty :    out Boolean)
   is
   begin
      Dirty := Cache.Dirty (Obj.Cache_Obj);

      if Dirty and then
         Obj.Cache_Sync_State = Inactive
      then

         if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

            Cache.Submit_Primitive_Without_Data (
               Obj.Cache_Obj,
               Primitive.Valid_Object_No_Pool_Idx (
                  Sync, False, Primitive.Tag_Lib_Cache_Sync, 0, 0));

            Obj.Cache_Sync_State := Active;

         end if;

      end if;
   end Try_Flush_Cache_If_Dirty;

   procedure Create_Snapshot (
      Obj     : in out Object_Type;
      Quara   :        Boolean;
      Snap_ID :    out Generation_Type;
      Result  :    out Boolean)
   is
   begin

      --
      --  Initially assume creation will be unsuccessfull and will
      --  only be changed when a snapshot creation was started.
      --
      Result := False;

      --
      --  As long as we are creating a snapshot or
      --  the superblock the new snapshot belongs to is being
      --  written we do not allow the creation of a new
      --  snapshot.
      --
      if not Obj.Creating_Snapshot or else
         not Obj.Secure_Superblock
      then
         Declare_Current_Snapshot :
         declare
            Snap : constant Snapshot_Type :=
               Obj.Superblock.Snapshots (Curr_Snap (Obj));
         begin
            --  if Obj.Last_Secured_Generation = Obj.Cur_Gen then
            if Snap.PBA = Obj.Last_Root_PBA and then
               Snap.Hash = Obj.Last_Root_Hash
            then
               Snap_ID := Obj.Last_Secured_Generation;

               pragma Debug (Debug.Print_String ("Creating_Snapshot: "
                  & "generation already secured - no new snapshot: "
                  & Debug.To_String (Debug.Uint64_Type (Snap_ID))));
               return;
            end if;

            Obj.Creating_Snapshot := True;
            Obj.Creating_Quarantine_Snapshot := Quara;

            Result := True;
         end Declare_Current_Snapshot;
      end if;
      Snap_ID := Obj.Cur_Gen;

      pragma Debug (Debug.Print_String ("Creating_Snapshot: id: "
         & Debug.To_String (Debug.Uint64_Type (Snap_ID))));
   end Create_Snapshot;

   procedure Create_Snapshot_Internal (
      Obj      : in out Object_Type;
      Progress :    out Boolean)
   is
   begin
      if Obj.Cache_Sync_State = Active or else
         Obj.Secure_Superblock
      then
         pragma Debug (Debug.Print_String ("Create_Snapshot_Internal: "
             & "flusher active: False "
             & Debug.To_String (Obj.Cache_Sync_State = Active) & " "
             & "Secure_Superblock: "
             & Debug.To_String (Obj.Secure_Superblock)));
         Progress := False;
         return;
      end if;

      Declare_Cache_Dirty :
      declare
         Cache_Dirty : Boolean;
      begin

         Try_Flush_Cache_If_Dirty (Obj, Cache_Dirty);

         --
         --  In case we have to flush the Cache, wait until we have
         --  finished doing that.
         --
         if not Cache_Dirty then
            pragma Debug (Debug.Print_String ("Create_Snapshot_Internal: "
               & "snapshot created: "
               & "gen: " & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))));

            Obj.Superblock.Snapshots (Curr_Snap (Obj)).Keep :=
               Obj.Creating_Quarantine_Snapshot;

            --  Obj.Cur_Gen := Obj.Cur_Gen  + 1;
            --  trigger securing of suerblock
            --  Obj.Creating_Snapshot := False;
            Obj.Secure_Superblock := True;
         end if;
         Progress := True;
      end Declare_Cache_Dirty;
   end Create_Snapshot_Internal;

   function Snapshot_Creation_Complete (
      Obj     : Object_Type;
      Snap_ID : Generation_Type)
   return Boolean
   is
      Result : constant Boolean := (Obj.Last_Secured_Generation = Snap_ID);
   begin
      pragma Debug (Debug.Print_String ("Snapshot_Creation_Complete: " &
         Debug.To_String (Debug.Uint64_Type (Obj.Last_Secured_Generation))
         & " = "
         & Debug.To_String (Debug.Uint64_Type (Snap_ID)) & " "
         & " result: " & Debug.To_String (Result)));

      return Result;
   end Snapshot_Creation_Complete;

   procedure Active_Snapshot_IDs (
      Obj  :     Object_Type;
      List : out Active_Snapshot_IDs_Type)
   is
   begin
      For_Snapshots :
      for Snap_ID in Snapshots_Index_Type loop

         if Obj.Superblock.Snapshots (Snap_ID).Valid and then
            Obj.Superblock.Snapshots (Snap_ID).Keep
         then
            List (Snap_ID) := Obj.Superblock.Snapshots (Snap_ID).Gen;
         else
            List (Snap_ID) := Generation_Type (0);
         end if;
      end loop For_Snapshots;
   end Active_Snapshot_IDs;

   procedure Initialize_Object (
      Obj     : out Object_Type;
      SBs     :     Superblocks_Type;
      Curr_SB :     Superblocks_Index_Type)
   is
      Curr_Snap : constant Snapshots_Index_Type := SBs (Curr_SB).Curr_Snap;
      Degree : constant Tree_Degree_Type := SBs (Curr_SB).Degree;
      Max_Level : constant Tree_Level_Index_Type :=
         SBs (Curr_SB).Snapshots (Curr_Snap).Max_Level;

      Leafs : constant Tree_Number_Of_Leafs_Type :=
         SBs (Curr_SB).Snapshots (Curr_Snap).Nr_Of_Leafs;
   begin
      Obj.Execute_Progress        := False;
      Obj.Request_Pool_Obj        := Pool.Initialized_Object;
      Obj.Splitter_Obj            := Splitter.Initialized_Object;
      Obj.Crypto_Obj              := Crypto.Initialized_Object;

      Obj.IO_Obj                  := Block_IO.Initialized_Object;

      Cache.Initialize (Obj.Cache_Obj);
      Obj.Cache_Jobs_Data := (others => (others => 0));
      Obj.Cache_Slots_Data := (others => (others => 0));
      Obj.Cache_Sync_State := Inactive;

      Obj.Trans_Data              := (others => (others => 0));
      Obj.VBD                     :=
         Virtual_Block_Device.Initialized_Object (Max_Level, Degree, Leafs);

      Obj.Write_Back_Obj          := Write_Back.Initialized_Object;
      Obj.Write_Back_Data         := (others => (others => 0));
      Obj.Sync_SB_Obj             := Sync_Superblock.Initialized_Object;

      if SBs (Curr_SB).Free_Max_Level < Free_Tree_Min_Max_Level then
         raise Program_Error;
      end if;

      New_Free_Tree.Initialized_Object (Obj.New_Free_Tree_Obj);
      Obj.New_Free_Tree_Prim := Primitive.Invalid_Object;
      Meta_Tree.Initialized_Object (Obj.Meta_Tree_Obj);

      Obj.Free_Tree_Retry_Count   := 0;

      Obj.Secure_Superblock            := False;
      Obj.Wait_For_Front_End           := Wait_For_Event_Invalid;
      Obj.Creating_Snapshot            := False;
      Obj.Creating_Quarantine_Snapshot := False;
      Obj.Stall_Snapshot_Creation      := False;

      Obj.Superblock := SBs (Curr_SB);
      Obj.Superblock.Superblock_ID := Obj.Superblock.Superblock_ID + 1;
      Obj.Last_Secured_Generation := Obj.Superblock.Last_Secured_Generation;
      Obj.Cur_Gen := Obj.Last_Secured_Generation + 1;
      Obj.Last_Root_PBA :=
         Obj.Superblock.Snapshots (Obj.Superblock.Curr_Snap).PBA;
      Obj.Last_Root_Hash :=
         Obj.Superblock.Snapshots (Obj.Superblock.Curr_Snap).Hash;
      Obj.Cur_SB := Curr_SB;
      Obj.Cur_SB := Advance_Superblocks_Index (Obj.Cur_SB);

      Obj.Sync_Primitive := Primitive.Invalid_Object;

      Obj.SCD_State       := Inactive;
      Obj.SCD_Req         := Request.Invalid_Object;
      Obj.SCD_Data        := (others => 0);
      Obj.SCD_Curr_Lvl    := 1;
      Obj.SCD_New_PBAs    := (others => 0);
      Obj.SCD_New_Blocks  := 0;
      Obj.SCD_Free_PBAs   := (others => 0);
      Obj.SCD_Free_Blocks := 0;

      Obj.SCD_Cache_Prim := Primitive.Invalid_Object;
      Obj.SCD_Cache_Prim_State := Invalid;
      Obj.SCD_Cache_Prim_Data := (others => 0);

      Obj.WB_Update_PBA := 0;

      Obj.WB_Cache_Prim_1 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_1_State := Invalid;
      Obj.WB_Cache_Prim_1_Data := (others => 0);

      Obj.WB_Cache_Prim_2 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_2_State := Invalid;
      Obj.WB_Cache_Prim_2_Data := (others => 0);

      Obj.WB_Cache_Prim_3 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_3_State := Invalid;
      Obj.WB_Cache_Prim_3_Data := (others => 0);

      Obj.WB_Prim := Primitive.Invalid_Object;

      declare
         Next_Snap : constant Snapshots_Index_Type :=
            Next_Snap_Slot (Obj);
      begin
         Obj.Superblock.Snapshots (Next_Snap) :=
            Obj.Superblock.Snapshots (Obj.Superblock.Curr_Snap);
         --
         --  Clear flags to prevent creating a quarantine snapshot
         --  unintentionally.
         --
         Obj.Superblock.Snapshots (Next_Snap).Keep := False;
         Obj.Superblock.Snapshots (Next_Snap).Gen := Obj.Cur_Gen - 1;
         Obj.Superblock.Curr_Snap := Next_Snap;
      end;

      pragma Debug (Debug.Print_String ("Initial SB state: "));
      pragma Debug (Debug.Dump_Superblock (Obj.Cur_SB, Obj.Superblock));

   end Initialize_Object;

   function Curr_Snap (Obj : Object_Type)
   return Snapshots_Index_Type
   is (Obj.Superblock.Curr_Snap);

   function Snap_Slot_For_ID (
      Obj : Object_Type;
      ID  : Generation_Type)
   return Snapshots_Index_Type
   is
   begin
      for I in Snapshots_Index_Type loop
         if Obj.Superblock.Snapshots (I).Valid and then
            Obj.Superblock.Snapshots (I).Gen = ID
         then
            return I;
         end if;
      end loop;
      raise Program_Error;
   end Snap_Slot_For_ID;

   function Next_Snap_Slot (Obj : Object_Type)
   return Snapshots_Index_Type
   is
         Next_Snap : Snapshots_Index_Type := Curr_Snap (Obj);
   begin
      --  XXX make sure we end up at the same idx in case
      --  there is no free slot
      Loop_Snap_Slots :
      for Idx in Snapshots_Index_Type loop
         Next_Snap := (
            if Next_Snap < Snapshots_Index_Type'Last then
               Next_Snap + 1
            else
               Snapshots_Index_Type'First);

         exit Loop_Snap_Slots when
            not Obj.Superblock.Snapshots (Next_Snap).Valid or else
            not Obj.Superblock.Snapshots (Next_Snap).Keep;
      end loop Loop_Snap_Slots;
      return Next_Snap;
   end Next_Snap_Slot;

   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type
   is
   begin
      return
         Virtual_Block_Address_Type (
            Obj.Superblock.Snapshots (Curr_Snap (Obj)).Nr_Of_Leafs - 1);
   end Max_VBA;

   procedure Update_Snapshot_Hash (
      WB       :        Write_Back.Object_Type;
      Curr_Gen :        Generation_Type;
      Snap     : in out Snapshot_Type;
      Prim     :        Primitive.Object_Type)
   is
      PBA : constant Physical_Block_Address_Type :=
         Write_Back.Peek_Completed_Root (WB, Prim);
   begin
      Snap.Gen := Curr_Gen;
      Snap.PBA := PBA;
      Write_Back.Peek_Completed_Root_Hash (WB, Prim, Snap.Hash);
   end Update_Snapshot_Hash;

   procedure Execute_VBD (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin
      Virtual_Block_Device.Execute (Obj.VBD, Obj.Trans_Data);
      if Virtual_Block_Device.Execute_Progress (Obj.VBD) then
         Progress := True;
      end if;

      Loop_Generated_Cache_Prims :
      loop
         Prim := Virtual_Block_Device.Peek_Generated_Cache_Primitive (Obj.VBD);

         exit Loop_Generated_Cache_Prims when
            not Primitive.Valid (Prim) or else
            not Cache.Primitive_Acceptable (Obj.Cache_Obj);

         Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

         if Primitive.Operation (Prim) = Write then
            Obj.Cache_Jobs_Data (Job_Idx) :=
               Virtual_Block_Device.Peek_Generated_Cache_Data (Obj.VBD);
         end if;
         Virtual_Block_Device.Drop_Generated_Cache_Primitive (
            Obj.VBD, Prim);

         Progress := True;

      end loop Loop_Generated_Cache_Prims;

   end Execute_VBD;

   procedure Execute_Free_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin

      --------------------------
      --  Free-tree handling  --
      --------------------------

      New_Free_Tree.Execute (
         Obj.New_Free_Tree_Obj,
         Obj.Superblock.Snapshots,
         Obj.Last_Secured_Generation,
         Progress);

      Loop_Generated_Cache_Prims :
      loop
         Prim := New_Free_Tree.Peek_Generated_Cache_Primitive (Obj.New_Free_Tree_Obj);

         exit Loop_Generated_Cache_Prims when
            not Primitive.Valid (Prim) or else
            not Cache.Primitive_Acceptable (Obj.Cache_Obj);

         Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

         if Primitive.Operation (Prim) = Write then
            Obj.Cache_Jobs_Data (Job_Idx) :=
               New_Free_Tree.Peek_Generated_Cache_Data (Obj.New_Free_Tree_Obj,
                  Prim);
         end if;
         New_Free_Tree.Drop_Generated_Cache_Primitive (
            Obj.New_Free_Tree_Obj, Prim);

         Progress := True;

      end loop Loop_Generated_Cache_Prims;

      Loop_Free_Tree_Generated_Meta_Tree_Prims :
      loop
         Declare_Free_Tree_Generated_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               New_Free_Tree.Peek_Generated_Meta_Tree_Primitive (
                  Obj.New_Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Generated_Meta_Tree_Prims when
               not Primitive.Valid (Prim);
            exit Loop_Free_Tree_Generated_Meta_Tree_Prims when
               not Meta_Tree.Request_Acceptable (Obj.Meta_Tree_Obj);

            Meta_Tree.Submit_Request (Obj.Meta_Tree_Obj,
               (PBA => Obj.Superblock.Meta_Number,
                Gen => Obj.Superblock.Meta_Gen,
                Hash => Obj.Superblock.Meta_Hash),
               (Max_Level => Obj.Superblock.Meta_Max_Level,
                Edges     => Obj.Superblock.Meta_Degree,
                Leafs     => Obj.Superblock.Meta_Leafs),
               Obj.Cur_Gen,
               Physical_Block_Address_Type (Primitive.Block_Number (Prim)));

            New_Free_Tree.Drop_Generated_Meta_Tree_Primitive (
               Obj.New_Free_Tree_Obj, Prim);
            Obj.New_Free_Tree_Prim := Prim;
            Progress := True;
         end Declare_Free_Tree_Generated_Prim;
      end loop Loop_Free_Tree_Generated_Meta_Tree_Prims;
   end Execute_Free_Tree;

   procedure Execute_Meta_Tree (
      Obj      : in out Object_Type;
      Progress :    out Boolean)
   is
      Job_Idx : Cache.Jobs_Index_Type;
   begin
      Progress := False;

      Meta_Tree.Execute (Obj.Meta_Tree_Obj, Progress);

      Loop_Generated_Meta_Tree_Primitives :
      loop
         declare
            Prim : constant Primitive.Object_Type :=
               Meta_Tree.Peek_Generated_Cache_Primitive (Obj.Meta_Tree_Obj);
         begin
            exit Loop_Generated_Meta_Tree_Primitives when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

            if Primitive.Operation (Prim) = Write then
               Obj.Cache_Jobs_Data (Job_Idx) :=
                  Meta_Tree.Peek_Generated_Cache_Data (Obj.Meta_Tree_Obj,
                  Prim);
            end if;

            Meta_Tree.Drop_Generated_Cache_Primitive (
               Obj.Meta_Tree_Obj, Prim);

            exit Loop_Generated_Meta_Tree_Primitives;
         end;
      end loop Loop_Generated_Meta_Tree_Primitives;

      Loop_Completed_Meta_Tree_Primitives :
      loop
         declare
            Prim : constant Primitive.Object_Type :=
               Meta_Tree.Peek_Completed_Primitive (Obj.Meta_Tree_Obj);
         begin
            exit Loop_Completed_Meta_Tree_Primitives when
               not Primitive.Valid (Prim);

            Debug.Print_String ("Loop_Completed_Meta_Tree_Primitives: "
               & Primitive.To_String (Prim));

            if Primitive.Has_Tag_FT_MT (Prim) then
               declare
                  Node : constant Type_1_Node_Type :=
                     Meta_Tree.Peek_Completed_Root_Node (Obj.Meta_Tree_Obj,
                        Prim);
               begin
                  Obj.Superblock.Meta_Gen    := Node.Gen;
                  Obj.Superblock.Meta_Number := Node.PBA;
                  Obj.Superblock.Meta_Hash   := Node.Hash;

                  Debug.Print_String ("Loop_Completed_Meta_Tree_Primitives: "
                     & " Node: " & Debug.To_String (Node.PBA)
                     & " Prim: " & Primitive.To_String (Prim));
               end;

               Primitive.Success (Obj.New_Free_Tree_Prim,
                  Primitive.Success (Prim));

               New_Free_Tree.Mark_Generated_Meta_Tree_Primitive_Complete (
                  Obj.New_Free_Tree_Obj, Obj.New_Free_Tree_Prim,
                  Physical_Block_Address_Type (Primitive.Block_Number (Prim)));

               Meta_Tree.Drop_Completed_Primitive (
                  Obj.Meta_Tree_Obj, Prim);
               Progress := True;
            else
               raise Program_Error;
            end if;
         end;
      end loop Loop_Completed_Meta_Tree_Primitives;
   end Execute_Meta_Tree;

   procedure Execute_SCD (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Wait_For_Front_End.Prim;
      Curr_Lvl : constant Tree_Level_Index_Type := Obj.SCD_Curr_Lvl;
      Req  : constant Request.Object_Type := Obj.SCD_Req;
      Data : constant Block_Data_Type := Obj.SCD_Data;
   begin
      if Obj.SCD_State = Inactive then
         return;
      end if;

      --
      --  For now there is only one Request pending.
      --
      if not Request.Equal (Obj.Wait_For_Front_End.Req, Req) then
         Obj.SCD_State := Inactive;
         return;
      end if;

      if Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_FT then

         if not Write_Back.Primitive_Acceptable (Obj.Write_Back_Obj) then
            return;
         end if;

         Obj.Free_Tree_Retry_Count := 0;

         declare
            WB : constant New_Free_Tree.Write_Back_Data_Type :=
               New_Free_Tree.Peek_Completed_WB_Data (Obj.New_Free_Tree_Obj, Prim);

            FT_Root_Node : constant Type_1_Node_Type :=
               New_Free_Tree.Peek_Completed_Root_Node (Obj.New_Free_Tree_Obj,
                  Prim);
         begin

            Write_Back.Submit_Primitive (
               Obj.Write_Back_Obj, WB.Prim, WB.Gen, WB.VBA, WB.New_PBAs,
               WB.Old_PBAs, WB.Tree_Max_Level, Data, Obj.Write_Back_Data);

            Obj.Superblock.Free_Hash   := FT_Root_Node.Hash;
            Obj.Superblock.Free_Number := FT_Root_Node.PBA;
            Obj.Superblock.Free_Gen    := FT_Root_Node.Gen;
         end;

         New_Free_Tree.Drop_Completed_Primitive (Obj.New_Free_Tree_Obj, Prim);

         Obj.Wait_For_Front_End := Wait_For_Event_Invalid;
         Progress := True;
         Obj.SCD_State := Inactive;
         return;

      --
      --  The VBD module translated a write Request, writing the data
      --  now to disk involves multiple steps:
      --
      --  1. Gathering of all nodes in the branch and looking up the
      --     volatile ones (those, which belong to theCurr generation
      --     and will be updated in place).
      --  2. Allocate new blocks if needed by consulting the FT
      --  3. Updating all entries in the nodes
      --  4. Writing the branch back to the block device.
      --
      --  Those steps are handled by different modules, depending on
      --  the allocation of new blocks.
      --
      elsif
         Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_VBD
      then

         --
         --  As usual check first we can submit new requests.
         --
         if not New_Free_Tree.Request_Acceptable (Obj.New_Free_Tree_Obj)
            or else not Virtual_Block_Device.Trans_Can_Get_Type_1_Node_Walk (
               Obj.VBD, Prim)
         then
            return;
         end if;

         --
         --  Then (ab-)use the Translation module and its still pending
         --  Request to get all old PBAs, whose generation we then check.
         --  The order of the array items corresponds to the level within
         --  the tree.
         --
         Declare_Old_PBAs : declare

            Old_PBAs : Type_1_Node_Walk_Type := (
               others => Type_1_Node_Invalid);

            Trans_Max_Level : constant Tree_Level_Index_Type :=
               Virtual_Block_Device.Tree_Max_Level (Obj.VBD);

            Snap : constant Snapshot_Type :=
               Obj.Superblock.Snapshots (Curr_Snap (Obj));

            --
            --  Get the corresponding VBA that we use to calculate the index
            --  for the edge in the node for a given level within the tree.
            --
            VBA : constant Virtual_Block_Address_Type :=
               Virtual_Block_Address_Type (
                  Virtual_Block_Device.Trans_Get_Virtual_Block_Address (
                     Obj.VBD, Prim));
         begin

            Virtual_Block_Device.Trans_Get_Type_1_Node_Walk (
               Obj.VBD, Old_PBAs);

            --
            --  Make sure we work with the proper snapshot.
            --
            --  (This check may be removed at some point.)
            --
            if Old_PBAs (Trans_Max_Level).PBA /= Snap.PBA then
               raise Program_Error;
            end if;

            --
            --  Here only the inner nodes, i.E. all nodes excluding root and
            --  leaf, are considered. The root node is checked afterwards as
            --  we need the information of theCurr snapshot for that.
            --
            for Level in Curr_Lvl .. Trans_Max_Level loop

               --
               --  Use the old PBA to get the node's data from the cache and
               --  use it check how we have to handle the node.
               --
               Declare_Nodes :
               declare
                  PBA : constant Physical_Block_Address_Type :=
                     Old_PBAs (Tree_Level_Index_Type (Level)).PBA;

                  Nodes : Type_1_Node_Block_Type;

                  Cache_Prim : constant Primitive.Object_Type :=
                     Primitive.Valid_Object_No_Pool_Idx (
                        Read, False, Primitive.Tag_SCD_Cache,
                        Block_Number_Type (PBA), 0);

                  Job_Idx : Cache.Jobs_Index_Type;
               begin

                  if Obj.SCD_Cache_Prim_State = Invalid then

                     if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                        Obj.SCD_Cache_Prim := Cache_Prim;
                        Obj.SCD_Cache_Prim_State := Submitted;
                        Cache.Submit_Primitive (
                           Obj.Cache_Obj, Obj.SCD_Cache_Prim, Job_Idx);

                        if Primitive.Operation (Obj.SCD_Cache_Prim) = Write
                        then
                           Obj.Cache_Jobs_Data (Job_Idx) :=
                              Obj.SCD_Cache_Prim_Data;
                        end if;

                        Progress := True;
                     end if;
                     return;

                  elsif Obj.SCD_Cache_Prim_State /= Complete or else
                        not Primitive.Equal (Obj.SCD_Cache_Prim, Cache_Prim)
                  then
                     return;
                  end if;

                  if not Primitive.Success (Obj.SCD_Cache_Prim) then
                     raise Program_Error;
                  end if;

                  Type_1_Node_Block_From_Block_Data (
                     Nodes, Obj.SCD_Cache_Prim_Data);

                  Declare_Generation :
                  declare
                     Child_Idx : constant Tree_Child_Index_Type :=
                        Virtual_Block_Device.Index_For_Level (
                           Obj.VBD, VBA, Tree_Level_Index_Type (Level));

                     Gen : constant Generation_Type :=
                        Nodes (Natural (Child_Idx)).Gen;
                  begin
                     --
                     --  In case the generation of the entry is the same as the
                     --  Curr generation OR if the generation is 0 (which means
                     --  it was never used before) the block is volatile and we
                     --  change it in place and store it directly in the
                     --  new_PBA array.
                     --
                     pragma Debug (Debug.Print_String ("PBA: "
                        & Debug.To_String (Debug.Uint64_Type (PBA)) & " "
                        & "Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Gen)) & " "
                        & "Cur_Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
                        & " Npba: "
                        & Debug.To_String (Debug.Uint64_Type (
                           Nodes (Natural (Child_Idx)).PBA))));
                     if Gen = Obj.Cur_Gen or else Gen = 0 then

                        Obj.SCD_New_PBAs (Tree_Level_Index_Type (Level - 1)) :=
                           Old_PBAs (Tree_Level_Index_Type (Level - 1)).PBA;

                     --
                     --  Otherwise add the block to the free_PBA array so that
                     --  the FT will reserved it and note that we need another
                     --  new block.
                     --
                     else
                        Obj.SCD_Free_PBAs (Obj.SCD_Free_Blocks) :=
                           Old_PBAs (Tree_Level_Index_Type (Level - 1)).PBA;

                        Obj.SCD_Free_Blocks := Obj.SCD_Free_Blocks + 1;
                        Obj.SCD_New_Blocks  := Obj.SCD_New_Blocks  + 1;

                        pragma Debug (Debug.Print_String ("New_Blocks: "
                           & Debug.To_String (Debug.Uint64_Type (
                                Obj.SCD_New_Blocks))
                           & " Free_PBA: "
                           & Debug.To_String (Debug.Uint64_Type (
                              Obj.SCD_Free_PBAs (Obj.SCD_Free_Blocks))) & " "
                           & Debug.To_String (Debug.Uint64_Type (
                              Old_PBAs (
                                 Tree_Level_Index_Type (Level - 1)).PBA))));
                     end if;
                  end Declare_Generation;
               end Declare_Nodes;

               Obj.SCD_Cache_Prim_State := Invalid;
               Obj.SCD_Curr_Lvl := Obj.SCD_Curr_Lvl + 1;
            end loop;

            pragma Debug (Debug.Print_String ("Snap.Gen: "
               & Debug.To_String (Debug.Uint64_Type (Snap.Gen)) & " "
               & "Cur_Gen: "
               & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
               & " root PBA: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Trans_Max_Level - 1).PBA))
               & " Gen: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Trans_Max_Level - 1).Gen))));

            --  check root node
            if Old_PBAs (Trans_Max_Level).Gen = 0
               or else Old_PBAs (Trans_Max_Level).Gen = Obj.Cur_Gen
            then
               pragma Debug (Debug.Print_String ("Change root PBA in place"));
               Obj.SCD_New_PBAs (Trans_Max_Level) :=
                  Old_PBAs (Trans_Max_Level).PBA;
            else
               pragma Debug (Debug.Print_String ("New root PBA"));
               Obj.SCD_Free_PBAs (Obj.SCD_Free_Blocks) :=
                  Old_PBAs (Trans_Max_Level).PBA;
               Obj.SCD_New_Blocks := Obj.SCD_New_Blocks  + 1;
            end if;

            --
            --  Since there are blocks we cannot change in place, use the
            --  FT module to allocate the blocks. As we have to reserve
            --  the blocks we implicitly will free (free_PBA items), pass
            --  on theCurr generation.
            --
            if Obj.SCD_New_Blocks > 0 then
               New_Free_Tree.Submit_Request (
                  Obj              => Obj.New_Free_Tree_Obj,
                  Root_Node        => (
                     PBA  => Obj.Superblock.Free_Number,
                     Gen  => Obj.Superblock.Free_Gen,
                     Hash => Obj.Superblock.Free_Hash),
                  Tree_Geom        => (
                     Max_Level => Obj.Superblock.Free_Max_Level,
                     Edges     => Obj.Superblock.Free_Degree,
                     Leafs     => Obj.Superblock.Free_Leafs),
                  Current_Gen    => Obj.Cur_Gen,
                  Requested_Blocks => Obj.SCD_New_Blocks,
                  New_Blocks       => Obj.SCD_New_PBAs,
                  Old_Blocks       => Old_PBAs,
                  Max_Level        => Trans_Max_Level,
                  Req_Prim         => Prim,
                  VBA              => VBA);
            else
               --
               --  The complete branch is still part of theCurr generation,
               --  call the Write_Back module directly.
               --
               --  (We would have to check if the module can acutally accept
               --  the Request...)
               --
               Write_Back.Submit_Primitive (
                  Obj      => Obj.Write_Back_Obj,
                  Prim     => Prim,
                  Gen      => Obj.Cur_Gen,
                  VBA      => VBA,
                  New_PBAs => Obj.SCD_New_PBAs,
                  Old_PBAs => Old_PBAs,
                  N        => Trans_Max_Level,
                  Data     => Data,
                  WB_Data  => Obj.Write_Back_Data);
            end if;

            Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);

            Obj.Wait_For_Front_End := Wait_For_Event_Invalid;

            --
            --  Inhibit translation which effectively will suspend the
            --  Translation modules operation and will stall all other
            --  pending requests to make sure all following Request will
            --  use the newest tree.
            --
            --  (It stands to reasons whether we can remove this check
            --  if we make sure that only the requests belonging to
            --  the same branch are serialized.)
            --
            Virtual_Block_Device.Trans_Inhibit_Translation (Obj.VBD);
            Obj.Stall_Snapshot_Creation := True;
            Progress := True;
            Obj.SCD_State := Inactive;
            return;
         end Declare_Old_PBAs;
      end if;
      Obj.SCD_State := Inactive;
   end Execute_SCD;

   --
   --  Execute_Cache_Generated_Prims
   --
   procedure Execute_Cache_Generated_Prims (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Slot_Idx : Cache.Slots_Index_Type;
      Data_Idx : Block_IO.Data_Index_Type;
   begin

      Handle_Generated_Prims :
      loop

         Cache.Peek_Generated_Primitive (Obj.Cache_Obj, Prim, Slot_Idx);
         exit Handle_Generated_Prims when
            not Primitive.Valid (Prim);

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Cache_Blk_IO =>

            exit Handle_Generated_Prims when
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Cache_Blk_IO, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) := Obj.Cache_Slots_Data (Slot_Idx);
            end if;

            Cache.Drop_Generated_Primitive (Obj.Cache_Obj, Slot_Idx);
            Progress := True;

         when others => raise Program_Error;
         end case;

      end loop Handle_Generated_Prims;

   end Execute_Cache_Generated_Prims;

   --
   --  Execute_Cache_Completed_Prims
   --
   procedure Execute_Cache_Completed_Prims (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin

      Handle_Completed_Prims :
      loop

         Cache.Peek_Completed_Primitive (Obj.Cache_Obj, Prim, Job_Idx);
         exit Handle_Completed_Prims when
            not Primitive.Valid (Prim);

         if not Primitive.Success (Prim) then
            raise Program_Error;
         end if;

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Lib_Cache_Sync =>

            case Obj.Cache_Sync_State is
            when Active =>

               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Obj.Cache_Sync_State := Inactive;
               Progress := True;

            when Inactive => raise Program_Error;
            end case;

         when Primitive.Tag_WB_Cache =>

            if Obj.WB_Cache_Prim_1_State = Submitted and then
               Primitive.Equal (Obj.WB_Cache_Prim_1, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_1_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_1_State := Complete;
               Obj.WB_Cache_Prim_1 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

            elsif Obj.WB_Cache_Prim_2_State = Submitted and then
                  Primitive.Equal (Obj.WB_Cache_Prim_2, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_2_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_2_State := Complete;
               Obj.WB_Cache_Prim_2 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

            elsif Obj.WB_Cache_Prim_3_State = Submitted and then
                  Primitive.Equal (Obj.WB_Cache_Prim_3, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_3_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_3_State := Complete;
               Obj.WB_Cache_Prim_3 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
            else
               raise Program_Error;
            end if;

         when Primitive.Tag_SCD_Cache =>

            if Obj.SCD_Cache_Prim_State = Submitted and then
               Primitive.Equal (Obj.SCD_Cache_Prim, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.SCD_Cache_Prim_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.SCD_Cache_Prim_State := Complete;
               Obj.SCD_Cache_Prim := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
            else
               raise Program_Error;
            end if;

         when Primitive.Tag_VBD_Cache =>

            Virtual_Block_Device.Mark_Generated_Cache_Primitive_Complete (
                  Obj.VBD, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

         when Primitive.Tag_FT_Cache =>

            New_Free_Tree.Mark_Generated_Cache_Primitive_Complete (
               Obj.New_Free_Tree_Obj, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

         when Primitive.Tag_MT_Cache =>

            Meta_Tree.Mark_Generated_Cache_Primitive_Complete (
               Obj.Meta_Tree_Obj, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

         when others => raise Program_Error;
         end case;

      end loop Handle_Completed_Prims;

   end Execute_Cache_Completed_Prims;

   --
   --  Execute_Cache
   --
   procedure Execute_Cache (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean)
   is
   begin
      Cache.Execute (
         Obj.Cache_Obj, Obj.Cache_Slots_Data, Obj.Cache_Jobs_Data, Progress);

      Execute_Cache_Generated_Prims (Obj, IO_Buf, Progress);
      Execute_Cache_Completed_Prims (Obj, Progress);

   end Execute_Cache;

   --
   --  Execute
   --
   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Now               :        Timestamp_Type)
   is
      pragma Unreferenced (Now);
      Progress : Boolean := False;
   begin

      Execute_SCD (Obj, Progress);

      --  pragma Debug (Debug.Print_String (To_String (Obj)));

      -------------------------
      --  Snapshot handling  --
      -------------------------

      if Obj.Creating_Snapshot and then
         not Obj.Secure_Superblock and then
         not Obj.Stall_Snapshot_Creation
      then
         Create_Snapshot_Internal (Obj, Progress);
      end if;

      --------------------
      --  Sync handling --
      --------------------

      if Primitive.Valid (Obj.Sync_Primitive) and then
         not Obj.Secure_Superblock
      then
         if Obj.Cache_Sync_State = Inactive then
            Declare_Sync_Cache_Dirty :
            declare
               Cache_Dirty : Boolean;
            begin

               Try_Flush_Cache_If_Dirty (Obj, Cache_Dirty);

               if not Cache_Dirty then
                  Obj.Secure_Superblock := True;
               end if;
               Progress := True;
            end Declare_Sync_Cache_Dirty;
         end if;
      end if;

      Execute_Cache (Obj, IO_Buf, Progress);

      declare
         Local_Progress : Boolean := False;
      begin
         Execute_Free_Tree (Obj, Local_Progress);
         Progress := Progress or else Local_Progress;
      end;

      declare
         Local_Progress : Boolean := False;
      begin
         Execute_Meta_Tree (Obj, Local_Progress);
         Progress := Progress or else Local_Progress;
      end;

      --
      --  A complete primitive was either successful or has failed.
      --
      --  In the former case we will instruct the Write_Back module to
      --  write all changed nodes of the VBD back to the block device
      --  and eventually will leadt to ACKing the block Request.
      --
      --  In the later case we will attempt to free reserved blocks in
      --  the FT by discarding snapshots. Briefly speaking all snapshots
      --  that were not specifically marked (see FLAG_KEEP) will be
      --  discarded. A finit number of retries will be performed. If we
      --  cannot free enough blocks, the write operation is marked as
      --  failed and will result in an I/O error at the Block session.
      --
      --
      Loop_Free_Tree_Completed_Prims :
      loop
         Declare_Prim_1 :
         declare
            Prim : constant Primitive.Object_Type :=
               New_Free_Tree.Peek_Completed_Primitive (Obj.New_Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Success (Prim);

            if Obj.Free_Tree_Retry_Count < Free_Tree_Retry_Limit then
               Declare_Could_Discard_Snap :
               declare
                  Could_Discard_Snap : Boolean;
               begin
                  Try_Discard_Snapshot (
                     Obj.Superblock.Snapshots,
                     Curr_Snap (Obj), Could_Discard_Snap);
                  if Could_Discard_Snap then
                     Obj.Free_Tree_Retry_Count :=
                        Obj.Free_Tree_Retry_Count + 1;

                     --
                     --  Instructing the FT to retry the allocation will
                     --  lead to clearing its internal 'query branches'
                     --  state and executing the previously submitted
                     --  Request again.
                     --
                     --  (This retry attempt is a shortcut as we do not have
                     --  all information available at this point to call
                     --  'submit_Request' again - so we must not call
                     --  'drop_Completed_Primitive' as this will clear the
                     --  Request.)
                     --
                     New_Free_Tree.Retry_Allocation (Obj.New_Free_Tree_Obj);
                  else
                     Debug.Print_String ("Retry_Allocation failed");
                     raise Program_Error;
                  end if;
               end Declare_Could_Discard_Snap;
               exit Loop_Free_Tree_Completed_Prims;
            end if;

            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);

            --  FIXME
            Virtual_Block_Device.Trans_Resume_Translation (Obj.VBD);
            Obj.Stall_Snapshot_Creation := False;
            New_Free_Tree.Drop_Completed_Primitive (Obj.New_Free_Tree_Obj,
            Prim);

         end Declare_Prim_1;
         Progress := True;

      end loop Loop_Free_Tree_Completed_Prims;

      ---------------------------------
      --  Put Request into splitter  --
      ---------------------------------

      --
      --  An arbitrary sized Block Request will be cut into Block_Size-sized
      --  primitves by the Splitter module.
      --
      Loop_Pool_Pending_Requests :
      loop
         Declare_Pool_Idx_Slot :
         declare
            Pool_Idx_Slot : constant Pool_Index_Slot_Type :=
               Pool.Peek_Pending_Request (Obj.Request_Pool_Obj);
         begin
            exit Loop_Pool_Pending_Requests when
               not Pool_Idx_Slot_Valid (Pool_Idx_Slot) or else
               not Splitter.Request_Acceptable (Obj.Splitter_Obj);

            Declare_Pool_Idx :
            declare
               Pool_Idx : constant Pool_Index_Type :=
                  Pool_Idx_Slot_Content (Pool_Idx_Slot);

               Req : constant Request.Object_Type :=
                  Pool.Request_For_Index (Obj.Request_Pool_Obj, Pool_Idx);

               Snap_ID : constant Snapshot_ID_Type :=
                  Pool.Snap_ID_For_Request (Obj.Request_Pool_Obj, Req);
            begin

               if Pool.Overlapping_Request_In_Progress (
                  Obj.Request_Pool_Obj,
                  Request.Block_Number (Req))
               then
                  pragma Debug (Pool.Dump_Pool_State (Obj.Request_Pool_Obj));
                  pragma Debug (Debug.Print_String ("Execute: "
                     & "overlapping request in progress"));
                  exit Loop_Pool_Pending_Requests;
               end if;

               Pool.Drop_Pending_Request (Obj.Request_Pool_Obj);
               Splitter.Submit_Request (
                  Obj.Splitter_Obj, Pool_Idx, Req, Snap_ID);

            end Declare_Pool_Idx;
         end Declare_Pool_Idx_Slot;
         Progress := True;
      end loop Loop_Pool_Pending_Requests;

      --
      --  Give primitive to the translation module
      --
      Loop_Splitter_Generated_Prims :
      loop
         Declare_Prim_3 :
         declare
            Snap_Slot_Index : Snapshots_Index_Type;
            Prim : constant Primitive.Object_Type :=
               Splitter.Peek_Generated_Primitive (Obj.Splitter_Obj);
            Snap_ID : constant Snapshot_ID_Type :=
               Splitter.Peek_Generated_Primitive_ID (Obj.Splitter_Obj);
         begin
            exit Loop_Splitter_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Virtual_Block_Device.Primitive_Acceptable (Obj.VBD);

            if Obj.Secure_Superblock or else Obj.Creating_Snapshot then
               pragma Debug (Debug.Print_String ("Execute: "
                  & "creating snapshot or securing superblock in progress, "
                  & "inhibit Splitter"));
               exit Loop_Splitter_Generated_Prims;
            end if;

            --
            --  exit loop when sync is already in progress
            --  XXX this check and the one above should be moved outside
            --      the loop
            --
            if Primitive.Valid (Obj.Sync_Primitive) then
               pragma Debug (Debug.Print_String ("Execute: "
                  & "Sync operation pending"));
               exit Loop_Splitter_Generated_Prims;
            end if;

            Splitter.Drop_Generated_Primitive (Obj.Splitter_Obj);

            if Primitive.Operation (Prim) = Sync then
               pragma Debug (Debug.Print_String ("Execute: "
                  & "Sync operation requested"));
               Obj.Sync_Primitive := Prim;
               Progress := True;
               exit Loop_Splitter_Generated_Prims;
            end if;

            if Snap_ID /= 0 then
               Snap_Slot_Index := Snap_Slot_For_ID (Obj,
                  Generation_Type (Snap_ID));
            else
               Snap_Slot_Index := Curr_Snap (Obj);
            end if;

            pragma Debug (Debug.Print_String ("Execute: "
               & "submit VBD: "
               & "Snap_ID: " & Debug.To_String (Debug.Uint64_Type (Snap_ID))
               & " Prim: " & Primitive.To_String (Prim)
               & " Cur_SB: " & Debug.To_String (Debug.Uint64_Type (Obj.Cur_SB))
               & " Curr_Snap: " & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblock.Curr_Snap))
               & " PBA: " & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblock.Snapshots (Snap_Slot_Index).PBA))
               & " Gen: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblock.Snapshots (Snap_Slot_Index).Gen))
               & " "
               & Debug.To_String (Obj.Superblock.Snapshots (
                  Snap_Slot_Index).Hash)));

            --
            --  For every new Request, we have to use the currlently active
            --  snapshot as a previous Request may have changed the tree.
            --
            Virtual_Block_Device.Submit_Primitive (
               Obj.VBD,
               Obj.Superblock.Snapshots (Snap_Slot_Index).PBA,
               Obj.Superblock.Snapshots (Snap_Slot_Index).Gen,
               Obj.Superblock.Snapshots (Snap_Slot_Index).Hash,
               Prim);

         end Declare_Prim_3;
         Progress := True;
      end loop Loop_Splitter_Generated_Prims;

      Execute_VBD (Obj, Progress);

      ---------------------------
      --  Write-back handling  --
      ---------------------------

      --
      --  The Write_Back module will store a changed branch including its leaf
      --  node on the block device.
      --
      --  The way it currently operates is as follows:
      --    1. (CRYPTO)   it hands the leaf Data to the Crypto module for
      --                  encryption
      --    2. (IO)       it hands the encrypted leaf Data to I/O module to
      --                  write it to the block device
      --    3. (CACHE)    starting by the lowest inner node it will update the
      --                  node entry (PBA and Hash)
      --    4. (COMPLETE) update root PBA and root Hash
      --

      Loop_WB_Completed_Prims :
      loop
         Declare_Prim_6 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Completed_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Update_Snapshot_Hash (
               Obj.Write_Back_Obj,
               Obj.Cur_Gen,
               Obj.Superblock.Snapshots (Curr_Snap (Obj)),
               Prim);

            --
            --  We touched the super-block, either by updating a snapshot or by
            --  creating a new one - make sure it gets secured within the next
            --  interval.
            --
            Write_Back.Drop_Completed_Primitive (Obj.Write_Back_Obj, Prim);

            --
            --  Since the write Request is finally finished, all nodes stored
            --  at some place "save" (leafs on the block device, inner nodes
            --  within the Cache, acknowledge the primitive.
            --
            Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);

         end Declare_Prim_6;
         Progress := True;

         --
         --  FIXME stalling translation as long as the write-back takes places
         --        is not a good idea
         --
         Virtual_Block_Device.Trans_Resume_Translation (Obj.VBD);
         Obj.Stall_Snapshot_Creation := False;

      end loop Loop_WB_Completed_Prims;

      --
      --  Give the leaf Data to the Crypto module.
      --
      Loop_WB_Generated_Crypto_Prims :
      loop

         Declare_Prim_7 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_Crypto_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            --
            --  The Data will be copied into the Crypto module's internal
            --  buffer
            --
            Declare_Crypto_Data :
            declare
               Plain_Data_Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (
                     Obj.Write_Back_Obj, Prim);

               Data_Idx : Crypto.Item_Index_Type;
            begin
               Crypto.Submit_Primitive (Obj.Crypto_Obj, Prim, Data_Idx);
               Crypto_Plain_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Plain_Data_Index);

            end Declare_Crypto_Data;
            Write_Back.Drop_Generated_Crypto_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_7;
         Progress := True;

      end loop Loop_WB_Generated_Crypto_Prims;

      --
      --  Pass the encrypted leaf Data to the I/O module.
      --
      Loop_WB_Generated_IO_Prims :
      loop
         Declare_Prim_8 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_IO_Primitive (Obj.Write_Back_Obj);

            Data_Idx : Block_IO.Data_Index_Type;
         begin
            exit Loop_WB_Generated_IO_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Write_Back, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Write_Back.Peek_Generated_IO_Data (
                     Obj.Write_Back_Obj, Prim));
            end if;

            Write_Back.Drop_Generated_IO_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_8;
         Progress := True;

      end loop Loop_WB_Generated_IO_Prims;

      --
      --  Update the inner nodes of the tree. This is always done after the
      --  encrypted leaf node was stored by the I/O module.
      --
      Loop_WB_Generated_Cache_Prims :
      loop
         if not Primitive.Valid (Obj.WB_Prim) then
            Obj.WB_Prim :=
               Write_Back.Peek_Generated_Cache_Primitive (
                  Obj.Write_Back_Obj);
         end if;

         exit Loop_WB_Generated_Cache_Prims when
            not Primitive.Valid (Obj.WB_Prim);

         if Obj.WB_Cache_Prim_1_State = Invalid then
            Obj.WB_Update_PBA :=
               Write_Back.Peek_Generated_Cache_Update_PBA (
                  Obj.Write_Back_Obj, Obj.WB_Prim);
         end if;

         Declare_PBAs :
         declare
            PBA : constant Physical_Block_Address_Type :=
               Physical_Block_Address_Type (
                  Primitive.Block_Number (Obj.WB_Prim));

            Cache_Prim_1 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (PBA), 0);

            Cache_Prim_2 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (Obj.WB_Update_PBA), 0);

            Cache_Prim_3 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (Obj.WB_Update_PBA), 0);

            Job_Idx : Cache.Jobs_Index_Type;
         begin

            if Obj.WB_Cache_Prim_1_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Obj.WB_Cache_Prim_1 := Cache_Prim_1;
                  Obj.WB_Cache_Prim_1_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_1, Job_Idx);

                  if Primitive.Operation (Cache_Prim_1) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_1_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_1_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_1, Cache_Prim_1)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            if not Primitive.Success (Obj.WB_Cache_Prim_1) then
               raise Program_Error;
            end if;

            if Obj.WB_Cache_Prim_2_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Obj.WB_Cache_Prim_2 := Cache_Prim_2;
                  Obj.WB_Cache_Prim_2_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_2, Job_Idx);

                  if Primitive.Operation (Cache_Prim_2) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_1_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_2_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_2, Cache_Prim_2)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            if not Primitive.Success (Obj.WB_Cache_Prim_2) then
               raise Program_Error;
            end if;

            if Obj.WB_Cache_Prim_3_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Write_Back.Drop_Generated_Cache_Primitive (
                     Obj.Write_Back_Obj, Obj.WB_Prim);

                  Write_Back.Update (
                     Obj.Write_Back_Obj,
                     PBA, Virtual_Block_Device.Get_Tree_Helper (Obj.VBD),
                     Obj.WB_Cache_Prim_1_Data, Obj.WB_Cache_Prim_2_Data);

                  Obj.WB_Cache_Prim_3 := Cache_Prim_3;
                  Obj.WB_Cache_Prim_3_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_3, Job_Idx);

                  if Primitive.Operation (Cache_Prim_3) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_2_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_3_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_3, Cache_Prim_3)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            Obj.WB_Cache_Prim_1_State := Invalid;
            Obj.WB_Cache_Prim_2_State := Invalid;
            Obj.WB_Cache_Prim_3_State := Invalid;
            Obj.WB_Prim := Primitive.Invalid_Object;

         end Declare_PBAs;

      end loop Loop_WB_Generated_Cache_Prims;

      ----------------------------
      --  Super-block handling  --
      ----------------------------

      --
      --  Store the current generation in the current
      --  super-block before it gets secured.
      --
      if
         Obj.Secure_Superblock and then
         Sync_Superblock.Request_Acceptable (Obj.Sync_SB_Obj)
      then

         --
         --  Only when a snapshot was created secure the generation.
         --
         --  In case the superblock was merely synced because either
         --  a snapshot was dropped or a sync request was processed
         --  do not secure the generation.
         --
         if Obj.Creating_Snapshot then
            Obj.Superblock.Last_Secured_Generation := Obj.Cur_Gen;

            pragma Debug (Debug.Print_String ("Sync_Superblock "
               & " new Obj.Last_Secured_Generation: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblock.Last_Secured_Generation))));

            Sync_Superblock.Submit_Request (
               Obj.Sync_SB_Obj, Obj.Cur_SB, Obj.Cur_Gen);
         else
            pragma Debug (Debug.Print_String ("Sync_Superblock "
               & " Obj.Last_Secured_Generation: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Superblock.Last_Secured_Generation))));

            Sync_Superblock.Submit_Request (
               Obj.Sync_SB_Obj, Obj.Cur_SB, Obj.Last_Secured_Generation);
         end if;

      end if;

      --
      --  When the current super-block was secured, select the next one.
      --
      Loop_Sync_SB_Completed_Prims :
      loop
         Declare_Prim_10 :
         declare
            Prim : constant Primitive.Object_Type :=
               Sync_Superblock.Peek_Completed_Primitive (Obj.Sync_SB_Obj);
         begin
            exit Loop_Sync_SB_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            --  handle state
            Obj.Cur_SB := Advance_Superblocks_Index (Obj.Cur_SB);
            Obj.Last_Secured_Generation :=
               Sync_Superblock.Peek_Completed_Generation (
                  Obj.Sync_SB_Obj, Prim);

            Obj.Superblock.Superblock_ID := Obj.Superblock.Superblock_ID + 1;

            pragma Debug (Debug.Print_String (
               "Loop_Sync_SB_Completed_Prims "
               & " Obj.Last_Secured_Generation: "
               & Debug.To_String (Debug.Uint64_Type (
                  Obj.Last_Secured_Generation))));

            if Obj.Creating_Snapshot then
               --  next snapshot slot will contain new generation
               Obj.Last_Root_PBA := Obj.Superblock.Snapshots (
                  Curr_Snap (Obj)).PBA;
               Obj.Last_Root_Hash := Obj.Superblock.Snapshots (
                  Curr_Snap (Obj)).Hash;

               --
               --  Look for a new snapshot slot. If we cannot find one
               --  we manual intervention b/c there are too many snapshots
               --  flagged as keep
               --
               Declare_Next_Snap_2 :
               declare
                  Next_Snap : constant Snapshots_Index_Type :=
                     Next_Snap_Slot (Obj);
               begin

                  --
                  --  Could not find free slots, we need to discard some
                  --  quarantine snapshots, user intervention needed.
                  --
                  if Next_Snap = Curr_Snap (Obj) then
                     raise Program_Error;
                  end if;

                  Declare_Tree :
                  declare
                     Tree : constant Tree_Helper.Object_Type :=
                        Virtual_Block_Device.Get_Tree_Helper (Obj.VBD);
                  begin
                     Obj.Superblock.Snapshots (Next_Snap).Max_Level :=
                        Tree_Helper.Max_Level (Tree);
                     Obj.Superblock.Snapshots (Next_Snap).Nr_Of_Leafs :=
                        Tree_Helper.Leafs (Tree);
                  end Declare_Tree;

                  Obj.Superblock.Snapshots (Next_Snap) :=
                     Obj.Superblock.Snapshots (Curr_Snap (Obj));
                  Obj.Superblock.Snapshots (Next_Snap).Keep := False;

                  Obj.Superblock.Snapshots (Next_Snap).Valid := True;
                  Obj.Superblock.Snapshots (Next_Snap).Gen := Obj.Cur_Gen;
                  Obj.Superblock.Curr_Snap := Next_Snap;
               end Declare_Next_Snap_2;

               Obj.Cur_Gen := Obj.Cur_Gen  + 1;
               Obj.Creating_Snapshot := False;
            end if;

            if Primitive.Valid (Obj.Sync_Primitive) then

               Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj,
                  Obj.Sync_Primitive);
               Obj.Sync_Primitive := Primitive.Invalid_Object;
            end if;

            Obj.Secure_Superblock := False;

            pragma Debug (Debug.Dump_Superblock (Obj.Cur_SB,
               Obj.Superblock));

            Sync_Superblock.Drop_Completed_Primitive (Obj.Sync_SB_Obj, Prim);

         end Declare_Prim_10;
         Progress := True;

      end loop Loop_Sync_SB_Completed_Prims;

      --
      --  Use I/O module to write super-block to the block device.
      --
      Loop_Sync_SB_Generated_Prims :
      loop
         Declare_Prim_11 :
         declare
            Prim : constant Primitive.Object_Type :=
               Sync_Superblock.Peek_Generated_Primitive (Obj.Sync_SB_Obj);
         begin
            exit Loop_Sync_SB_Generated_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Declare_SB_Data :
            declare
               SB_Data : Block_Data_Type;
               Data_Idx : Block_IO.Data_Index_Type;
            begin
               Block_Data_From_Superblock (SB_Data, Obj.Superblock);

               Block_IO.Submit_Primitive (
                  Obj.IO_Obj, Primitive.Tag_Sync_SB, Prim, Data_Idx);

               if Primitive.Operation (Prim) = Write then
                  IO_Buf (Data_Idx) := SB_Data;
               end if;
            end Declare_SB_Data;
            Sync_Superblock.Drop_Generated_Primitive (Obj.Sync_SB_Obj, Prim);

         end Declare_Prim_11;
         Progress := True;

      end loop Loop_Sync_SB_Generated_Prims;

      -----------------------
      --  Crypto handling  --
      -----------------------

      --
      --  The Crypto module has its own internal buffer, Data has to be
      --  copied in and copied out.
      --

      --
      --  Only writes primitives (encrypted Data) are handled here,
      --  read primitives (decrypred Data) are handled in 'give_Read_Data'.
      --
      Loop_Crypto_Completed_Prims :
      loop
         Declare_Prim_12 :
         declare
            Prim : constant Primitive.Object_Type :=
               Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
         begin
            exit Loop_Crypto_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Operation (Prim) = Read;

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Declare_Index_2 :
            declare
               Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (
                     Obj.Write_Back_Obj, Prim);
            begin
               --
               --  FIXME instead of copying the Data just ask the crypto
               --        module for the resulting Hash and omit further
               --        processing in case the operation failed
               --
               Obj.Write_Back_Data (Index) := Crypto_Cipher_Buf (
                  Crypto.Data_Index (Obj.Crypto_Obj, Prim));

               Write_Back.Mark_Completed_Crypto_Primitive (
                  Obj.Write_Back_Obj, Prim, Obj.Write_Back_Data (Index));

            end Declare_Index_2;
            Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);

         end Declare_Prim_12;
         Progress := True;

      end loop Loop_Crypto_Completed_Prims;

      --------------------
      --  I/O handling  --
      --------------------

      --
      --  This module handles all the block backend I/O and has to
      --  work with all most all modules. IT uses the 'Tag' field
      --  to differentiate the modules.
      --

      Loop_IO_Completed_Prims :
      loop
         Declare_Prim_15 :
         declare
            Prim : constant Primitive.Object_Type :=
               Block_IO.Peek_Completed_Primitive (Obj.IO_Obj);
         begin
            exit Loop_IO_Completed_Prims when not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            Declare_Index_3 :
            declare
               Index : constant Block_IO.Data_Index_Type :=
                  Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj);

               --
               --  Whenever we cannot hand a successful primitive over
               --  to the corresponding module, leave the loop but keep
               --  the completed primitive so that it might be processed
               --  next time.
               --
               Mod_Progress : Boolean := True;
            begin
               if Primitive.Has_Tag_Decrypt (Prim) then

                  if not Crypto.Primitive_Acceptable (Obj.Crypto_Obj) then
                     Mod_Progress := False;
                  else
                     Declare_Data :
                     declare
                        Data_Idx : Crypto.Item_Index_Type;
                        SHA_Data : SHA256_4K.Data_Type;
                        SHA_Hash : SHA256_4K.Hash_Type;
                        CBE_Hash : Hash_Type;
                     begin
                        SHA256_4K_Data_From_CBE_Data (
                           SHA_Data, IO_Buf (Index));
                        SHA256_4K.Hash (SHA_Data, SHA_Hash);
                        CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
                        if CBE_Hash /=
                           Block_IO.Peek_Completed_Hash (Obj.IO_Obj, Prim)
                        then
                           raise Program_Error;
                        end if;

                        --
                        --  Having to override the Tag is needed because of
                        --  the way the Crypto module is hooked up in the
                        --  overall Data flow. Since it is the one that
                        --  acknowledges the primitive to the pool in the read
                        --  case, we have to use the Tag the pool module uses.
                        --
                        Crypto.Submit_Primitive (
                           Obj.Crypto_Obj,
                           Primitive.Copy_Valid_Object_New_Tag (
                              Prim,
                              Block_IO.Peek_Completed_Tag (
                                 Obj.IO_Obj, Prim)),
                           Data_Idx);

                        Crypto_Cipher_Buf (Data_Idx) := IO_Buf (Index);

                     end Declare_Data;
                  end if;

               elsif Primitive.Has_Tag_Cache_Blk_IO (Prim) then

                  Declare_Slot_Idx :
                  declare
                     Slot_Idx : constant Cache.Slots_Index_Type :=
                        Cache.Slots_Index_Type (Primitive.Index (Prim));
                  begin
                     if Primitive.Operation (Prim) = Read then
                        Obj.Cache_Slots_Data (Slot_Idx) := IO_Buf (Index);
                     end if;
                     Cache.Mark_Generated_Primitive_Complete (
                        Obj.Cache_Obj, Slot_Idx, Primitive.Success (Prim));

                  end Declare_Slot_Idx;

               elsif Primitive.Has_Tag_Write_Back (Prim) then
                  Write_Back.Mark_Completed_IO_Primitive (
                     Obj.Write_Back_Obj, Prim);

               elsif Primitive.Has_Tag_Sync_SB (Prim) then
                  Sync_Superblock.Mark_Generated_Primitive_Complete (
                     Obj.Sync_SB_Obj, Prim);
               else
                  raise Program_Error;
               end if;
               exit Loop_IO_Completed_Prims when not Mod_Progress;

            end Declare_Index_3;
            Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);

         end Declare_Prim_15;
         Progress := True;

      end loop Loop_IO_Completed_Prims;

      --
      --  Submit Block-IO read-primitives for completed primitives of the VBD
      --
      Loop_VBD_Completed_Prims :
      loop
         declare
            Prim : Primitive.Object_Type :=
               Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
         begin
            exit Loop_VBD_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Operation (Prim) /= Read;

            if Virtual_Block_Device.Peek_Completed_Generation (Obj.VBD) =
               Initial_Generation
            then
               --  write all 0 to cache entry
               --  write all 0 to result buffer
               --  mark primitive complete at request pool

               exit Loop_VBD_Completed_Prims when
                  not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

               Declare_Data_Idx :
               declare
                  Data_Idx : Crypto.Item_Index_Type;
               begin
                  Primitive.Success (Prim, True);
                  Crypto.Submit_Completed_Primitive (
                     Obj.Crypto_Obj, Prim, Data_Idx);
                  Crypto_Plain_Buf (Data_Idx) := (others => 0);
               end Declare_Data_Idx;

               Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);
               Progress := True;

            else

               exit Loop_VBD_Completed_Prims when
                  not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

               Block_IO.Submit_Primitive_Decrypt (
                  Obj.IO_Obj, Prim,
                  Virtual_Block_Device.Peek_Completed_Hash (Obj.VBD));

               Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);
               Progress := True;

            end if;
         end;
      end loop Loop_VBD_Completed_Prims;
      Obj.Execute_Progress := Progress;

   end Execute;

   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (Pool.Request_Acceptable (Obj.Request_Pool_Obj));

   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      ID  :        Snapshot_ID_Type)
   is
      Number_Of_Primitives : constant Number_Of_Primitives_Type :=
         Splitter.Number_Of_Primitives (Req);
   begin
      if Number_Of_Primitives = 0 then
         raise Program_Error;
      end if;

      Pool.Submit_Request (
         Obj.Request_Pool_Obj,
         Req,
         ID,
         Number_Of_Primitives);
   end Submit_Client_Request;

   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type
   is (Pool.Peek_Completed_Request (Obj.Request_Pool_Obj));

   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Pool.Drop_Completed_Request (Obj.Request_Pool_Obj, Req);
   end Drop_Completed_Client_Request;

   --
   --  For now there can be only one Request pending.
   --
   function Front_End_Busy_With_Other_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Boolean
   is (not Request.Equal (Obj.Wait_For_Front_End.Req, Req));

   procedure Has_IO_Request (
      Obj      :     Object_Type;
      Req      : out Request.Object_Type;
      Data_Idx : out Block_IO.Data_Index_Type)
   is
   begin
      Req      := Request.Invalid_Object;
      Data_Idx := 0;
      declare
         Prim : constant Primitive.Object_Type :=
            Block_IO.Peek_Generated_Primitive (Obj.IO_Obj);
      begin
         if Primitive.Valid (Prim) then
            Data_Idx := Block_IO.Peek_Generated_Data_Index (Obj.IO_Obj, Prim);
            Req      := Request.Valid_Object (
               Op     => Primitive.Operation (Prim),
               Succ   => False,
               Blk_Nr => Primitive.Block_Number (Prim),
               Off    => 0,
               Cnt    => 1,
               Tg     => 0);
         end if;
      end;
   end Has_IO_Request;

   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type)
   is
   begin
      Block_IO.Drop_Generated_Primitive_2 (Obj.IO_Obj, Data_Idx);
   end IO_Request_In_Progress;

   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean)
   is
   begin
      Block_IO.Mark_Generated_Primitive_Complete (
         Obj.IO_Obj, Data_Index, Success);
   end IO_Request_Completed;

   procedure Client_Data_Ready (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type)
   is
   begin
      Req := Request.Invalid_Object;

      if Primitive.Valid (Obj.Wait_For_Front_End.Prim) then
         return;
      end if;

      --
      --  When it was a read Request, we need the location to
      --  where the Crypto should copy the decrypted data.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
      begin
         if
            Primitive.Valid (Prim) and then
            Primitive.Operation (Prim) = Read
         then
            Start_Waiting_For_Front_End (Obj, Prim, Event_Obtain_Client_Data);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;
   end Client_Data_Ready;

   function Client_Data_Index (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Primitive.Index_Type
   is
   begin
      if Front_End_Busy_With_Other_Request (Obj, Req) then
         return Primitive.Invalid_Index;
      end if;

      return Primitive.Index (Obj.Wait_For_Front_End.Prim);
   end Client_Data_Index;

   procedure Obtain_Client_Data (
      Obj              : in out Object_Type;
      Req              :        Request.Object_Type;
      Data_Index       :    out Crypto.Plain_Buffer_Index_Type;
      Data_Index_Valid :    out Boolean)
   is
      Prim  : constant Primitive.Object_Type := Obj.Wait_For_Front_End.Prim;
      Event : constant Event_Type            := Obj.Wait_For_Front_End.Event;
   begin
      Data_Index_Valid := False;
      Data_Index       := Crypto.Plain_Buffer_Index_Type'First;

      if Front_End_Busy_With_Other_Request (Obj, Req) or else
         Event /= Event_Obtain_Client_Data
      then
         return;
      end if;

      Data_Index := Crypto.Plain_Buffer_Index_Type (
         Crypto.Data_Index (Obj.Crypto_Obj, Prim));

      Data_Index_Valid := True;
      Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
      Pool.Mark_Completed_Primitive (Obj.Request_Pool_Obj, Prim);
      Obj.Wait_For_Front_End := Wait_For_Event_Invalid;
   end Obtain_Client_Data;

   procedure Start_Waiting_For_Front_End (
      Obj   : in out Object_Type;
      Prim  :        Primitive.Object_Type;
      Event :        Event_Type)
   is
   begin
      Obj.Wait_For_Front_End := (
         Req         =>
            Pool.Request_For_Index (
               Obj.Request_Pool_Obj,
               Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim))),
         Prim        => Prim,
         Event       => Event,
         In_Progress => False);
   end Start_Waiting_For_Front_End;

   --
   --  FIXME move Wait_For_Front_End allocation into execute,
   --       turn procedure into function
   --
   procedure Client_Data_Required (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type)
   is
   begin
      Req := Request.Invalid_Object;

      if Primitive.Valid (Obj.Wait_For_Front_End.Prim) then
         return;
      end if;

      --
      --  A write Request, we need the location from where to read the new
      --  leaf data.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
      begin
         if Primitive.Valid (Prim) and then Primitive.Operation (Prim) = Write
         then
            Start_Waiting_For_Front_End (
               Obj, Prim, Event_Supply_Client_Data_After_VBD);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;

      --
      --  The free-tree needs the data to give to the Write_Back module.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            New_Free_Tree.Peek_Completed_Primitive (Obj.New_Free_Tree_Obj);
      begin
         if Primitive.Valid (Prim) and then Primitive.Success (Prim) then
            Start_Waiting_For_Front_End (
               Obj, Prim, Event_Supply_Client_Data_After_FT);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;
   end Client_Data_Required;

   procedure Supply_Client_Data (
      Obj      : in out Object_Type;
      Now      :        Timestamp_Type;
      Req      :        Request.Object_Type;
      Data     :        Block_Data_Type;
      Progress :    out Boolean)
   is
      pragma Unreferenced (Now);
   begin
      case Obj.SCD_State is
      when Inactive =>
         Obj.SCD_State := Active;
         Obj.SCD_Data := Data;
         Obj.SCD_Req := Req;
         Obj.SCD_Curr_Lvl := 1;
         Obj.SCD_New_PBAs := (others => 0);
         Obj.SCD_New_Blocks := 0;
         Obj.SCD_Free_PBAs := (others => 0);
         Obj.SCD_Free_Blocks := 0;
         Progress := True;
      when others =>
         raise Program_Error;
      end case;
   end Supply_Client_Data;

   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type)
   is
      Item_Index : Crypto.Item_Index_Type;
      Prim       : Primitive.Object_Type;
   begin
      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Item_Index, Prim);
      Data_Index := Crypto.Plain_Buffer_Index_Type (Item_Index);
      if not Primitive.Valid (Prim) or else
         Primitive.Operation (Prim) /= Write
      then
         Req := Request.Invalid_Object;
         return;
      end if;
      Req := Request.Valid_Object (
         Op     => CBE.Write,
         Succ   => False,
         Blk_Nr => Primitive.Block_Number (Prim),
         Off    => 0,
         Cnt    => 1,
         Tg     => 0);
   end Crypto_Cipher_Data_Required;

   procedure Crypto_Cipher_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index));
   end Crypto_Cipher_Data_Requested;

   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Cipher_Data;

   procedure Crypto_Plain_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Cipher_Buffer_Index_Type)
   is
      Item_Index : Crypto.Item_Index_Type;
      Prim       : Primitive.Object_Type;
   begin
      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Item_Index, Prim);
      Data_Index := Crypto.Cipher_Buffer_Index_Type (Item_Index);
      if not Primitive.Valid (Prim) or else
         Primitive.Operation (Prim) /= Read
      then
         Req := Request.Invalid_Object;
         return;
      end if;
      Req := Request.Valid_Object (
         Op     => CBE.Read,
         Succ   => False,
         Blk_Nr => Primitive.Block_Number (Prim),
         Off    => 0,
         Cnt    => 1,
         Tg     => 0);
   end Crypto_Plain_Data_Required;

   procedure Crypto_Plain_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index));
   end Crypto_Plain_Data_Requested;

   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Item_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Plain_Data;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function To_String (WFE : Wait_For_Event_Type)
   return String
   is (
      "WFE (Req=" & Request.To_String (WFE.Req) &
      ", Prim="        & Primitive.To_String (WFE.Prim) &
      ", Event="       & To_String (WFE.Event) &
      ", In_Progress=" & Debug.To_String (WFE.In_Progress) & ")");

   function To_String (Obj : Object_Type)
   return String
   is (
      "CBE=(" &
      ", Wait_For_Front_End=" & To_String (Obj.Wait_For_Front_End) &
      ", VBD="                & Virtual_Block_Device.To_String (Obj.VBD) &
      ", Secure_Superblock="  & Debug.To_String (Obj.Secure_Superblock) &
      ")");

end CBE.Library;
