--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with SHA256_4K;
with Interfaces;
with CBE.Debug;

use Interfaces;

package body CBE.VBD_Rekeying
with SPARK_Mode
is
   --
   --  CBE_Hash_From_SHA256_4K_Hash
   --
   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   --
   --  SHA256_4K_Data_From_CBE_Data
   --
   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   --
   --  Hash_Of_T1_Node_Blk
   --
   function Hash_Of_T1_Node_Blk (T1_Blk : Type_1_Node_Block_Type)
   return Hash_Type;

   --
   --  Hash_Of_Data_Blk
   --
   function Hash_Of_Data_Blk (CBE_Data : Block_Data_Type)
   return Hash_Type;

   --
   --  CBE_Hash_From_SHA256_4K_Hash
   --
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

   --
   --  SHA256_4K_Data_From_CBE_Data
   --
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

   --
   --  Hash_Of_T1_Node_Blk
   --
   function Hash_Of_T1_Node_Blk (T1_Blk : Type_1_Node_Block_Type)
   return Hash_Type
   is
   begin
      Declare_Hash_Data :
      declare
         SHA_Hash : SHA256_4K.Hash_Type;
         SHA_Data : SHA256_4K.Data_Type;
         CBE_Data : Block_Data_Type;
         CBE_Hash : Hash_Type;
      begin
         Block_Data_From_Type_1_Node_Block (CBE_Data, T1_Blk);
         SHA256_4K_Data_From_CBE_Data (SHA_Data, CBE_Data);
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
         return CBE_Hash;
      end Declare_Hash_Data;

   end Hash_Of_T1_Node_Blk;

   --
   --  Hash_Of_Data_Blk
   --
   function Hash_Of_Data_Blk (CBE_Data : Block_Data_Type)
   return Hash_Type
   is
      SHA_Hash : SHA256_4K.Hash_Type;
      SHA_Data : SHA256_4K.Data_Type;
      CBE_Hash : Hash_Type;
   begin
      SHA256_4K_Data_From_CBE_Data (SHA_Data, CBE_Data);
      SHA256_4K.Hash (SHA_Data, SHA_Hash);
      CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
      return CBE_Hash;
   end Hash_Of_Data_Blk;

   --
   --  Log_2
   --
   function  Log_2 (Value : Unsigned_32)
   return Unsigned_32
   is
      type Bit_Index_Type is range 0 .. 31;
   begin
      if Value = 0 then
         raise Program_Error;
      end if;
      for Bit_Index in reverse Bit_Index_Type'Range loop
         if (
            Value and
            Shift_Left (Unsigned_32 (1), Natural (Bit_Index))) /= 0
         then
            return Unsigned_32 (Bit_Index);
         end if;
      end loop;
      raise Program_Error;
   end Log_2;

   --
   --  Initialize_Rekeying
   --
   procedure Initialize_Rekeying (Rkg : out Rekeying_Type)
   is
   begin
      Initialize_Each_Job :
      for Idx in Rkg.Jobs'Range loop
         Rkg.Jobs (Idx) := (
            Operation => Invalid,
            State => Job_State_Type'First,
            Submitted_Prim => Primitive.Invalid_Object,
            Generated_Prim => Primitive.Invalid_Object,
            Snapshots => (others => Snapshot_Invalid),
            Snapshots_Degree => Tree_Degree_Type'First,
            Snapshot_Idx => Snapshots_Index_Type'First,
            First_Snapshot => Boolean'First,
            Old_Key_ID => Key_ID_Type'First,
            New_Key_ID => Key_ID_Type'First,
            T1_Blks => (others => (others => Type_1_Node_Invalid)),
            T1_Blks_Old_PBAs => (others => Physical_Block_Address_Type'First),
            T1_Blk_Idx => Type_1_Node_Blocks_Index_Type'First,
            Data_Blk => (others => Byte_Type'First),
            Data_Blk_Old_PBA => Physical_Block_Address_Type'First,
            VBA => Virtual_Block_Address_Type'First);
      end loop Initialize_Each_Job;
   end Initialize_Rekeying;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Rkg : Rekeying_Type)
   return Boolean
   is (for some Job of Rkg.Jobs => Job.Operation = Invalid);

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      VBA              :        Virtual_Block_Address_Type;
      Snapshots        :        Snapshots_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      Old_Key_ID       :        Key_ID_Type;
      New_Key_ID       :        Key_ID_Type)
   is
   begin

      Find_Invalid_Job :
      for Idx in Rkg.Jobs'Range loop

         if Rkg.Jobs (Idx).Operation = Invalid then

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_VBD_Rkg =>

               Rkg.Jobs (Idx).Operation        := Rekey_VBA;
               Rkg.Jobs (Idx).Submitted_Prim   := Prim;
               Rkg.Jobs (Idx).VBA              := VBA;
               Rkg.Jobs (Idx).Snapshots        := Snapshots;
               Rkg.Jobs (Idx).Snapshots_Degree := Snapshots_Degree;
               Rkg.Jobs (Idx).Old_Key_ID       := Old_Key_ID;
               Rkg.Jobs (Idx).New_Key_ID       := New_Key_ID;
               Rkg.Jobs (Idx).State            := Submitted;
               return;

            when others =>

               raise Program_Error;

            end case;

         end if;

      end loop Find_Invalid_Job;
      raise Program_Error;

   end Submit_Primitive;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type
   is
   begin
      Find_Completed_Job :
      for Idx in Rkg.Jobs'Range loop
         if Rkg.Jobs (Idx).Operation /= Invalid and then
            Rkg.Jobs (Idx).State = Completed
         then
            return Rkg.Jobs (Idx).Submitted_Prim;
         end if;
      end loop Find_Completed_Job;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Find_Corresponding_Job :
      for Idx in Rkg.Jobs'Range loop
         if Rkg.Jobs (Idx).Operation /= Invalid and then
            Rkg.Jobs (Idx).State = Completed and then
            Primitive.Equal (Prim, Rkg.Jobs (Idx).Submitted_Prim)
         then
            Rkg.Jobs (Idx).Operation := Invalid;
            return;
         end if;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Drop_Completed_Primitive;

   --
   --  Child_Idx_For_VBA
   --
   function Child_Idx_For_VBA (
      VBA  : Virtual_Block_Address_Type;
      Lvl  : Type_1_Node_Blocks_Index_Type;
      Degr : Tree_Degree_Type)
   return Type_1_Node_Block_Index_Type
   is
      Degree_Log_2 : constant Tree_Degree_Log_2_Type :=
         Tree_Degree_Log_2_Type (Log_2 (Unsigned_32 (Degr)));

      Degree_Mask : constant Tree_Degree_Mask_Type :=
         Tree_Degree_Mask_Type (
            Shift_Left (Unsigned_32 (1), Natural (Degree_Log_2)) - 1);
   begin
      return
         Type_1_Node_Block_Index_Type (
            Shift_Right (
               Unsigned_64 (VBA),
               Natural (Unsigned_32 (Degree_Log_2) * (Unsigned_32 (Lvl) - 1)))
            and
            Unsigned_64 (Degree_Mask));
   end Child_Idx_For_VBA;

   --
   --  Execute_Rekey_VBA_Read_Node_Completed
   --
   procedure Execute_Rekey_VBA_Read_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Hash     :        Hash_Type;
      Progress : in out Boolean)
   is
   begin

      if Hash_Of_T1_Node_Blk (Job.T1_Blks (Job.T1_Blk_Idx)) /= Hash then
         raise Program_Error;
      end if;

      if Job.T1_Blk_Idx > Type_1_Node_Blocks_Index_Type'First then

         Declare_Child_PBA_1 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.T1_Blk_Idx;

            Child_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Type_1_Node_Blocks_Index_Type'Pred (Parent_Lvl_Idx);

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.Snapshots_Degree);

            Child_PBA : constant Physical_Block_Address_Type :=
               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).PBA;
         begin

            Job.T1_Blk_Idx := Child_Lvl_Idx;
            Job.T1_Blks_Old_PBAs (Child_Lvl_Idx) := Child_PBA;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_VBD_Rkg_Cache,
               Blk_Nr => Block_Number_Type (Child_PBA),
               Idx    => Primitive.Index_Type (Job_Idx));

         end Declare_Child_PBA_1;

         Debug.Print_String (
            "READ LVL " &
            Debug.To_String (Debug.Uint64_Type (Job.T1_Blk_Idx)) &
            " PBA " &
            Debug.To_String (Debug.Uint64_Type (
               Job.T1_Blks_Old_PBAs (Job.T1_Blk_Idx))));

         Job.State := Read_Inner_Node_Pending;
         Progress := True;

      else

         Declare_Child_PBA_2 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.T1_Blk_Idx;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.Snapshots_Degree);

            Child_PBA : constant Physical_Block_Address_Type :=
               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).PBA;
         begin

            Job.Data_Blk_Old_PBA := Child_PBA;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_VBD_Rkg_Blk_IO,
               Blk_Nr => Block_Number_Type (Child_PBA),
               Idx    => Primitive.Index_Type (Job_Idx));

         end Declare_Child_PBA_2;

         Debug.Print_String (
            "READ LVL " &
            Debug.To_String (Debug.Uint64_Type (0)) &
            " PBA " &
            Debug.To_String (Debug.Uint64_Type (Job.Data_Blk_Old_PBA)));

         Job.State := Read_Leaf_Node_Pending;
         Progress := True;

      end if;

   end Execute_Rekey_VBA_Read_Node_Completed;

   --
   --  Execute_Rekey_VBA
   --
   procedure Execute_Rekey_VBA (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Declare_Newest_Snap_Idx :
         declare
            Newest_Snap_Idx : Snapshots_Index_Type :=
               Snapshots_Index_Type'First;

            Newest_Snap_Idx_Valid : Boolean := False;
         begin
            For_Each_Snap_Idx :
            for Snap_Idx in Job.Snapshots'Range loop
               if Job.Snapshots (Snap_Idx).Valid and then
                  (not Newest_Snap_Idx_Valid or else
                   Job.Snapshots (Snap_Idx).Gen >
                   Job.Snapshots (Newest_Snap_Idx).Gen)
               then
                  Newest_Snap_Idx := Snap_Idx;
                  Newest_Snap_Idx_Valid := True;
               end if;
            end loop For_Each_Snap_Idx;

            if Newest_Snap_Idx_Valid then
               Job.Snapshot_Idx := Newest_Snap_Idx;
            else
               raise Program_Error;
            end if;
         end Declare_Newest_Snap_Idx;

         Job.First_Snapshot := True;
         Job.T1_Blk_Idx :=
            Type_1_Node_Blocks_Index_Type (
               Job.Snapshots (Job.Snapshot_Idx).Max_Level);

         Job.T1_Blks_Old_PBAs (Job.T1_Blk_Idx) :=
            Job.Snapshots (Job.Snapshot_Idx).PBA;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_VBD_Rkg_Cache,
            Blk_Nr => Block_Number_Type (Job.Snapshots (Job.Snapshot_Idx).PBA),
            Idx    => Primitive.Index_Type (Job_Idx));

         Debug.Print_String (
            "READ LVL " &
            Debug.To_String (Debug.Uint64_Type (Job.T1_Blk_Idx)) &
            " PBA " &
            Debug.To_String (Debug.Uint64_Type (
               Job.T1_Blks_Old_PBAs (Job.T1_Blk_Idx))));

         Job.State := Read_Root_Node_Pending;
         Progress := True;

      when Read_Root_Node_Completed =>

         Execute_Rekey_VBA_Read_Node_Completed (
            Job, Job_Idx, Job.Snapshots (Job.Snapshot_Idx).Hash, Progress);

      when Read_Inner_Node_Completed =>

         Declare_Child_Idx_1 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.T1_Blk_Idx + 1;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.Snapshots_Degree);
         begin

            Execute_Rekey_VBA_Read_Node_Completed (
               Job, Job_Idx, Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash,
               Progress);

         end Declare_Child_Idx_1;

      when Read_Leaf_Node_Completed =>

         Declare_Child_Idx_2 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Type_1_Node_Blocks_Index_Type'First;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.Snapshots_Degree);
         begin

            if Hash_Of_Data_Blk (Job.Data_Blk) /=
                  Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash
            then
               raise Program_Error;
            end if;

         end Declare_Child_Idx_2;

         Job.State := Decrypt_Leaf_Node_Pending;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Rekey_VBA;

   --
   --  Execute
   --
   procedure Execute (
      Rkg      : in out Rekeying_Type;
      Progress : in out Boolean)
   is
   begin

      Execute_Each_Valid_Job :
      for Idx in Rkg.Jobs'Range loop

         case Rkg.Jobs (Idx).Operation is
         when Rekey_VBA =>

            Execute_Rekey_VBA (Rkg.Jobs (Idx), Idx, Progress);

         when Invalid =>

            null;

         end case;

      end loop Execute_Each_Valid_Job;

   end Execute;

   --
   --  Peek_Generated_Blk_IO_Primitive
   --
   function Peek_Generated_Blk_IO_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Rkg.Jobs'Range loop

         case Rkg.Jobs (Idx).Operation is
         when Rekey_VBA =>

            case Rkg.Jobs (Idx).State is
            when Read_Leaf_Node_Pending =>

               return Rkg.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         when others =>

            null;

         end case;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Blk_IO_Primitive;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Rkg.Jobs'Range loop

         case Rkg.Jobs (Idx).Operation is
         when Rekey_VBA =>

            case Rkg.Jobs (Idx).State is
            when Read_Root_Node_Pending | Read_Inner_Node_Pending =>

               return Rkg.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         when others =>

            null;

         end case;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Cache_Primitive;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type)
   return Block_Data_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));

      Blk_Data : Block_Data_Type;
   begin
      if Rkg.Jobs (Idx).Operation /= Invalid then

         case Rkg.Jobs (Idx).State is
         when Write_Root_Node_Pending =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Block_Data_From_Type_1_Node_Block (
               Blk_Data, Rkg.Jobs (Idx).T1_Blks (Rkg.Jobs (Idx).T1_Blk_Idx));

            return Blk_Data;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Blk_Data;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rkg.Jobs (Idx).Operation /= Invalid then

         case Rkg.Jobs (Idx).State is
         when Read_Root_Node_Pending =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Root_Node_In_Progress;
            return;

         when Read_Inner_Node_Pending =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Inner_Node_In_Progress;
            return;

         when Read_Leaf_Node_Pending =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Leaf_Node_In_Progress;
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Prim_Completed_Blk_Data
   --
   procedure Mark_Generated_Prim_Completed_Blk_Data (
      Rkg      : in out Rekeying_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rkg.Jobs (Idx).Operation /= Invalid then

         case Rkg.Jobs (Idx).State is
         when Read_Root_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Root_Node_Completed;
            Rkg.Jobs (Idx).Generated_Prim := Prim;
            Type_1_Node_Block_From_Block_Data (
               Rkg.Jobs (Idx).T1_Blks (Rkg.Jobs (Idx).T1_Blk_Idx), Blk_Data);

            return;

         when Read_Inner_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Inner_Node_Completed;
            Rkg.Jobs (Idx).Generated_Prim := Prim;
            Type_1_Node_Block_From_Block_Data (
               Rkg.Jobs (Idx).T1_Blks (Rkg.Jobs (Idx).T1_Blk_Idx), Blk_Data);

            return;

         when Read_Leaf_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Read_Leaf_Node_Completed;
            Rkg.Jobs (Idx).Generated_Prim := Prim;
            Rkg.Jobs (Idx).Data_Blk := Blk_Data;
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed_Blk_Data;

   --
   --  Mark_Generated_Prim_Completed
   --
   procedure Mark_Generated_Prim_Completed (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rkg.Jobs (Idx).Operation /= Invalid then

         case Rkg.Jobs (Idx).State is
         when Write_Root_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rkg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rkg.Jobs (Idx).State := Write_Root_Node_Completed;
            Rkg.Jobs (Idx).Generated_Prim := Prim;
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed;

end CBE.VBD_Rekeying;
