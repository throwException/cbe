--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;
with SHA256_4K;

pragma Unreferenced (CBE.Debug);

package body CBE.Superblock_Control
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
   --  Hash_Of_Superblock
   --
   function Hash_Of_Superblock (SB : Superblock_Type)
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
   --  Hash_Of_Superblock
   --
   function Hash_Of_Superblock (SB : Superblock_Type)
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
         Block_Data_From_Superblock (CBE_Data, SB);
         SHA256_4K_Data_From_CBE_Data (SHA_Data, CBE_Data);
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
         return CBE_Hash;
      end Declare_Hash_Data;

   end Hash_Of_Superblock;

   --
   --  Initialize_Control
   --
   procedure Initialize_Control (Ctrl : out Control_Type)
   is
   begin
      Initialize_Each_Job :
      for Idx in Ctrl.Jobs'Range loop
         Ctrl.Jobs (Idx) := (
            Operation => Invalid,
            State => Job_State_Type'First,
            Submitted_Prim => Primitive.Invalid_Object,
            Generated_Prim => Primitive.Invalid_Object,
            Key_Plaintext => Key_Plaintext_Invalid,
            Generation => Generation_Type'First,
            Hash => (others => Byte_Type'First),
            PBA => Physical_Block_Address_Type'First,
            Nr_Of_Blks => Number_Of_Blocks_Type'First,
            Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            SB_Ciphertext => Superblock_Ciphertext_Invalid,
            SB_Idx => Superblocks_Index_Type'First,
            SB_Found => Boolean'First,
            Read_SB_Idx => Superblocks_Index_Type'First,
            Request_Finished => Boolean'First,
            Snapshots => (others => Snapshot_Invalid),
            FT_Root => Type_1_Node_Invalid,
            FT_Max_Lvl_Idx => Tree_Level_Index_Type'First,
            FT_Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            Prev_Key_Plaintext => Key_Plaintext_Invalid,
            Curr_Key_Plaintext => Key_Plaintext_Invalid);
      end loop Initialize_Each_Job;
   end Initialize_Control;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Ctrl : Control_Type)
   return Boolean
   is (for some Job of Ctrl.Jobs => Job.Operation = Invalid);

   --
   --  Discard_Disposable_Snapshots
   --
   procedure Discard_Disposable_Snapshots (
      Snapshots        : in out Snapshots_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type)
   is
   begin

      For_Each_Snap :
      for Idx in Snapshots'Range loop

         if Snapshots (Idx).Valid and then
            not Snapshots (Idx).Keep and then
            Snapshots (Idx).Gen /= Curr_Gen and then
            Snapshots (Idx).Gen /= Last_Secured_Gen
         then
            Snapshots (Idx).Valid := False;
         end if;

      end loop For_Each_Snap;

   end Discard_Disposable_Snapshots;

   --
   --  Submit_Primitive_Nr_Of_Blks
   --
   procedure Submit_Primitive_Nr_Of_Blks (
      Ctrl       : in out Control_Type;
      Prim       :        Primitive.Object_Type;
      Nr_Of_PBAs :        Number_Of_Blocks_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Ctrl.Jobs'Range loop
         if Ctrl.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step =>

               Ctrl.Jobs (Idx).Operation := VBD_Extension_Step;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               Ctrl.Jobs (Idx).Nr_Of_Blks := Nr_Of_PBAs;
               return;

            when Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step =>

               Ctrl.Jobs (Idx).Operation := FT_Extension_Step;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               Ctrl.Jobs (Idx).Nr_Of_Blks := Nr_Of_PBAs;
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive_Nr_Of_Blks;

   --
   --  Submit_Primitive_Gen
   --
   procedure Submit_Primitive_Gen (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type;
      Gen  :        Generation_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Ctrl.Jobs'Range loop
         if Ctrl.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when Primitive.Tag_Pool_SB_Ctrl_Discard_Snap =>

               Ctrl.Jobs (Idx).Operation := Discard_Snapshot;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               Ctrl.Jobs (Idx).Generation := Gen;
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive_Gen;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Ctrl.Jobs'Range loop
         if Ctrl.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when Primitive.Tag_Pool_SB_Ctrl_Init_Rekey =>

               Ctrl.Jobs (Idx).Operation := Initialize_Rekeying;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               return;

            when Primitive.Tag_Pool_SB_Ctrl_Rekey_VBA =>

               Ctrl.Jobs (Idx).Operation := Rekey_VBA;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               return;

            when Primitive.Tag_Pool_SB_Ctrl_Initialize =>

               Ctrl.Jobs (Idx).Operation := Initialize;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               return;

            when Primitive.Tag_Pool_SB_Ctrl_Deinitialize =>

               Ctrl.Jobs (Idx).Operation := Deinitialize;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
               return;

            when Primitive.Tag_Pool_SB_Ctrl_Create_Snap =>

               Ctrl.Jobs (Idx).Operation := Create_Snapshot;
               Ctrl.Jobs (Idx).State := Submitted;
               Ctrl.Jobs (Idx).Submitted_Prim := Prim;
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
   function Peek_Completed_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Find_Completed_Job :
      for Idx in Ctrl.Jobs'Range loop
         if Ctrl.Jobs (Idx).Operation /= Invalid and then
            Ctrl.Jobs (Idx).State = Completed
         then
            return Ctrl.Jobs (Idx).Submitted_Prim;
         end if;
      end loop Find_Completed_Job;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Peek_Completed_Request_Finished
   --
   function Peek_Completed_Request_Finished (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Boolean
   is
   begin
      Find_Corresponding_Job :
      for Idx in Ctrl.Jobs'Range loop
         case Ctrl.Jobs (Idx).Operation is
         when Rekey_VBA | VBD_Extension_Step | FT_Extension_Step =>
            if Ctrl.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Ctrl.Jobs (Idx).Submitted_Prim)
            then
               return Ctrl.Jobs (Idx).Request_Finished;
            end if;
         when others =>
            raise Program_Error;
         end case;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Peek_Completed_Request_Finished;

   --
   --  Peek_Completed_Previous_Key_Plaintext
   --
   function Peek_Completed_Previous_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type
   is
   begin
      Find_Corresponding_Job :
      for Idx in Ctrl.Jobs'Range loop
         case Ctrl.Jobs (Idx).Operation is
         when Initialize =>

            if Ctrl.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Ctrl.Jobs (Idx).Submitted_Prim)
            then
               return Ctrl.Jobs (Idx).Prev_Key_Plaintext;
            end if;

         when others =>

            raise Program_Error;

         end case;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Peek_Completed_Previous_Key_Plaintext;

   --
   --  Peek_Completed_Current_Key_Plaintext
   --
   function Peek_Completed_Current_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type
   is
   begin
      Find_Corresponding_Job :
      for Idx in Ctrl.Jobs'Range loop
         case Ctrl.Jobs (Idx).Operation is
         when Initialize =>

            if Ctrl.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Ctrl.Jobs (Idx).Submitted_Prim)
            then
               return Ctrl.Jobs (Idx).Curr_Key_Plaintext;
            end if;

         when others =>

            raise Program_Error;

         end case;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Peek_Completed_Current_Key_Plaintext;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type
   is
   begin
      Find_Corresponding_Job :
      for Idx in Ctrl.Jobs'Range loop
         case Ctrl.Jobs (Idx).Operation is
         when Create_Snapshot =>

            if Ctrl.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Ctrl.Jobs (Idx).Submitted_Prim)
            then
               return Ctrl.Jobs (Idx).Generation;
            end if;

         when others =>

            raise Program_Error;

         end case;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Peek_Completed_Generation;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Find_Corresponding_Job :
      for Idx in Ctrl.Jobs'Range loop
         if Ctrl.Jobs (Idx).Operation /= Invalid and then
            Ctrl.Jobs (Idx).State = Completed and then
            Primitive.Equal (Prim, Ctrl.Jobs (Idx).Submitted_Prim)
         then
            Ctrl.Jobs (Idx).Operation := Invalid;
            return;
         end if;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Drop_Completed_Primitive;

   --
   --  Superblock_Enter_Rekeying_State
   --
   procedure Superblock_Enter_Rekeying_State (
      SB        : in out Superblock_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
   begin

      if SB.State /= Normal then
         raise Program_Error;
      end if;

      SB.State := Rekeying;
      SB.Rekeying_VBA := 0;
      SB.Previous_Key := SB.Current_Key;
      SB.Current_Key := (
         Value => Key_Value,
         ID => SB.Previous_Key.ID + 1);

   end Superblock_Enter_Rekeying_State;

   --
   --  Init_SB_Ciphertext_Without_Key_Values
   --
   procedure Init_SB_Ciphertext_Without_Key_Values (
      SB_Plain  :     Superblock_Type;
      SB_Cipher : out Superblock_Ciphertext_Type)
   is
   begin

      SB_Cipher.State                   := SB_Plain.State;
      SB_Cipher.Rekeying_VBA            := SB_Plain.Rekeying_VBA;
      SB_Cipher.Resizing_Nr_Of_PBAs     := SB_Plain.Resizing_Nr_Of_PBAs;
      SB_Cipher.Resizing_Nr_Of_Leaves   := SB_Plain.Resizing_Nr_Of_Leaves;
      SB_Cipher.First_PBA               := SB_Plain.First_PBA;
      SB_Cipher.Nr_Of_PBAs              := SB_Plain.Nr_Of_PBAs;
      SB_Cipher.Previous_Key.Value      := (others => Byte_Type'First);
      SB_Cipher.Previous_Key.ID         := SB_Plain.Previous_Key.ID;
      SB_Cipher.Current_Key.Value       := (others => Byte_Type'First);
      SB_Cipher.Current_Key.ID          := SB_Plain.Current_Key.ID;
      SB_Cipher.Snapshots               := SB_Plain.Snapshots;
      SB_Cipher.Last_Secured_Generation := SB_Plain.Last_Secured_Generation;
      SB_Cipher.Curr_Snap               := SB_Plain.Curr_Snap;
      SB_Cipher.Degree                  := SB_Plain.Degree;
      SB_Cipher.Free_Gen                := SB_Plain.Free_Gen;
      SB_Cipher.Free_Number             := SB_Plain.Free_Number;
      SB_Cipher.Free_Hash               := SB_Plain.Free_Hash;
      SB_Cipher.Free_Max_Level          := SB_Plain.Free_Max_Level;
      SB_Cipher.Free_Degree             := SB_Plain.Free_Degree;
      SB_Cipher.Free_Leafs              := SB_Plain.Free_Leafs;
      SB_Cipher.Meta_Gen                := SB_Plain.Meta_Gen;
      SB_Cipher.Meta_Number             := SB_Plain.Meta_Number;
      SB_Cipher.Meta_Hash               := SB_Plain.Meta_Hash;
      SB_Cipher.Meta_Max_Level          := SB_Plain.Meta_Max_Level;
      SB_Cipher.Meta_Degree             := SB_Plain.Meta_Degree;
      SB_Cipher.Meta_Leafs              := SB_Plain.Meta_Leafs;

   end Init_SB_Ciphertext_Without_Key_Values;

   --
   --  Init_SB_Plaintext_Without_Key_Values
   --
   procedure Init_SB_Plaintext_Without_Key_Values (
      SB_Cipher :     Superblock_Ciphertext_Type;
      SB_Plain  : out Superblock_Type)
   is
   begin

      SB_Plain.State                   := SB_Cipher.State;
      SB_Plain.Rekeying_VBA            := SB_Cipher.Rekeying_VBA;
      SB_Plain.Resizing_Nr_Of_PBAs     := SB_Cipher.Resizing_Nr_Of_PBAs;
      SB_Plain.Resizing_Nr_Of_Leaves   := SB_Cipher.Resizing_Nr_Of_Leaves;
      SB_Plain.First_PBA               := SB_Cipher.First_PBA;
      SB_Plain.Nr_Of_PBAs              := SB_Cipher.Nr_Of_PBAs;
      SB_Plain.Previous_Key.Value      := (others => Byte_Type'First);
      SB_Plain.Previous_Key.ID         := SB_Cipher.Previous_Key.ID;
      SB_Plain.Current_Key.Value       := (others => Byte_Type'First);
      SB_Plain.Current_Key.ID          := SB_Cipher.Current_Key.ID;
      SB_Plain.Snapshots               := SB_Cipher.Snapshots;
      SB_Plain.Last_Secured_Generation := SB_Cipher.Last_Secured_Generation;
      SB_Plain.Curr_Snap               := SB_Cipher.Curr_Snap;
      SB_Plain.Degree                  := SB_Cipher.Degree;
      SB_Plain.Free_Gen                := SB_Cipher.Free_Gen;
      SB_Plain.Free_Number             := SB_Cipher.Free_Number;
      SB_Plain.Free_Hash               := SB_Cipher.Free_Hash;
      SB_Plain.Free_Max_Level          := SB_Cipher.Free_Max_Level;
      SB_Plain.Free_Degree             := SB_Cipher.Free_Degree;
      SB_Plain.Free_Leafs              := SB_Cipher.Free_Leafs;
      SB_Plain.Meta_Gen                := SB_Cipher.Meta_Gen;
      SB_Plain.Meta_Number             := SB_Cipher.Meta_Number;
      SB_Plain.Meta_Hash               := SB_Cipher.Meta_Hash;
      SB_Plain.Meta_Max_Level          := SB_Cipher.Meta_Max_Level;
      SB_Plain.Meta_Degree             := SB_Cipher.Meta_Degree;
      SB_Plain.Meta_Leafs              := SB_Cipher.Meta_Leafs;

   end Init_SB_Plaintext_Without_Key_Values;

   --
   --  Execute_VBD_Extension_Step
   --
   procedure Execute_VBD_Extension_Step (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         case SB.State is
         when Normal =>

            Job.Request_Finished := False;

            Declare_Nr_Of_Unused_PBAs_1 :
            declare
               Last_Used_PBA : constant Physical_Block_Address_Type :=
                  SB.First_PBA + (
                     Physical_Block_Address_Type (SB.Nr_Of_PBAs) - 1);

               Nr_Of_Unused_PBAs : constant Number_Of_Blocks_Type :=
                  Number_Of_Blocks_Type (
                     Physical_Block_Address_Type'Last - Last_Used_PBA);
            begin

               if Job.Nr_Of_Blks > Nr_Of_Unused_PBAs then
                  raise Program_Error;
               end if;

               SB.State := Extending_VBD;
               SB.Resizing_Nr_Of_PBAs := Job.Nr_Of_Blks;
               SB.Resizing_Nr_Of_Leaves := 0;

               Job.PBA := Last_Used_PBA + 1;

               pragma Debug (Debug.Print_String (
                  "VBD EXT INIT PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.PBA)) &
                  " NR_OF_PBAS " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.Nr_Of_Blks)) &
                  " NR_OF_LEAVES " &
                  Debug.To_String (Debug.Uint64_Type (
                     SB.Resizing_Nr_Of_Leaves)) &
                  " "));

            end Declare_Nr_Of_Unused_PBAs_1;

            SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

            Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
            Job.Key_Plaintext := SB.Current_Key;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Encrypt_Current_Key_Pending;
            Progress := True;

         when Extending_VBD =>

            Declare_Nr_Of_Unused_PBAs_2 :
            declare
               Last_Used_PBA : constant Physical_Block_Address_Type :=
                  SB.First_PBA + (
                     Physical_Block_Address_Type (SB.Nr_Of_PBAs - 1));

               Nr_Of_Unused_PBAs : constant Number_Of_Blocks_Type :=
                  Number_Of_Blocks_Type (
                     Physical_Block_Address_Type'Last - Last_Used_PBA);
            begin

               if SB.Resizing_Nr_Of_PBAs > Nr_Of_Unused_PBAs then
                  raise Program_Error;
               end if;

               Job.PBA := Last_Used_PBA + 1;
               Job.Nr_Of_Blks := SB.Resizing_Nr_Of_PBAs;

               pragma Debug (Debug.Print_String (
                  "VBD EXT STEP PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.PBA)) &
                  " NR_OF_PBAS " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.Nr_Of_Blks)) &
                  " NR_OF_LEAVES " &
                  Debug.To_String (Debug.Uint64_Type (
                     SB.Resizing_Nr_Of_Leaves)) &
                  " "));

            end Declare_Nr_Of_Unused_PBAs_2;

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := VBD_Ext_Step_In_VBD_Pending;
            Progress := True;

         when others =>

            raise Program_Error;

         end case;

      when VBD_Ext_Step_In_VBD_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         if Job.Nr_Of_Blks >= SB.Resizing_Nr_Of_PBAs then
            raise Program_Error;
         end if;

         Declare_New_First_Unused_PBA :
         declare
            Nr_Of_Added_PBAs : constant Number_Of_Blocks_Type :=
               SB.Resizing_Nr_Of_PBAs - Job.Nr_Of_Blks;

            New_First_Unused_PBA : constant Physical_Block_Address_Type :=
               SB.First_PBA +
                  Physical_Block_Address_Type (
                     SB.Nr_Of_PBAs + Nr_Of_Added_PBAs);
         begin

            if Job.PBA /= New_First_Unused_PBA then
               raise Program_Error;
            end if;

            SB.Nr_Of_PBAs := SB.Nr_Of_PBAs + Nr_Of_Added_PBAs;

         end Declare_New_First_Unused_PBA;

         SB.Snapshots := Job.Snapshots;
         SB.Curr_Snap := Newest_Snapshot_Idx (Job.Snapshots);

         if Job.Nr_Of_Blks > 0 then

            SB.Resizing_Nr_Of_PBAs := Job.Nr_Of_Blks;
            SB.Resizing_Nr_Of_Leaves :=
               SB.Resizing_Nr_Of_Leaves + Job.Nr_Of_Leaves;

         else

            SB.State := Normal;
            Job.Request_Finished := True;

         end if;

         SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

         Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Current_Key_Pending;
         Progress := True;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Key_Plaintext := SB.Previous_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Previous_Key_Pending;
         Progress := True;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_VBD_Extension_Step;

   --
   --  Execute_FT_Extension_Step
   --
   procedure Execute_FT_Extension_Step (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         case SB.State is
         when Normal =>

            Job.Request_Finished := False;

            Declare_Nr_Of_Unused_PBAs_1 :
            declare
               Last_Used_PBA : constant Physical_Block_Address_Type :=
                  SB.First_PBA + (
                     Physical_Block_Address_Type (SB.Nr_Of_PBAs) - 1);

               Nr_Of_Unused_PBAs : constant Number_Of_Blocks_Type :=
                  Number_Of_Blocks_Type (
                     Physical_Block_Address_Type'Last - Last_Used_PBA);
            begin

               if Job.Nr_Of_Blks > Nr_Of_Unused_PBAs then
                  raise Program_Error;
               end if;

               SB.State := Extending_FT;
               SB.Resizing_Nr_Of_PBAs := Job.Nr_Of_Blks;
               SB.Resizing_Nr_Of_Leaves := 0;

               Job.PBA := Last_Used_PBA + 1;

               pragma Debug (Debug.Print_String (
                  "FT EXT INIT PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.PBA)) &
                  " NR_OF_PBAS " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.Nr_Of_Blks)) &
                  " NR_OF_LEAVES " &
                  Debug.To_String (Debug.Uint64_Type (
                     SB.Resizing_Nr_Of_Leaves)) &
                  " "));

            end Declare_Nr_Of_Unused_PBAs_1;

            SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

            Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
            Job.Key_Plaintext := SB.Current_Key;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Encrypt_Current_Key_Pending;
            Progress := True;

         when Extending_FT =>

            Declare_Nr_Of_Unused_PBAs_2 :
            declare
               Last_Used_PBA : constant Physical_Block_Address_Type :=
                  SB.First_PBA + (
                     Physical_Block_Address_Type (SB.Nr_Of_PBAs - 1));

               Nr_Of_Unused_PBAs : constant Number_Of_Blocks_Type :=
                  Number_Of_Blocks_Type (
                     Physical_Block_Address_Type'Last - Last_Used_PBA);
            begin

               if SB.Resizing_Nr_Of_PBAs > Nr_Of_Unused_PBAs then
                  raise Program_Error;
               end if;

               Job.PBA := Last_Used_PBA + 1;
               Job.Nr_Of_Blks := SB.Resizing_Nr_Of_PBAs;

               pragma Debug (Debug.Print_String (
                  "FT EXT STEP PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.PBA)) &
                  " NR_OF_PBAS " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.Nr_Of_Blks)) &
                  " NR_OF_LEAVES " &
                  Debug.To_String (Debug.Uint64_Type (
                     SB.Resizing_Nr_Of_Leaves)) &
                  " "));

            end Declare_Nr_Of_Unused_PBAs_2;

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := FT_Ext_Step_In_FT_Pending;
            Progress := True;

         when others =>

            raise Program_Error;

         end case;

      when FT_Ext_Step_In_FT_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         if Job.Nr_Of_Blks >= SB.Resizing_Nr_Of_PBAs then
            raise Program_Error;
         end if;

         Declare_New_First_Unused_PBA :
         declare
            Nr_Of_Added_PBAs : constant Number_Of_Blocks_Type :=
               SB.Resizing_Nr_Of_PBAs - Job.Nr_Of_Blks;

            New_First_Unused_PBA : constant Physical_Block_Address_Type :=
               SB.First_PBA +
                  Physical_Block_Address_Type (
                     SB.Nr_Of_PBAs + Nr_Of_Added_PBAs);
         begin

            if Job.PBA /= New_First_Unused_PBA then
               raise Program_Error;
            end if;

            SB.Nr_Of_PBAs := SB.Nr_Of_PBAs + Nr_Of_Added_PBAs;

         end Declare_New_First_Unused_PBA;

         SB.Free_Gen       := Job.FT_Root.Gen;
         SB.Free_Number    := Job.FT_Root.PBA;
         SB.Free_Hash      := Job.FT_Root.Hash;
         SB.Free_Max_Level := Job.FT_Max_Lvl_Idx;
         SB.Free_Leafs     := Job.FT_Nr_Of_Leaves;

         SB.Resizing_Nr_Of_PBAs := Job.Nr_Of_Blks;
         SB.Resizing_Nr_Of_Leaves :=
            SB.Resizing_Nr_Of_Leaves + Job.Nr_Of_Leaves;

         if Job.Nr_Of_Blks = 0 then

            SB.State := Normal;
            Job.Request_Finished := True;

         end if;

         SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

         pragma Debug (Debug.Print_String ("   SET SB"));
         pragma Debug (Debug.Print_String (
            "      STATE " &
            (case SB.State is
             when Normal => "Normal",
             when Extending_VBD => "Ext_VBD",
             when Extending_FT => "Ext_FT",
             when Rekeying => "Rekg")));

         pragma Debug (Debug.Print_String (
            "      RESIZING PBAS " &
            Debug.To_String (Debug.Uint64_Type (SB.Resizing_Nr_Of_PBAs)) &
            " LEAFS " &
            Debug.To_String (Debug.Uint64_Type (SB.Resizing_Nr_Of_Leaves))));

         pragma Debug (Debug.Print_String (
            "      CURR_SNAP PBA " &
            Debug.To_String (Debug.Uint64_Type (
               SB.Snapshots (SB.Curr_Snap).PBA)) &
            " GEN " &
            Debug.To_String (Debug.Uint64_Type (
               SB.Snapshots (SB.Curr_Snap).Gen))));

         pragma Debug (Debug.Print_String (
            "      FT PBA " &
            Debug.To_String (Debug.Uint64_Type (SB.Free_Number)) &
            " GEN " &
            Debug.To_String (Debug.Uint64_Type (SB.Free_Gen)) &
            " LEAFS " &
            Debug.To_String (Debug.Uint64_Type (SB.Free_Leafs)) &
            " MAX_LVL " &
            Debug.To_String (Debug.Uint64_Type (SB.Free_Max_Level)) &
            " " &
            Debug.To_String (SB.Free_Hash)));

         Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Current_Key_Pending;
         Progress := True;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Key_Plaintext := SB.Previous_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Previous_Key_Pending;
         Progress := True;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         pragma Debug (Debug.Print_String (
            "   WRITE SB"));

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_FT_Extension_Step;

   --
   --  Execute_Deinitialize
   --
   procedure Execute_Deinitialize (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            :        Superblock_Type;
      Progress      : in out Boolean)
   is
   begin
      case Job.State is
      when Submitted =>

         Job.Curr_Key_Plaintext.ID := SB.Current_Key.ID;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Crypto_Remove_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Remove_Current_Key_At_Crypto_Module_Pending;
         Progress := True;

      when Remove_Current_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         case SB.State is
         when Rekeying =>

            Job.Prev_Key_Plaintext.ID := SB.Previous_Key.ID;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_Crypto_Remove_Key,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Remove_Previous_Key_At_Crypto_Module_Pending;
            Progress := True;

         when Normal | Extending_VBD | Extending_FT =>

            Primitive.Success (Job.Submitted_Prim, True);
            Job.State := Completed;
            Progress := True;

         end case;

      when Remove_Previous_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;
   end Execute_Deinitialize;

   --
   --  Execute_Initialize
   --
   procedure Execute_Initialize (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      SB       : in out Superblock_Type;
      SB_Idx   : in out Superblocks_Index_Type;
      Curr_Gen : in out Generation_Type;
      Progress : in out Boolean)
   is
   begin
      case Job.State is
      when Submitted =>

         Job.SB_Found := False;
         Job.Read_SB_Idx := 0;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB,
            Blk_Nr => Block_Number_Type (Job.Read_SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Read_SB_Pending;
         Progress := True;

      when Read_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         if Superblock_Ciphertext_Valid (Job.SB_Ciphertext) then

            Declare_SB_Generation :
            declare
               SB_Generation : constant Generation_Type :=
                  Job.SB_Ciphertext.Snapshots (
                     Newest_Snapshot_Idx (Job.SB_Ciphertext.Snapshots)).Gen;
            begin

               if Job.SB_Found then

                  if Job.Generation > SB_Generation then

                     Job.Generation := SB_Generation;
                     Job.SB_Idx := Job.Read_SB_Idx;

                  elsif Job.Generation = SB_Generation then

                     raise Program_Error;

                  end if;

               else

                  Job.Generation := SB_Generation;
                  Job.SB_Idx := Job.Read_SB_Idx;
                  Job.SB_Found := True;

               end if;

            end Declare_SB_Generation;

         end if;

         if Job.Read_SB_Idx < Superblocks_Index_Type'Last then

            Job.Read_SB_Idx := Job.Read_SB_Idx + 1;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB,
               Blk_Nr => Block_Number_Type (Job.Read_SB_Idx),
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Read_SB_Pending;
            Progress := True;

         else

            if not Job.SB_Found then
               raise Program_Error;
            end if;

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB,
               Blk_Nr => Block_Number_Type (Job.SB_Idx),
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Read_Current_SB_Pending;
            Progress := True;

         end if;

      when Read_Current_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Decrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Decrypt_Current_Key_Pending;
         Progress := True;

      when Decrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Curr_Key_Plaintext.ID := Job.SB_Ciphertext.Current_Key.ID;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Crypto_Add_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Add_Current_Key_At_Crypto_Module_Pending;
         Progress := True;

      when Add_Current_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         case SB.State is
         when Rekeying =>

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_TA_Decrypt_Key,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Decrypt_Previous_Key_Pending;
            Progress := True;

         when Normal | Extending_VBD | Extending_FT =>

            Init_SB_Plaintext_Without_Key_Values (Job.SB_Ciphertext, SB);
            SB.Current_Key.Value := Job.Curr_Key_Plaintext.Value;
            SB_Idx := Job.SB_Idx;
            Curr_Gen := Job.Generation + 1;

            Primitive.Success (Job.Submitted_Prim, True);
            Job.State := Completed;
            Progress := True;

         end case;

      when Decrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Crypto_Add_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Add_Previous_Key_At_Crypto_Module_Pending;
         Progress := True;

      when Add_Previous_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Init_SB_Plaintext_Without_Key_Values (Job.SB_Ciphertext, SB);
         SB.Current_Key.Value := Job.Curr_Key_Plaintext.Value;
         SB.Previous_Key.Value := Job.Prev_Key_Plaintext.Value;
         SB_Idx := Job.SB_Idx;
         Curr_Gen := Job.Generation + 1;

         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;
   end Execute_Initialize;

   --
   --  Execute_Discard_Snapshot
   --
   procedure Execute_Discard_Snapshot (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Declare_Result_Of_Snapshot_Search :
         declare
            Snapshot_Found : Boolean := False;
            Snapshot_Idx : Snapshots_Index_Type;
         begin

            Search_For_Snapshot :
            for Idx in Snapshots_Index_Type loop

               if SB.Snapshots (Idx).Valid and then
                  SB.Snapshots (Idx).Keep and then
                  SB.Snapshots (Idx).Gen = Job.Generation
               then

                  Snapshot_Idx := Idx;
                  Snapshot_Found := True;
                  exit Search_For_Snapshot;

               end if;

            end loop Search_For_Snapshot;

            if Snapshot_Found then

               SB.Snapshots (Snapshot_Idx).Valid := False;

               Discard_Disposable_Snapshots (
                  SB.Snapshots,
                  SB.Last_Secured_Generation,
                  Curr_Gen);

               SB.Last_Secured_Generation := Curr_Gen;
               SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

               Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
               Job.Key_Plaintext := SB.Current_Key;
               Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Primitive_Operation_Type'First,
                  Succ   => False,
                  Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
                  Blk_Nr => Block_Number_Type'First,
                  Idx    => Primitive.Index_Type (Job_Idx));

               Job.State := Encrypt_Current_Key_Pending;
               Progress := True;

            else

               Primitive.Success (Job.Submitted_Prim, False);
               Job.State := Completed;
               Progress := True;

            end if;

         end Declare_Result_Of_Snapshot_Search;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Key_Plaintext := SB.Previous_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Previous_Key_Pending;
         Progress := True;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Discard_Snapshot;

   --
   --  Execute_Create_Snapshot
   --
   procedure Execute_Create_Snapshot (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Discard_Disposable_Snapshots (
            SB.Snapshots,
            SB.Last_Secured_Generation,
            Curr_Gen);

         SB.Last_Secured_Generation := Curr_Gen;
         SB.Snapshots (SB.Curr_Snap).Keep := True;
         SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

         Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Current_Key_Pending;
         Progress := True;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Key_Plaintext := SB.Previous_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Previous_Key_Pending;
         Progress := True;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Create_Snapshot;

   --
   --  Execute_Initialize_Rekeying
   --
   procedure Execute_Initialize_Rekeying (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Create_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Create_Key_Pending;
         Progress := True;

      when Create_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Superblock_Enter_Rekeying_State (SB, Job.Key_Plaintext.Value);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Crypto_Add_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Add_Key_At_Crypto_Module_Pending;
         Progress := True;

      when Add_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

         Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Current_Key_Pending;
         Progress := True;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Key_Plaintext := SB.Previous_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Previous_Key_Pending;
         Progress := True;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Initialize_Rekeying;

   --
   --  Execute_Rekey_VBA
   --
   procedure Execute_Rekey_VBA (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         if SB.State /= Rekeying then
            raise Program_Error;
         end if;

         pragma Debug (Debug.Print_String ("REKEY VBA " &
            Debug.To_String (Debug.Uint64_Type (SB.Rekeying_VBA))));

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA,
            Blk_Nr => Block_Number_Type (SB.Rekeying_VBA),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Rekey_VBA_In_VBD_Pending;
         Progress := True;

      when Rekey_VBA_In_VBD_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Snapshots := Job.Snapshots;

         Declare_Max_Nr_Of_Leafs :
         declare
            Max_Nr_Of_Leafs : Tree_Number_Of_Leafs_Type := 0;
         begin

            For_Each_Snap :
            for Snap of SB.Snapshots loop

               if Snap.Valid and then
                  Max_Nr_Of_Leafs < Snap.Nr_Of_Leafs
               then
                  Max_Nr_Of_Leafs := Snap.Nr_Of_Leafs;
               end if;

            end loop For_Each_Snap;

            if SB.Rekeying_VBA <
                  Virtual_Block_Address_Type (Max_Nr_Of_Leafs - 1)
            then

               SB.Rekeying_VBA := SB.Rekeying_VBA + 1;
               Job.Request_Finished := False;

               SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

               Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
               Job.Key_Plaintext := SB.Current_Key;
               Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Primitive_Operation_Type'First,
                  Succ   => False,
                  Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
                  Blk_Nr => Block_Number_Type'First,
                  Idx    => Primitive.Index_Type (Job_Idx));

               Job.State := Encrypt_Current_Key_Pending;
               Progress := True;

            else

               Job.Prev_Key_Plaintext.ID := SB.Previous_Key.ID;
               Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Primitive_Operation_Type'First,
                  Succ   => False,
                  Tg     => Primitive.Tag_SB_Ctrl_Crypto_Remove_Key,
                  Blk_Nr => Block_Number_Type'First,
                  Idx    => Primitive.Index_Type (Job_Idx));

               Job.State := Remove_Previous_Key_At_Crypto_Module_Pending;
               Progress := True;

            end if;

         end Declare_Max_Nr_Of_Leafs;

      when Remove_Previous_Key_At_Crypto_Module_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Previous_Key := Key_Plaintext_Invalid;
         SB.State := Normal;
         Job.Request_Finished := True;

         SB.Snapshots (SB.Curr_Snap).Gen := Curr_Gen;

         Init_SB_Ciphertext_Without_Key_Values (SB, Job.SB_Ciphertext);
         Job.Key_Plaintext := SB.Current_Key;
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Encrypt_Current_Key_Pending;
         Progress := True;

      when Encrypt_Current_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         if Job.Request_Finished then

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Sync,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_Cache,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Sync_Cache_Pending;
            Progress := True;

         else

            Job.Key_Plaintext := SB.Previous_Key;
            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_SB_Ctrl_TA_Encrypt_Key,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Encrypt_Previous_Key_Pending;
            Progress := True;

         end if;

      when Encrypt_Previous_Key_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Cache,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Cache_Pending;
         Progress := True;

      when Sync_Cache_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Write_SB_Pending;
         Progress := True;

      when Write_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_Blk_IO_Sync,
            Blk_Nr => Block_Number_Type (SB_Idx),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Sync_Blk_IO_Pending;
         Progress := True;

      when Sync_Blk_IO_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Hash := Hash_Of_Superblock (SB);
         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_SB_Ctrl_TA_Secure_SB,
            Blk_Nr => Block_Number_Type'First,
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := Secure_SB_Pending;

         if SB_Idx < Superblocks_Index_Type'Last then
            SB_Idx := SB_Idx + 1;
         else
            SB_Idx := Superblocks_Index_Type'First;
         end if;

         Job.Generation := Curr_Gen;
         Curr_Gen := Curr_Gen + 1;

         Progress := True;

      when Secure_SB_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         SB.Last_Secured_Generation := Job.Generation;
         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Rekey_VBA;

   --
   --  Execute
   --
   procedure Execute (
      Ctrl          : in out Control_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean)
   is
   begin

      Execute_Each_Valid_Job :
      for Idx in Ctrl.Jobs'Range loop

         case Ctrl.Jobs (Idx).Operation is
         when Initialize_Rekeying =>

            Execute_Initialize_Rekeying (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when Rekey_VBA =>

            Execute_Rekey_VBA (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when VBD_Extension_Step =>

            Execute_VBD_Extension_Step (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when FT_Extension_Step =>

            Execute_FT_Extension_Step (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when Create_Snapshot =>

            Execute_Create_Snapshot (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when Discard_Snapshot =>

            Execute_Discard_Snapshot (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when Initialize =>

            Execute_Initialize (
               Ctrl.Jobs (Idx), Idx, SB, SB_Idx, Curr_Gen, Progress);

         when Deinitialize =>

            Execute_Deinitialize (Ctrl.Jobs (Idx), Idx, SB, Progress);

         when Invalid =>

            null;

         end case;

      end loop Execute_Each_Valid_Job;

   end Execute;

   --
   --  Peek_Generated_TA_Primitive
   --
   function Peek_Generated_TA_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when Create_Key_Pending |
                 Encrypt_Current_Key_Pending |
                 Encrypt_Previous_Key_Pending |
                 Decrypt_Current_Key_Pending |
                 Decrypt_Previous_Key_Pending |
                 Secure_SB_Pending
            =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;
   end Peek_Generated_TA_Primitive;

   --
   --  Peek_Generated_Crypto_Primitive
   --
   function Peek_Generated_Crypto_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when
               Add_Key_At_Crypto_Module_Pending |
               Add_Current_Key_At_Crypto_Module_Pending |
               Add_Previous_Key_At_Crypto_Module_Pending |
               Remove_Previous_Key_At_Crypto_Module_Pending |
               Remove_Current_Key_At_Crypto_Module_Pending
            =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;
   end Peek_Generated_Crypto_Primitive;

   --
   --  Peek_Generated_VBD_Rkg_Primitive
   --
   function Peek_Generated_VBD_Rkg_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when
               Rekey_VBA_In_VBD_Pending |
               VBD_Ext_Step_In_VBD_Pending
            =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;
   end Peek_Generated_VBD_Rkg_Primitive;

   --
   --  Peek_Generated_FT_Rszg_Primitive
   --
   function Peek_Generated_FT_Rszg_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when FT_Ext_Step_In_FT_Pending
            =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;
   end Peek_Generated_FT_Rszg_Primitive;

   --
   --  Peek_Generated_Hash
   --
   function Peek_Generated_Hash (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Secure_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Hash;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Hash;

   --
   --  Peek_Generated_Last_Secured_Gen
   --
   function Peek_Generated_Last_Secured_Gen (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Generation_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then
               return SB.Last_Secured_Generation;
            else
               raise Program_Error;
            end if;

         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_VBD
            then
               return SB.Last_Secured_Generation;
            else
               raise Program_Error;
            end if;

         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return SB.Last_Secured_Generation;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Last_Secured_Gen;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Virtual_Block_Address_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then
               return SB.Rekeying_VBA;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_VBA;

   --
   --  Peek_Generated_PBA
   --
   function Peek_Generated_PBA (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Physical_Block_Address_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_VBD
            then
               return
                  SB.First_PBA + Physical_Block_Address_Type (SB.Nr_Of_PBAs);
            else
               raise Program_Error;
            end if;

         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return
                  SB.First_PBA + Physical_Block_Address_Type (SB.Nr_Of_PBAs);
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_PBA;

   --
   --  Peek_Generated_Nr_Of_Blks
   --
   function Peek_Generated_Nr_Of_Blks (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Number_Of_Blocks_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_VBD
            then
               return SB.Resizing_Nr_Of_PBAs;
            else
               raise Program_Error;
            end if;

         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return SB.Resizing_Nr_Of_PBAs;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Nr_Of_Blks;

   --
   --  Peek_Generated_Snapshots
   --
   function Peek_Generated_Snapshots (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Snapshots_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then
               return SB.Snapshots;
            else
               raise Program_Error;
            end if;

         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_VBD
            then
               return SB.Snapshots;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Snapshots;

   --
   --  Peek_Generated_Snapshots_Degree
   --
   function Peek_Generated_Snapshots_Degree (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Degree_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then
               return SB.Degree;
            else
               raise Program_Error;
            end if;

         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_VBD
            then
               return SB.Degree;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Snapshots_Degree;

   --
   --  Peek_Generated_FT_Root
   --
   function Peek_Generated_FT_Root (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Type_1_Node_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return (SB.Free_Number, SB.Free_Gen, SB.Free_Hash);
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_FT_Root;

   --
   --  Peek_Generated_FT_Degree
   --
   function Peek_Generated_FT_Degree (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Degree_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return SB.Free_Degree;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_FT_Degree;

   --
   --  Peek_Generated_FT_Nr_Of_Leaves
   --
   function Peek_Generated_FT_Nr_Of_Leaves (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Number_Of_Leafs_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return SB.Free_Leafs;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_FT_Nr_Of_Leaves;

   --
   --  Peek_Generated_FT_Max_Lvl_Idx
   --
   function Peek_Generated_FT_Max_Lvl_Idx (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Level_Index_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Extending_FT
            then
               return SB.Free_Max_Level;
            else
               raise Program_Error;
            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_FT_Max_Lvl_Idx;

   --
   --  Peek_Generated_Old_Key_ID
   --
   function Peek_Generated_Old_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Key_ID_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then

               return SB.Previous_Key.ID;

            else

               raise Program_Error;

            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Old_Key_ID;

   --
   --  Peek_Generated_New_Key_ID
   --
   function Peek_Generated_New_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Key_ID_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) and then
               SB.State = Rekeying
            then

               return SB.Current_Key.ID;

            else

               raise Program_Error;

            end if;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_New_Key_ID;

   --
   --  Peek_Generated_Key_Plaintext
   --
   function Peek_Generated_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Add_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Key_Plaintext;
            end if;
            raise Program_Error;

         when Add_Previous_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Prev_Key_Plaintext;
            end if;
            raise Program_Error;

         when Add_Current_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Curr_Key_Plaintext;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Key_Plaintext;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Remove_Previous_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Prev_Key_Plaintext.ID;
            end if;
            raise Program_Error;

         when Remove_Current_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Curr_Key_Plaintext.ID;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Key_ID;

   --
   --  Peek_Generated_Key_Value_Plaintext
   --
   function Peek_Generated_Key_Value_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Value_Plaintext_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when
            Encrypt_Current_Key_Pending |
            Encrypt_Previous_Key_Pending
         =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).Key_Plaintext.Value;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Key_Value_Plaintext;

   --
   --  Peek_Generated_Key_Value_Ciphertext
   --
   function Peek_Generated_Key_Value_Ciphertext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Value_Ciphertext_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Decrypt_Current_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).SB_Ciphertext.Current_Key.Value;
            end if;
            raise Program_Error;

         when Decrypt_Previous_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               return Ctrl.Jobs (Idx).SB_Ciphertext.Previous_Key.Value;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Key_Value_Ciphertext;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin
      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when Sync_Cache_Pending =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;
   end Peek_Generated_Cache_Primitive;

   --
   --  Peek_Generated_Blk_IO_Primitive
   --
   function Peek_Generated_Blk_IO_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Ctrl.Jobs'Range loop

         if Ctrl.Jobs (Idx).Operation /= Invalid then

            case Ctrl.Jobs (Idx).State is
            when
               Sync_Blk_IO_Pending |
               Write_SB_Pending |
               Read_SB_Pending |
               Read_Current_SB_Pending
            =>

               return Ctrl.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Blk_IO_Primitive;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Write_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Declare_Blk_Data :
               declare
                  Blk_Data : Block_Data_Type;
               begin

                  Block_Data_From_Superblock_Ciphertext (
                     Blk_Data, Ctrl.Jobs (Idx).SB_Ciphertext);

                  return Blk_Data;

               end Declare_Blk_Data;

            end if;
            raise Program_Error;

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
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Create_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Create_Key_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Add_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Add_Key_At_Crypto_Module_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Remove_Previous_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State :=
                  Remove_Previous_Key_At_Crypto_Module_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Remove_Current_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State :=
                  Remove_Current_Key_At_Crypto_Module_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Add_Previous_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State :=
                  Add_Previous_Key_At_Crypto_Module_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Add_Current_Key_At_Crypto_Module_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State :=
                  Add_Current_Key_At_Crypto_Module_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Encrypt_Current_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Encrypt_Current_Key_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Encrypt_Previous_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Encrypt_Previous_Key_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Decrypt_Current_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Decrypt_Current_Key_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Decrypt_Previous_Key_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Decrypt_Previous_Key_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Sync_Cache_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Sync_Cache_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Write_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Write_SB_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Read_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Read_SB_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Read_Current_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Read_Current_SB_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Sync_Blk_IO_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Sync_Blk_IO_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Secure_SB_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Secure_SB_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Rekey_VBA_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Rekey_VBA_In_VBD_In_Progress;
               return;
            end if;
            raise Program_Error;

         when VBD_Ext_Step_In_VBD_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := VBD_Ext_Step_In_VBD_In_Progress;
               return;
            end if;
            raise Program_Error;

         when FT_Ext_Step_In_FT_Pending =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := FT_Ext_Step_In_FT_In_Progress;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Prim_Complete_VBD_Ext
   --
   procedure Mark_Generated_Prim_Complete_VBD_Ext (
      Ctrl         : in out Control_Type;
      Prim         :        Primitive.Object_Type;
      Snapshots    :        Snapshots_Type;
      First_PBA    :        Physical_Block_Address_Type;
      Nr_Of_PBAs   :        Number_Of_Blocks_Type;
      Nr_Of_Leaves :        Tree_Number_Of_Leafs_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when VBD_Ext_Step_In_VBD_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := VBD_Ext_Step_In_VBD_Completed;
               Ctrl.Jobs (Idx).Snapshots := Snapshots;
               Ctrl.Jobs (Idx).PBA := First_PBA;
               Ctrl.Jobs (Idx).Nr_Of_Blks := Nr_Of_PBAs;
               Ctrl.Jobs (Idx).Nr_Of_Leaves := Nr_Of_Leaves;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_VBD_Ext;

   --
   --  Mark_Generated_Prim_Complete_Blk_Data
   --
   procedure Mark_Generated_Prim_Complete_Blk_Data (
      Ctrl     : in out Control_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Read_SB_In_Progress =>

            if not Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Ctrl.Jobs (Idx).State := Read_SB_Completed;
            Ctrl.Jobs (Idx).Generated_Prim := Prim;
            Superblock_Ciphertext_From_Block_Data (
               Ctrl.Jobs (Idx).SB_Ciphertext, Blk_Data);
            return;

         when Read_Current_SB_In_Progress =>

            if not Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Ctrl.Jobs (Idx).State := Read_Current_SB_Completed;
            Ctrl.Jobs (Idx).Generated_Prim := Prim;
            Superblock_Ciphertext_From_Block_Data (
               Ctrl.Jobs (Idx).SB_Ciphertext, Blk_Data);
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_Blk_Data;

   --
   --  Mark_Generated_Prim_Complete_FT_Ext
   --
   procedure Mark_Generated_Prim_Complete_FT_Ext (
      Ctrl            : in out Control_Type;
      Prim            :        Primitive.Object_Type;
      FT_Root         :        Type_1_Node_Type;
      FT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      FT_Nr_Of_Leaves :        Tree_Number_Of_Leafs_Type;
      First_PBA       :        Physical_Block_Address_Type;
      Nr_Of_PBAs      :        Number_Of_Blocks_Type;
      Nr_Of_Leaves    :        Tree_Number_Of_Leafs_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when FT_Ext_Step_In_FT_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := FT_Ext_Step_In_FT_Completed;
               Ctrl.Jobs (Idx).FT_Root := FT_Root;
               Ctrl.Jobs (Idx).FT_Max_Lvl_Idx := FT_Max_Lvl_Idx;
               Ctrl.Jobs (Idx).FT_Nr_Of_Leaves := FT_Nr_Of_Leaves;
               Ctrl.Jobs (Idx).PBA := First_PBA;
               Ctrl.Jobs (Idx).Nr_Of_Blks := Nr_Of_PBAs;
               Ctrl.Jobs (Idx).Nr_Of_Leaves := Nr_Of_Leaves;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_FT_Ext;

   --
   --  Mark_Generated_Prim_Complete_Key_Value_Plaintext
   --
   procedure Mark_Generated_Prim_Complete_Key_Value_Plaintext (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Create_Key_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Create_Key_Completed;
               Ctrl.Jobs (Idx).Key_Plaintext.Value := Key_Value;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;
            end if;
            raise Program_Error;

         when Decrypt_Current_Key_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Decrypt_Current_Key_Completed;
               Ctrl.Jobs (Idx).Curr_Key_Plaintext.Value := Key_Value;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;
            end if;
            raise Program_Error;

         when Decrypt_Previous_Key_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then
               Ctrl.Jobs (Idx).State := Decrypt_Previous_Key_Completed;
               Ctrl.Jobs (Idx).Prev_Key_Plaintext.Value := Key_Value;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_Key_Value_Plaintext;

   --
   --  Mark_Generated_Prim_Complete_Snapshots
   --
   procedure Mark_Generated_Prim_Complete_Snapshots (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Snapshots :        Snapshots_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Rekey_VBA_In_VBD_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Rekey_VBA_In_VBD_Completed;
               Ctrl.Jobs (Idx).Snapshots := Snapshots;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_Snapshots;

   --
   --  Mark_Generated_Prim_Complete_Key_Value_Ciphertext
   --
   procedure Mark_Generated_Prim_Complete_Key_Value_Ciphertext (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Encrypt_Current_Key_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Encrypt_Current_Key_Completed;
               Ctrl.Jobs (Idx).SB_Ciphertext.Current_Key.Value :=
                 Key_Value;

               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Encrypt_Previous_Key_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Encrypt_Previous_Key_Completed;
               Ctrl.Jobs (Idx).SB_Ciphertext.Previous_Key.Value :=
                  Key_Value;

               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete_Key_Value_Ciphertext;

   --
   --  Mark_Generated_Prim_Complete
   --
   procedure Mark_Generated_Prim_Complete (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Ctrl.Jobs (Idx).Operation /= Invalid then

         case Ctrl.Jobs (Idx).State is
         when Sync_Cache_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Sync_Cache_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Add_Key_At_Crypto_Module_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Add_Key_At_Crypto_Module_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Add_Previous_Key_At_Crypto_Module_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State :=
                  Add_Previous_Key_At_Crypto_Module_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Remove_Previous_Key_At_Crypto_Module_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State :=
                  Remove_Previous_Key_At_Crypto_Module_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Remove_Current_Key_At_Crypto_Module_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State :=
                  Remove_Current_Key_At_Crypto_Module_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Add_Current_Key_At_Crypto_Module_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State :=
                  Add_Current_Key_At_Crypto_Module_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Write_SB_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Write_SB_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Sync_Blk_IO_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Sync_Blk_IO_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when Secure_SB_In_Progress =>

            if Primitive.Equal (Prim, Ctrl.Jobs (Idx).Generated_Prim) then

               Ctrl.Jobs (Idx).State := Secure_SB_Completed;
               Ctrl.Jobs (Idx).Generated_Prim := Prim;
               return;

            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Complete;

end CBE.Superblock_Control;
