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

pragma Unreferenced (CBE.Debug);

use Interfaces;

package body CBE.FT_Resizing
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
   --  Hash_Of_T2_Node_Blk
   --
   function Hash_Of_T2_Node_Blk (T2_Blk : Type_2_Node_Block_Type)
   return Hash_Type;

   --
   --  Log_2
   --
   function  Log_2 (Value : Unsigned_32)
   return Unsigned_32;

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
   --  Hash_Of_T2_Node_Blk
   --
   function Hash_Of_T2_Node_Blk (T2_Blk : Type_2_Node_Block_Type)
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
         Block_Data_From_Type_2_Node_Block (CBE_Data, T2_Blk);
         SHA256_4K_Data_From_CBE_Data (SHA_Data, CBE_Data);
         SHA256_4K.Hash (SHA_Data, SHA_Hash);
         CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
         return CBE_Hash;
      end Declare_Hash_Data;

   end Hash_Of_T2_Node_Blk;

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
   --  Initialize_Resizing
   --
   procedure Initialize_Resizing (Rszg : out Resizing_Type)
   is
   begin
      Initialize_Each_Job :
      for Idx in Rszg.Jobs'Range loop
         Rszg.Jobs (Idx) := (
            Operation => Invalid,
            State => Job_State_Type'First,
            Submitted_Prim => Primitive.Invalid_Object,
            Generated_Prim => Primitive.Invalid_Object,
            FT_Root => Type_1_Node_Invalid,
            FT_Max_Lvl_Idx => Tree_Level_Index_Type'First,
            FT_Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            FT_Degree => Tree_Degree_Type'First,
            T1_Blks => (others => (others => Type_1_Node_Invalid)),
            T2_Blk => (others => Type_2_Node_Invalid),
            Lvl_Idx => Tree_Level_Index_Type'First,
            Alloc_Lvl_Idx => Tree_Level_Index_Type'First,
            VBA => Virtual_Block_Address_Type'First,
            Old_PBAs => (others => Physical_Block_Address_Type'First),
            Old_Generations => (others => Generation_Type'First),
            New_PBAs => (others => Physical_Block_Address_Type'First),
            PBA => Physical_Block_Address_Type'First,
            Nr_Of_PBAs => Number_Of_Blocks_Type'First,
            Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            MT_Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            Curr_Gen => Generation_Type'First,
            Last_Secured_Gen => Generation_Type'First,
            Free_Gen => Generation_Type'First,
            VBD_Max_Lvl_Idx => Tree_Level_Index_Type'First,
            VBD_Degree => Tree_Degree_Type'First,
            VBD_Snapshots => (others => Snapshot_Invalid),
            VBD_Degree_Log_2 => Tree_Degree_Log_2_Type'First,
            VBD_Highest_VBA => Virtual_Block_Address_Type'First,
            VBD_New_PBAs => (others => Physical_Block_Address_Type'First),
            VBD_T1_Node_Walk => (others => Type_1_Node_Invalid),
            Rekeying => Boolean'First,
            Previous_Key_ID => Key_ID_Type'First,
            Current_Key_ID => Key_ID_Type'First);
      end loop Initialize_Each_Job;
      Rszg.VBA := 0;
   end Initialize_Resizing;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Rszg : Resizing_Type)
   return Boolean
   is (for some Job of Rszg.Jobs => Job.Operation = Invalid);

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Rszg             : in out Resizing_Type;
      Prim             :        Primitive.Object_Type;
      Curr_Gen         :        Generation_Type;
      FT_Root          :        Type_1_Node_Type;
      FT_Max_Lvl_Idx   :        Tree_Level_Index_Type;
      FT_Nr_Of_Leaves  :        Tree_Number_Of_Leafs_Type;
      FT_Degree        :        Tree_Degree_Type;
      First_PBA        :        Physical_Block_Address_Type;
      Nr_Of_PBAs       :        Number_Of_Blocks_Type)
   is
   begin

      Find_Invalid_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation = Invalid then

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step =>

               Rszg.Jobs (Idx).Operation        := FT_Extension_Step;
               Rszg.Jobs (Idx).Submitted_Prim   := Prim;
               Rszg.Jobs (Idx).Curr_Gen         := Curr_Gen;
               Rszg.Jobs (Idx).FT_Root          := FT_Root;
               Rszg.Jobs (Idx).FT_Max_Lvl_Idx   := FT_Max_Lvl_Idx;
               Rszg.Jobs (Idx).FT_Nr_Of_Leaves  := FT_Nr_Of_Leaves;
               Rszg.Jobs (Idx).FT_Degree        := FT_Degree;
               Rszg.Jobs (Idx).PBA              := First_PBA;
               Rszg.Jobs (Idx).Nr_Of_PBAs       := Nr_Of_PBAs;
               Rszg.Jobs (Idx).State            := Submitted;
               return;

            when others =>

               raise Program_Error;

            end case;

         end if;

      end loop Find_Invalid_Job;
      raise Program_Error;

   end Submit_Primitive;

   --
   --  Submit_Primitive_Alloc_PBAs
   --
   procedure Submit_Primitive_Alloc_PBAs (
      Rszg                : in out Resizing_Type;
      Prim                :        Primitive.Object_Type;
      Curr_Gen            :        Generation_Type;
      Last_Secured_Gen    :        Generation_Type;
      Free_Gen            :        Generation_Type;
      FT_Root             :        Type_1_Node_Type;
      FT_Max_Lvl_Idx      :        Tree_Level_Index_Type;
      FT_Nr_Of_Leaves     :        Tree_Number_Of_Leafs_Type;
      FT_Degree           :        Tree_Degree_Type;
      VBD_Snapshots       :        Snapshots_Type;
      VBD_Max_Lvl_Idx     :        Tree_Level_Index_Type;
      VBD_Degree          :        Tree_Degree_Type;
      VBD_Highest_VBA     :        Virtual_Block_Address_Type;
      VBD_T1_Node_Walk    :        Type_1_Node_Walk_Type;
      Nr_Of_Required_Blks :        Number_Of_Blocks_Type;
      New_PBAs            :        Tree_Level_PBAs_Type;
      Rekeying            :        Boolean;
      Previous_Key_ID     :        Key_ID_Type;
      Current_Key_ID      :        Key_ID_Type;
      Rekeying_VBA        :        Virtual_Block_Address_Type)
   is
   begin

      Find_Invalid_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation = Invalid then

            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_VBD_Rkg_FT_Rszg_Alloc_For_Non_Rkg |
               Primitive.Tag_VBD_Rkg_FT_Rszg_Alloc_For_Rkg_Curr_Gen_Blks |
               Primitive.Tag_VBD_Rkg_FT_Rszg_Alloc_For_Rkg_Old_Gen_Blks
            =>

               Rszg.Jobs (Idx).Operation        := Allocate_PBAs;
               Rszg.Jobs (Idx).Submitted_Prim   := Prim;
               Rszg.Jobs (Idx).Curr_Gen         := Curr_Gen;
               Rszg.Jobs (Idx).Last_Secured_Gen := Last_Secured_Gen;
               Rszg.Jobs (Idx).Free_Gen         := Free_Gen;
               Rszg.Jobs (Idx).FT_Root          := FT_Root;
               Rszg.Jobs (Idx).FT_Max_Lvl_Idx   := FT_Max_Lvl_Idx;
               Rszg.Jobs (Idx).FT_Nr_Of_Leaves  := FT_Nr_Of_Leaves;
               Rszg.Jobs (Idx).FT_Degree        := FT_Degree;
               Rszg.Jobs (Idx).VBD_Max_Lvl_Idx  := VBD_Max_Lvl_Idx;
               Rszg.Jobs (Idx).VBD_Snapshots    := VBD_Snapshots;
               Rszg.Jobs (Idx).VBD_Degree       := VBD_Degree;
               Rszg.Jobs (Idx).VBD_Highest_VBA  := VBD_Highest_VBA;
               Rszg.Jobs (Idx).VBD_T1_Node_Walk := VBD_T1_Node_Walk;
               Rszg.Jobs (Idx).Nr_Of_PBAs       := Nr_Of_Required_Blks;
               Rszg.Jobs (Idx).VBD_New_PBAs     := New_PBAs;
               Rszg.Jobs (Idx).Rekeying         := Rekeying;
               Rszg.Jobs (Idx).Previous_Key_ID  := Previous_Key_ID;
               Rszg.Jobs (Idx).Current_Key_ID   := Current_Key_ID;
               Rszg.Jobs (Idx).VBA              := Rekeying_VBA;
               Rszg.Jobs (Idx).State            := Submitted;

               Debug.Print_String ("XXX");
               return;

            when others =>

               raise Program_Error;

            end case;

         end if;

      end loop Find_Invalid_Job;
      raise Program_Error;

   end Submit_Primitive_Alloc_PBAs;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Rszg : Resizing_Type)
   return Primitive.Object_Type
   is
   begin
      Find_Completed_Job :
      for Idx in Rszg.Jobs'Range loop
         if Rszg.Jobs (Idx).Operation /= Invalid and then
            Rszg.Jobs (Idx).State = Completed
         then
            return Rszg.Jobs (Idx).Submitted_Prim;
         end if;
      end loop Find_Completed_Job;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Peek_Completed_FT_Root
   --
   function Peek_Completed_FT_Root (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).FT_Root;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_FT_Root;

   --
   --  Peek_Completed_FT_Max_Lvl_Idx
   --
   function Peek_Completed_FT_Max_Lvl_Idx (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).FT_Max_Lvl_Idx;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_FT_Max_Lvl_Idx;

   --
   --  Peek_Completed_FT_Nr_Of_Leaves
   --
   function Peek_Completed_FT_Nr_Of_Leaves (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).FT_Nr_Of_Leaves;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_FT_Nr_Of_Leaves;

   --
   --  Peek_Completed_Nr_Of_Leaves
   --
   function Peek_Completed_Nr_Of_Leaves (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).Nr_Of_Leaves;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_Nr_Of_Leaves;

   --
   --  Peek_Completed_PBA
   --
   function Peek_Completed_PBA (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).PBA;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_PBA;

   --
   --  Peek_Completed_Nr_Of_PBAs
   --
   function Peek_Completed_Nr_Of_PBAs (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).Nr_Of_PBAs;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_Nr_Of_PBAs;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Rszg : in out Resizing_Type;
      Prim :        Primitive.Object_Type)
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation /= Invalid and then
            Rszg.Jobs (Idx).State = Completed and then
            Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
         then
            Rszg.Jobs (Idx).Operation := Invalid;
            return;
         end if;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Drop_Completed_Primitive;

   --
   --  T1_Child_Idx_For_VBA
   --
   function T1_Child_Idx_For_VBA (
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
   end T1_Child_Idx_For_VBA;

   --
   --  T2_Child_Idx_For_VBA
   --
   function T2_Child_Idx_For_VBA (
      VBA  : Virtual_Block_Address_Type;
      Degr : Tree_Degree_Type)
   return Type_2_Node_Block_Index_Type
   is
      Degree_Log_2 : constant Tree_Degree_Log_2_Type :=
         Tree_Degree_Log_2_Type (Log_2 (Unsigned_32 (Degr)));

      Degree_Mask : constant Tree_Degree_Mask_Type :=
         Tree_Degree_Mask_Type (
            Shift_Left (Unsigned_32 (1), Natural (Degree_Log_2)) - 1);
   begin
      return
         Type_2_Node_Block_Index_Type (
            Unsigned_64 (VBA) and Unsigned_64 (Degree_Mask));
   end T2_Child_Idx_For_VBA;

   --
   --  Alloc_PBA_From_Resizing_Contingent
   --
   procedure Alloc_PBA_From_Resizing_Contingent (
      First_PBA     : in out Physical_Block_Address_Type;
      Nr_Of_PBAs    : in out Number_Of_Blocks_Type;
      Allocated_PBA :    out Physical_Block_Address_Type)
   is
   begin

      if Nr_Of_PBAs = 0 then
         raise Program_Error;
      end if;

      Allocated_PBA := First_PBA;
      First_PBA := First_PBA + 1;
      Nr_Of_PBAs := Nr_Of_PBAs - 1;

   end Alloc_PBA_From_Resizing_Contingent;

   --
   --  Add_New_Root_Lvl_To_FT_Using_PBA_Contingent
   --
   procedure Add_New_Root_Lvl_To_FT_Using_PBA_Contingent (
      FT_Root          : in out Type_1_Node_Type;
      FT_Max_Lvl_Idx   : in out Tree_Level_Index_Type;
      FT_Nr_Of_Leaves  :        Tree_Number_Of_Leafs_Type;
      Curr_Gen         :        Generation_Type;
      T1_Blks          : in out Type_1_Node_Blocks_Type;
      New_PBAs         : in out Tree_Level_PBAs_Type;
      First_PBA        : in out Physical_Block_Address_Type;
      Nr_Of_PBAs       : in out Number_Of_Blocks_Type)
   is
   begin

      if FT_Max_Lvl_Idx = Tree_Level_Index_Type'Last then
         raise Program_Error;
      end if;

      FT_Max_Lvl_Idx := FT_Max_Lvl_Idx + 1;

      T1_Blks (FT_Max_Lvl_Idx) := (
         0      => FT_Root,
         others => Type_1_Node_Invalid);

      Alloc_PBA_From_Resizing_Contingent (
         First_PBA, Nr_Of_PBAs, New_PBAs (FT_Max_Lvl_Idx));

      FT_Root := (
         PBA => New_PBAs (FT_Max_Lvl_Idx),
         Gen => Curr_Gen,
         Hash => (others => 0));

      pragma Debug (Debug.Print_String (
         "   SET FT_ROOT PBA " &
         Debug.To_String (Debug.Uint64_Type (FT_Root.PBA)) &
         " GEN " &
         Debug.To_String (Debug.Uint64_Type (FT_Root.Gen)) &
         " LEAVES " &
         Debug.To_String (Debug.Uint64_Type (FT_Nr_Of_Leaves)) &
         " MAX_LVL " &
         Debug.To_String (Debug.Uint64_Type (FT_Max_Lvl_Idx)) &
         " " &
         Debug.To_String (FT_Root.Hash) &
         " "));

      pragma Debug (Debug.Print_String (
         "   SET LVL " &
         Debug.To_String (Debug.Uint64_Type (FT_Max_Lvl_Idx)) &
         " CHILD 0 PBA " &
         Debug.To_String (Debug.Uint64_Type (
            T1_Blks (FT_Max_Lvl_Idx) (0).PBA)) &
         " GEN " &
         Debug.To_String (Debug.Uint64_Type (
            T1_Blks (FT_Max_Lvl_Idx) (0).Gen)) &
         " " &
         Debug.To_String (
            T1_Blks (FT_Max_Lvl_Idx) (0).Hash) &
         " "));

      pragma Unreferenced (FT_Nr_Of_Leaves);

   end Add_New_Root_Lvl_To_FT_Using_PBA_Contingent;

   --
   --  Add_New_Branch_To_FT_Using_PBA_Contingent
   --
   procedure Add_New_Branch_To_FT_Using_PBA_Contingent (
      Mount_Point_Lvl_Idx   :        Tree_Level_Index_Type;
      Mount_Point_Child_Idx :        Tree_Child_Index_Type;
      FT_Degree             :        Tree_Degree_Type;
      Curr_Gen              :        Generation_Type;
      First_PBA             : in out Physical_Block_Address_Type;
      Nr_Of_PBAs            : in out Number_Of_Blocks_Type;
      T1_Blks               : in out Type_1_Node_Blocks_Type;
      T2_Blk                : in out Type_2_Node_Block_Type;
      New_PBAs              : in out Tree_Level_PBAs_Type;
      Stopped_At_Lvl_Idx    :    out Tree_Level_Index_Type;
      Nr_Of_Leaves          :    out Tree_Number_Of_Leafs_Type)
   is
   begin

      Nr_Of_Leaves := 0;
      Stopped_At_Lvl_Idx := Mount_Point_Lvl_Idx;

      if Mount_Point_Lvl_Idx > 1 then

         Reset_All_Lvls_Below_Mount_Point :
         for Lvl_Idx in 1 .. Mount_Point_Lvl_Idx - 1 loop

            if Lvl_Idx > 1 then
               T1_Blks (Lvl_Idx) := (others => Type_1_Node_Invalid);
            else
               T2_Blk := (others => Type_2_Node_Invalid);
            end if;

            pragma Debug (Debug.Print_String (
               "   RESET LVL " &
               Debug.To_String (Debug.Uint64_Type (Lvl_Idx))));

         end loop Reset_All_Lvls_Below_Mount_Point;

      end if;

      if Nr_Of_PBAs > 0 then

         Set_Child_PBAs_In_New_Branch :
         for Lvl_Idx in reverse 1 .. Mount_Point_Lvl_Idx loop

            Stopped_At_Lvl_Idx := Lvl_Idx;

            if Lvl_Idx > 1 then

               exit Set_Child_PBAs_In_New_Branch when Nr_Of_PBAs = 0;

               Declare_Child_Args :
               declare
                  Child_Idx : constant Type_1_Node_Block_Index_Type := (
                     if Lvl_Idx = Mount_Point_Lvl_Idx
                     then
                        Type_1_Node_Block_Index_Type (
                           Mount_Point_Child_Idx)
                     else 0);

                  Child_Lvl_Idx : constant Tree_Level_Index_Type :=
                     Lvl_Idx - 1;
               begin

                  Alloc_PBA_From_Resizing_Contingent (
                     First_PBA, Nr_Of_PBAs, New_PBAs (Child_Lvl_Idx));

                  T1_Blks (Lvl_Idx) (Child_Idx) := (
                     PBA => New_PBAs (Child_Lvl_Idx),
                     Gen => Curr_Gen,
                     Hash => (others => 0));

                  pragma Debug (Debug.Print_String (
                     "   SET LVL " &
                     Debug.To_String (Debug.Uint64_Type (Lvl_Idx)) &
                     " CHILD " &
                     Debug.To_String (Debug.Uint64_Type (Child_Idx)) &
                     " PBA " &
                     Debug.To_String (Debug.Uint64_Type (
                        T1_Blks (Lvl_Idx) (Child_Idx).PBA)) &
                     " GEN " &
                     Debug.To_String (Debug.Uint64_Type (
                        T1_Blks (Lvl_Idx) (Child_Idx).Gen)) &
                     " " &
                     Debug.To_String (
                        T1_Blks (Lvl_Idx) (Child_Idx).Hash) &
                     " "));

               end Declare_Child_Args;

            else

               Declare_First_Child_Idx :
               declare
                  First_Child_Idx :
                     constant Type_2_Node_Block_Index_Type := (
                        if Lvl_Idx = Mount_Point_Lvl_Idx
                        then
                           Type_2_Node_Block_Index_Type (
                              Mount_Point_Child_Idx)
                        else 0);

                  Child_PBA : Physical_Block_Address_Type;
               begin

                  for Child_Idx in First_Child_Idx ..
                         Type_1_Node_Block_Index_Type (FT_Degree - 1)
                  loop

                     exit Set_Child_PBAs_In_New_Branch when Nr_Of_PBAs = 0;

                     Alloc_PBA_From_Resizing_Contingent (
                        First_PBA, Nr_Of_PBAs, Child_PBA);

                     T2_Blk (Child_Idx) := (
                        PBA => Child_PBA,
                        Last_VBA => VBA_Invalid,
                        Alloc_Gen => Initial_Generation,
                        Free_Gen => Initial_Generation,
                        Last_Key_ID => Key_ID_Invalid,
                        Reserved => False);

                     pragma Debug (Debug.Print_String (
                        "   SET LVL " &
                        Debug.To_String (Debug.Uint64_Type (Lvl_Idx)) &
                        " CHILD " &
                        Debug.To_String (Debug.Uint64_Type (Child_Idx)) &
                        " PBA " &
                        Debug.To_String (Debug.Uint64_Type (
                           T2_Blk (Child_Idx).PBA)) &
                        " AGEN " &
                        Debug.To_String (Debug.Uint64_Type (
                           T2_Blk (Child_Idx).Alloc_Gen)) &
                        " FGEN " &
                        Debug.To_String (Debug.Uint64_Type (
                           T2_Blk (Child_Idx).Free_Gen)) &
                        " KEY " &
                        Debug.To_String (Debug.Uint64_Type (
                           T2_Blk (Child_Idx).Last_Key_ID)) &
                        " VBA " &
                        Debug.To_String (Debug.Uint64_Type (
                           T2_Blk (Child_Idx).Last_VBA)) &
                        " "));

                     Nr_Of_Leaves := Nr_Of_Leaves + 1;

                  end loop;

               end Declare_First_Child_Idx;

            end if;

         end loop Set_Child_PBAs_In_New_Branch;

      end if;

   end Add_New_Branch_To_FT_Using_PBA_Contingent;

   --
   --  Execute_FT_Ext_Step_Read_Inner_Node_Completed
   --
   procedure Execute_FT_Ext_Step_Read_Inner_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress :    out Boolean)
   is
   begin

      if not Primitive.Success (Job.Generated_Prim) then
         raise Program_Error;
      end if;

      if Job.Lvl_Idx > 1 then

         if Job.Lvl_Idx = Job.FT_Max_Lvl_Idx then

            if Hash_Of_T1_Node_Blk (Job.T1_Blks (Job.Lvl_Idx)) /=
               Job.FT_Root.Hash
            then
               raise Program_Error;
            end if;

         else

            Declare_Child_Idx_1 :
            declare
               Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
                  Job.Lvl_Idx + 1;

               Child_Idx : constant Type_1_Node_Block_Index_Type :=
                  T1_Child_Idx_For_VBA (
                     Job.VBA, Parent_Lvl_Idx, Job.FT_Degree);
            begin

               if Hash_Of_T1_Node_Blk (Job.T1_Blks (Job.Lvl_Idx)) /=
                  Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash
               then
                  raise Program_Error;
               end if;

            end Declare_Child_Idx_1;

         end if;

         Declare_Child_1 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.Lvl_Idx;

            Child_Lvl_Idx : constant Tree_Level_Index_Type :=
               Job.Lvl_Idx - 1;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (Job.VBA, Parent_Lvl_Idx, Job.FT_Degree);

            Child : constant Type_1_Node_Type :=
               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx);
         begin

            if Type_1_Node_Valid (Child) then

               Job.Lvl_Idx := Child_Lvl_Idx;
               Job.Old_PBAs (Child_Lvl_Idx) := Child.PBA;
               Job.Old_Generations (Child_Lvl_Idx) := Child.Gen;

               Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Read,
                  Succ   => False,
                  Tg     => Primitive.Tag_FT_Rszg_Cache,
                  Blk_Nr => Block_Number_Type (Child.PBA),
                  Idx    => Primitive.Index_Type (Job_Idx));

               Job.State := Read_Inner_Node_Pending;
               Progress := True;

               pragma Debug (Debug.Print_String (
                  "   READ LVL " &
                  Debug.To_String (Debug.Uint64_Type (Child_Lvl_Idx)) &
                  " PARENT LVL " &
                  Debug.To_String (Debug.Uint64_Type (Parent_Lvl_Idx)) &
                  " CHILD " &
                  Debug.To_String (Debug.Uint64_Type (Child_Idx)) &
                  " PBA " &
                  Debug.To_String (Debug.Uint64_Type (Child.PBA)) &
                  " GEN " &
                  Debug.To_String (Debug.Uint64_Type (Child.Gen)) &
                  " " &
                  Debug.To_String (Child.Hash) &
                  " "));

            else

               Add_New_Branch_To_FT_Using_PBA_Contingent (
                  Mount_Point_Lvl_Idx   => Parent_Lvl_Idx,
                  Mount_Point_Child_Idx => Tree_Child_Index_Type (Child_Idx),
                  FT_Degree             => Job.FT_Degree,
                  Curr_Gen              => Job.Curr_Gen,
                  First_PBA             => Job.PBA,
                  Nr_Of_PBAs            => Job.Nr_Of_PBAs,
                  T1_Blks               => Job.T1_Blks,
                  T2_Blk                => Job.T2_Blk,
                  New_PBAs              => Job.New_PBAs,
                  Stopped_At_Lvl_Idx    => Job.Lvl_Idx,
                  Nr_Of_Leaves          => Job.Nr_Of_Leaves);

               Job.Alloc_Lvl_Idx := Parent_Lvl_Idx;

               if Job.Old_Generations (Job.Alloc_Lvl_Idx) = Job.Curr_Gen then

                  Job.New_PBAs (Job.Alloc_Lvl_Idx) :=
                     Job.Old_PBAs (Job.Alloc_Lvl_Idx);

                  Job.State := Alloc_PBA_Completed;
                  Progress := True;

               else

                  Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                     Op     => Primitive_Operation_Type'First,
                     Succ   => False,
                     Tg     => Primitive.Tag_FT_Rszg_MT_Alloc,
                     Blk_Nr => Block_Number_Type'First,
                     Idx    => Primitive.Index_Type (Job_Idx));

                  Job.State := Alloc_PBA_Pending;
                  Progress := True;

               end if;

            end if;

         end Declare_Child_1;

      else

         Declare_Child_Idx_4 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.Lvl_Idx + 1;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.FT_Degree);
         begin

            if Hash_Of_T2_Node_Blk (Job.T2_Blk) /=
               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash
            then
               raise Program_Error;
            end if;

         end Declare_Child_Idx_4;

         Declare_Child_3 :
         declare
            Parent_Lvl_Idx : constant Tree_Level_Index_Type := Job.Lvl_Idx;
            Child_Idx : constant Type_2_Node_Block_Index_Type :=
               T2_Child_Idx_For_VBA (Job.VBA, Job.FT_Degree);

            Child : constant Type_2_Node_Type := Job.T2_Blk (Child_Idx);
         begin

            if Type_2_Node_Valid (Child) then
               raise Program_Error;
            end if;

            Add_New_Branch_To_FT_Using_PBA_Contingent (
               Mount_Point_Lvl_Idx   => Parent_Lvl_Idx,
               Mount_Point_Child_Idx => Tree_Child_Index_Type (Child_Idx),
               FT_Degree             => Job.FT_Degree,
               Curr_Gen              => Job.Curr_Gen,
               First_PBA             => Job.PBA,
               Nr_Of_PBAs            => Job.Nr_Of_PBAs,
               T1_Blks               => Job.T1_Blks,
               T2_Blk                => Job.T2_Blk,
               New_PBAs              => Job.New_PBAs,
               Stopped_At_Lvl_Idx    => Job.Lvl_Idx,
               Nr_Of_Leaves          => Job.Nr_Of_Leaves);

            Job.Alloc_Lvl_Idx := Parent_Lvl_Idx;

            pragma Debug (Debug.Print_String (
               "   ALLOC LVL " &
               Debug.To_String (Debug.Uint64_Type (Job.Alloc_Lvl_Idx))));

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive_Operation_Type'First,
               Succ   => False,
               Tg     => Primitive.Tag_FT_Rszg_MT_Alloc,
               Blk_Nr => Block_Number_Type'First,
               Idx    => Primitive.Index_Type (Job_Idx));

            Job.State := Alloc_PBA_Pending;
            Progress := True;

         end Declare_Child_3;

      end if;

   end Execute_FT_Ext_Step_Read_Inner_Node_Completed;

   --
   --  Tree_Max_Max_VBA
   --
   function Tree_Max_Max_VBA (
      Degree      : Tree_Degree_Type;
      Max_Lvl_Idx : Tree_Level_Index_Type)
   return Virtual_Block_Address_Type
   is (
      (Virtual_Block_Address_Type (Degree) ** Natural (Max_Lvl_Idx)) - 1);

   --
   --  Set_Args_For_Write_Back_Of_Inner_Lvl
   --
   procedure Set_Args_For_Write_Back_Of_Inner_Lvl (
      Max_Lvl_Idx :     Tree_Level_Index_Type;
      Lvl_Idx     :     Tree_Level_Index_Type;
      PBA         :     Physical_Block_Address_Type;
      Prim_Idx    :     Primitive.Index_Type;
      Job_State   : out Job_State_Type;
      Progress    : out Boolean;
      Prim        : out Primitive.Object_Type)
   is
   begin

      if Lvl_Idx = 0 then
         raise Program_Error;
      end if;

      if Lvl_Idx > Max_Lvl_Idx then
         raise Program_Error;
      end if;

      Prim := Primitive.Valid_Object_No_Pool_Idx (
         Op     => Write,
         Succ   => False,
         Tg     => Primitive.Tag_FT_Rszg_Cache,
         Blk_Nr => Block_Number_Type (PBA),
         Idx    => Prim_Idx);

      pragma Debug (Debug.Print_String (
         "   WRITE LVL " &
         Debug.To_String (Debug.Uint64_Type (Lvl_Idx)) &
         " PBA " &
         Debug.To_String (Debug.Uint64_Type (PBA)) &
         " "));

      if Lvl_Idx < Max_Lvl_Idx then

         Job_State := Write_Inner_Node_Pending;
         Progress := True;

      else

         Job_State := Write_Root_Node_Pending;
         Progress := True;

      end if;

   end Set_Args_For_Write_Back_Of_Inner_Lvl;

   --
   --  Initialize_Args_Of_Operation_Allocate_PBAs
   --
   procedure Initialize_Args_Of_Operation_Allocate_PBAs (
      FT_Max_Lvl_Idx   :     Tree_Level_Index_Type;
      VBD_Degree       :     Tree_Degree_Type;
      VBD_Degree_Log_2 : out Tree_Degree_Log_2_Type;
      Old_PBAs         : out Tree_Level_PBAs_Type;
      Old_Generations  : out Tree_Level_Generations_Type;
      New_PBAs         : out Tree_Level_PBAs_Type;
      Lvl_Idx          : out Tree_Level_Index_Type)
   is
   begin

      Old_PBAs := (others => 0);
      Old_Generations := (others => 0);
      New_PBAs := (others => 0);
      Lvl_Idx := FT_Max_Lvl_Idx;
      VBD_Degree_Log_2 :=
         Tree_Degree_Log_2_Type (Log_2 (Unsigned_32 (VBD_Degree)));

   end Initialize_Args_Of_Operation_Allocate_PBAs;

   --
   --  Set_Args_In_Order_To_Read_Inner_Node
   --
   procedure Set_Args_In_Order_To_Read_Inner_Node (
      FT_Root         :        Type_1_Node_Type;
      FT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      FT_Degree       :        Tree_Degree_Type;
      T1_Blks         :        Type_1_Node_Blocks_Type;
      Lvl_Idx         :        Tree_Level_Index_Type;
      VBA             :        Virtual_Block_Address_Type;
      Job_Idx         :        Jobs_Index_Type;
      Old_PBAs        : in out Tree_Level_PBAs_Type;
      Old_Generations : in out Tree_Level_Generations_Type;
      State           :    out Job_State_Type;
      Generated_Prim  :    out Primitive.Object_Type;
      Progress        :    out Boolean)
   is
   begin

      if Lvl_Idx = FT_Max_Lvl_Idx then

         Old_PBAs (Lvl_Idx) := FT_Root.PBA;
         Old_Generations (Lvl_Idx) := FT_Root.Gen;

         Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_FT_Rszg_Cache,
            Blk_Nr => Block_Number_Type (FT_Root.PBA),
            Idx    => Primitive.Index_Type (Job_Idx));

      else

         Declare_Child :
         declare
            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (VBA, Lvl_Idx + 1, FT_Degree);

            Child : constant Type_1_Node_Type :=
               T1_Blks (Lvl_Idx + 1) (Child_Idx);
         begin

            Old_PBAs (Lvl_Idx) := Child.PBA;
            Old_Generations (Lvl_Idx) := Child.Gen;

            Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_FT_Rszg_Cache,
               Blk_Nr => Block_Number_Type (Child.PBA),
               Idx    => Primitive.Index_Type (Job_Idx));

         end Declare_Child;

      end if;

      State := Read_Inner_Node_Pending;
      Progress := True;

   end Set_Args_In_Order_To_Read_Inner_Node;

   --
   --  Type_2_Node_Is_Free
   --
   function Type_2_Node_Is_Free (
      T2_Node          : Type_2_Node_Type;
      Snapshots        : Snapshots_Type;
      Last_Secured_Gen : Generation_Type;
      Rekeying         : Boolean;
      Rekeying_VBA     : Virtual_Block_Address_Type;
      Previous_Key_ID  : Key_ID_Type)
   return Boolean
   is
   begin
      if T2_Node.PBA = 0 or else
         T2_Node.PBA = PBA_Invalid or else
         T2_Node.Free_Gen > Last_Secured_Gen
      then
         return False;
      end if;

      if not T2_Node.Reserved then
         return True;
      end if;

      if Rekeying and then
         T2_Node.Last_Key_ID = Previous_Key_ID and then
         T2_Node.Last_VBA < Rekeying_VBA
      then
         return True;
      end if;

      For_Each_Snapshot :
      for Snap of Snapshots loop

         if Snap.Valid and then
            T2_Node.Free_Gen > Snap.Gen and then
            T2_Node.Alloc_Gen < Snap.Gen + 1
         then
            return False;
         end if;

      end loop For_Each_Snapshot;

      return True;

   end Type_2_Node_Is_Free;

   --
   --  VBD_Node_Lowest_VBA
   --
   function VBD_Node_Lowest_VBA (
      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      VBD_Level        : Tree_Level_Index_Type;
      VBD_Leaf_VBA     : Virtual_Block_Address_Type)
   return Virtual_Block_Address_Type
   is (
      Virtual_Block_Address_Type (
         Unsigned_64 (VBD_Leaf_VBA) and
         Shift_Left (
            Unsigned_64'Last,
            Natural (
               Unsigned_32 (VBD_Degree_Log_2) *
               Unsigned_32 (VBD_Level)))));

   --
   --  VBD_Node_Highest_VBA
   --
   function VBD_Node_Highest_VBA (
      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      VBD_Level        : Tree_Level_Index_Type;
      VBD_Leaf_VBA     : Virtual_Block_Address_Type)
   return Virtual_Block_Address_Type
   is (
      VBD_Node_Lowest_VBA (VBD_Degree_Log_2, VBD_Level, VBD_Leaf_VBA) +
         VBD_Node_Nr_Of_VBAs (VBD_Degree_Log_2, VBD_Level) - 1);

   --
   --  VBD_Node_Nr_Of_VBAs
   --
   function VBD_Node_Nr_Of_VBAs (
      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      VBD_Level        : Tree_Level_Index_Type)
   return Virtual_Block_Address_Type
   is (
      Virtual_Block_Address_Type (
         Shift_Left (
            Unsigned_64 (1),
            Natural (VBD_Level) * Natural (VBD_Degree_Log_2))));

   --
   --  Allocate_Free_Type_2_Node
   --
   procedure Allocate_Free_Type_2_Node (
      Free_Gen           :     Generation_Type;
      Submitted_Prim_Tag :     Primitive.Tag_Type;
      VBD_T1_Node        :     Type_1_Node_Type;
      VBD_Degree_Log_2   :     Tree_Degree_Log_2_Type;
      VBD_Highest_VBA    :     Virtual_Block_Address_Type;
      VBD_Lvl            :     Tree_Level_Index_Type;
      VBA                :     Virtual_Block_Address_Type;
      Rekeying           :     Boolean;
      Rekeying_VBA       :     Virtual_Block_Address_Type;
      Previous_Key_ID    :     Key_ID_Type;
      Current_Key_ID     :     Key_ID_Type;
      New_PBA            :     Physical_Block_Address_Type;
      T2_Node            : out Type_2_Node_Type)
   is
   begin
      T2_Node.Alloc_Gen := VBD_T1_Node.Gen;
      T2_Node.Free_Gen := Free_Gen;

      case Submitted_Prim_Tag is
      when
         Primitive.Tag_Pool_VBD |
         Primitive.Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg
      =>
         T2_Node.PBA := VBD_T1_Node.PBA;
         T2_Node.Reserved := True;
         T2_Node.Last_VBA :=
            VBD_Node_Lowest_VBA (VBD_Degree_Log_2, VBD_Lvl, VBA);

         if Rekeying then
            if VBA < Rekeying_VBA then
               T2_Node.Last_Key_ID := Current_Key_ID;
            else
               T2_Node.Last_Key_ID := Previous_Key_ID;
            end if;
         else
            T2_Node.Last_Key_ID := Current_Key_ID;
         end if;

      when Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks =>

         T2_Node.PBA := VBD_T1_Node.PBA;
         T2_Node.Reserved := False;
         T2_Node.Last_Key_ID := Previous_Key_ID;
         T2_Node.Last_VBA :=
            VBD_Node_Lowest_VBA (VBD_Degree_Log_2, VBD_Lvl, VBA);

      when Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks =>

         T2_Node.PBA := New_PBA;
         T2_Node.Reserved := True;

         Declare_VBD_Node_Highest_VBA :
         declare
            VBD_Node_High_VBA : constant Virtual_Block_Address_Type :=
               VBD_Node_Highest_VBA (VBD_Degree_Log_2, VBD_Lvl, VBA);
         begin

            if Rekeying_VBA < VBD_Node_High_VBA and then
               Rekeying_VBA < VBD_Highest_VBA
            then
               T2_Node.Last_Key_ID := Previous_Key_ID;
               T2_Node.Last_VBA := Rekeying_VBA + 1;
            elsif
               Rekeying_VBA = VBD_Node_High_VBA or else
               Rekeying_VBA = VBD_Highest_VBA
            then
               T2_Node.Last_Key_ID := Current_Key_ID;
               T2_Node.Last_VBA :=
                  VBD_Node_Lowest_VBA (VBD_Degree_Log_2, VBD_Lvl, VBA);
            else
               raise Program_Error;
            end if;

         end Declare_VBD_Node_Highest_VBA;

      when others =>

         raise Program_Error;

      end case;

   end Allocate_Free_Type_2_Node;

   --
   --  New_PBA_Required_For_VBD_Node
   --
   function New_PBA_Required_For_VBD_Node (
      New_PBA : Physical_Block_Address_Type)
   return Boolean
   is (New_PBA = 0);

   --
   --  Allocate_Free_PBAs_From_Type_2_Node_Block
   --
   procedure Allocate_Free_PBAs_From_Type_2_Node_Block (
      T2_Node_Blk         : in out Type_2_Node_Block_Type;
      Nr_Of_Required_PBAs : in out Number_Of_Blocks_Type;
      New_PBAs            : in out Tree_Level_PBAs_Type;
      Free_Gen            :        Generation_Type;
      Submitted_Prim_Tag  :        Primitive.Tag_Type;
      Snapshots           :        Snapshots_Type;
      Last_Secured_Gen    :        Generation_Type;
      VBD_Degree_Log_2    :        Tree_Degree_Log_2_Type;
      VBD_Highest_VBA     :        Virtual_Block_Address_Type;
      VBD_Max_Lvl_Idx     :        Tree_Level_Index_Type;
      VBD_T1_Node_Walk    :        Type_1_Node_Walk_Type;
      VBA                 :        Virtual_Block_Address_Type;
      Rekeying            :        Boolean;
      Rekeying_VBA        :        Virtual_Block_Address_Type;
      Previous_Key_ID     :        Key_ID_Type;
      Current_Key_ID      :        Key_ID_Type)
   is
   begin

      For_Each_T2_Node :
      for T2_Node_Idx in T2_Node_Blk'Range loop

         if Type_2_Node_Is_Free (
               T2_Node          => T2_Node_Blk (T2_Node_Idx),
               Snapshots        => Snapshots,
               Last_Secured_Gen => Last_Secured_Gen,
               Rekeying         => Rekeying,
               Rekeying_VBA     => Rekeying_VBA,
               Previous_Key_ID  => Previous_Key_ID)
         then

            For_Each_VBD_Lvl :
            for VBD_Lvl in 0 .. VBD_Max_Lvl_Idx loop

               if New_PBA_Required_For_VBD_Node (New_PBAs (VBD_Lvl)) then

                  New_PBAs (VBD_Lvl) := T2_Node_Blk (T2_Node_Idx).PBA;
                  Allocate_Free_Type_2_Node (
                     Free_Gen           => Free_Gen,
                     Submitted_Prim_Tag => Submitted_Prim_Tag,
                     VBD_T1_Node        => VBD_T1_Node_Walk (VBD_Lvl),
                     VBD_Degree_Log_2   => VBD_Degree_Log_2,
                     VBD_Highest_VBA    => VBD_Highest_VBA,
                     VBD_Lvl            => VBD_Lvl,
                     VBA                => VBA,
                     Rekeying           => Rekeying,
                     Rekeying_VBA       => Rekeying_VBA,
                     Previous_Key_ID    => Previous_Key_ID,
                     Current_Key_ID     => Current_Key_ID,
                     T2_Node            => T2_Node_Blk (T2_Node_Idx),
                     New_PBA            => New_PBAs (VBD_Lvl));

                  Nr_Of_Required_PBAs := Nr_Of_Required_PBAs - 1;
                  exit For_Each_VBD_Lvl when Nr_Of_Required_PBAs = 0;

               end if;

            end loop For_Each_VBD_Lvl;

         end if;

      end loop For_Each_T2_Node;

   end Allocate_Free_PBAs_From_Type_2_Node_Block;

   --
   --  Execute_Allocate_PBAs
   --
   procedure Execute_Allocate_PBAs (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      VBA      :        Virtual_Block_Address_Type;
      Progress : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Initialize_Args_Of_Operation_Allocate_PBAs (
            FT_Max_Lvl_Idx   => Job.FT_Max_Lvl_Idx,
            Old_PBAs         => Job.Old_PBAs,
            Old_Generations  => Job.Old_Generations,
            New_PBAs         => Job.New_PBAs,
            Lvl_Idx          => Job.Lvl_Idx,
            VBD_Degree_Log_2 => Job.VBD_Degree_Log_2,
            VBD_Degree       => Job.VBD_Degree);

         Set_Args_In_Order_To_Read_Inner_Node (
            FT_Root         => Job.FT_Root,
            FT_Max_Lvl_Idx  => Job.FT_Max_Lvl_Idx,
            FT_Degree       => Job.FT_Degree,
            T1_Blks         => Job.T1_Blks,
            Lvl_Idx         => Job.Lvl_Idx,
            VBA             => VBA,
            Job_Idx         => Job_Idx,
            Old_PBAs        => Job.Old_PBAs,
            Old_Generations => Job.Old_Generations,
            State           => Job.State,
            Generated_Prim  => Job.Generated_Prim,
            Progress        => Progress);

      when Read_Inner_Node_Completed =>

         Check_That_Primitive_Was_Successful (Job.Generated_Prim);
         Check_Hash_Of_Read_Node (
            FT_Root        => Job.FT_Root,
            FT_Max_Lvl_Idx => Job.FT_Max_Lvl_Idx,
            FT_Degree      => Job.FT_Degree,
            T1_Blks        => Job.T1_Blks,
            T2_Blk         => Job.T2_Blk,
            Lvl_Idx        => Job.Lvl_Idx,
            VBA            => VBA);

         if Job.Lvl_Idx > 1 then

            Job.Lvl_Idx := Job.Lvl_Idx - 1;
            Set_Args_In_Order_To_Read_Inner_Node (
               FT_Root         => Job.FT_Root,
               FT_Max_Lvl_Idx  => Job.FT_Max_Lvl_Idx,
               FT_Degree       => Job.FT_Degree,
               T1_Blks         => Job.T1_Blks,
               Lvl_Idx         => Job.Lvl_Idx,
               VBA             => VBA,
               Job_Idx         => Job_Idx,
               Old_PBAs        => Job.Old_PBAs,
               Old_Generations => Job.Old_Generations,
               State           => Job.State,
               Generated_Prim  => Job.Generated_Prim,
               Progress        => Progress);

         else

            Allocate_Free_PBAs_From_Type_2_Node_Block (
               T2_Node_Blk         => Job.T2_Blk,
               Nr_Of_Required_PBAs => Job.Nr_Of_PBAs,
               Snapshots           => Job.VBD_Snapshots,
               Free_Gen            => Job.Free_Gen,
               Submitted_Prim_Tag  => Primitive.Tag (Job.Submitted_Prim),
               Last_Secured_Gen    => Job.Last_Secured_Gen,
               VBD_Degree_Log_2    => Job.VBD_Degree_Log_2,
               VBD_Highest_VBA     => Job.VBD_Highest_VBA,
               VBD_Max_Lvl_Idx     => Job.VBD_Max_Lvl_Idx,
               VBD_T1_Node_Walk    => Job.VBD_T1_Node_Walk,
               New_PBAs            => Job.New_PBAs,
               VBA                 => VBA,
               Rekeying            => Job.Rekeying,
               Rekeying_VBA        => Job.VBA,
               Previous_Key_ID     => Job.Previous_Key_ID,
               Current_Key_ID      => Job.Current_Key_ID);

         end if;

      when others =>

         null;

      end case;

   end Execute_Allocate_PBAs;

   --
   --  Check_Hash_Of_Read_Node
   --
   procedure Check_Hash_Of_Read_Node (
      FT_Root        : Type_1_Node_Type;
      FT_Max_Lvl_Idx : Tree_Level_Index_Type;
      FT_Degree      : Tree_Degree_Type;
      T1_Blks        : Type_1_Node_Blocks_Type;
      T2_Blk         : Type_2_Node_Block_Type;
      Lvl_Idx        : Tree_Level_Index_Type;
      VBA            : Virtual_Block_Address_Type)
   is
   begin

      if Lvl_Idx = FT_Max_Lvl_Idx then

         if Hash_Of_T1_Node_Blk (T1_Blks (Lvl_Idx)) /= FT_Root.Hash then
            raise Program_Error;
         end if;

      elsif Lvl_Idx > 1 then

         Declare_T1_Child_Idx :
         declare
            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (VBA, Lvl_Idx + 1, FT_Degree);
         begin

            if Hash_Of_T1_Node_Blk (T1_Blks (Lvl_Idx)) /=
                  T1_Blks (Lvl_Idx + 1) (Child_Idx).Hash
            then
               raise Program_Error;
            end if;

         end Declare_T1_Child_Idx;

      elsif Lvl_Idx = 1 then

         Declare_T2_Child_Idx :
         declare
            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (VBA, Lvl_Idx + 1, FT_Degree);
         begin

            if Hash_Of_T2_Node_Blk (T2_Blk) /=
                  T1_Blks (Lvl_Idx + 1) (Child_Idx).Hash
            then
               raise Program_Error;
            end if;

         end Declare_T2_Child_Idx;

      else

         raise Program_Error;

      end if;

   end Check_Hash_Of_Read_Node;

   --
   --  Check_That_Primitive_Was_Successful
   --
   procedure Check_That_Primitive_Was_Successful (
      Prim : Primitive.Object_Type)
   is
   begin

      if not Primitive.Success (Prim) then
         raise Program_Error;
      end if;

   end Check_That_Primitive_Was_Successful;

   --
   --  Execute_FT_Extension_Step
   --
   procedure Execute_FT_Extension_Step (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Job.State is
      when Submitted =>

         Job.Nr_Of_Leaves := 0;
         Job.VBA :=
            Virtual_Block_Address_Type (
               Job.FT_Nr_Of_Leaves);

         Job.Old_PBAs := (others => 0);
         Job.Old_Generations := (others => 0);
         Job.New_PBAs := (others => 0);

         Job.Lvl_Idx := Job.FT_Max_Lvl_Idx;
         Job.Old_PBAs (Job.Lvl_Idx) := Job.FT_Root.PBA;
         Job.Old_Generations (Job.Lvl_Idx) := Job.FT_Root.Gen;

         if Job.VBA <= Tree_Max_Max_VBA (Job.FT_Degree, Job.FT_Max_Lvl_Idx)
         then

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_FT_Rszg_Cache,
               Blk_Nr => Block_Number_Type (Job.FT_Root.PBA),
               Idx    => Primitive.Index_Type (Job_Idx));

            pragma Debug (Debug.Print_String (
               "   READ LVL " &
               Debug.To_String (Debug.Uint64_Type (Job.Lvl_Idx)) &
               " PARENT FT_ROOT PBA " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.FT_Root.PBA)) &
               " GEN " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.FT_Root.Gen)) &
               " LEAVES " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.FT_Nr_Of_Leaves)) &
               " MAX_LVL " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.FT_Max_Lvl_Idx)) &
               " " &
               Debug.To_String (Job.FT_Root.Hash) &
               " "));

            Job.State := Read_Root_Node_Pending;
            Progress := True;

         else

            Add_New_Root_Lvl_To_FT_Using_PBA_Contingent (
               FT_Root          => Job.FT_Root,
               FT_Max_Lvl_Idx   => Job.FT_Max_Lvl_Idx,
               FT_Nr_Of_Leaves  => Job.FT_Nr_Of_Leaves,
               Curr_Gen         => Job.Curr_Gen,
               T1_Blks          => Job.T1_Blks,
               New_PBAs         => Job.New_PBAs,
               First_PBA        => Job.PBA,
               Nr_Of_PBAs       => Job.Nr_Of_PBAs);

            Add_New_Branch_To_FT_Using_PBA_Contingent (
               Mount_Point_Lvl_Idx   => Job.FT_Max_Lvl_Idx,
               Mount_Point_Child_Idx => 1,
               FT_Degree             => Job.FT_Degree,
               Curr_Gen              => Job.Curr_Gen,
               First_PBA             => Job.PBA,
               Nr_Of_PBAs            => Job.Nr_Of_PBAs,
               T1_Blks               => Job.T1_Blks,
               T2_Blk                => Job.T2_Blk,
               New_PBAs              => Job.New_PBAs,
               Stopped_At_Lvl_Idx    => Job.Lvl_Idx,
               Nr_Of_Leaves          => Job.Nr_Of_Leaves);

            pragma Debug (Debug.Print_String (
               "   PBAS ALLOCATED CURR_GEN " &
               Debug.To_String (Debug.Uint64_Type (Job.Curr_Gen)) &
               " "));

            Set_Args_For_Write_Back_Of_Inner_Lvl (
               Max_Lvl_Idx => Job.FT_Max_Lvl_Idx,
               Lvl_Idx     => Job.Lvl_Idx,
               PBA         => Job.New_PBAs (Job.Lvl_Idx),
               Prim_Idx    => Primitive.Index_Type (Job_Idx),
               Job_State   => Job.State,
               Progress    => Progress,
               Prim        => Job.Generated_Prim);

         end if;

      when Read_Root_Node_Completed =>

         Execute_FT_Ext_Step_Read_Inner_Node_Completed (
            Job, Job_Idx, Progress);

      when Read_Inner_Node_Completed =>

         Execute_FT_Ext_Step_Read_Inner_Node_Completed (
            Job, Job_Idx, Progress);

      when Alloc_PBA_Completed =>

         if Job.Alloc_Lvl_Idx < Job.FT_Max_Lvl_Idx then

            Job.Alloc_Lvl_Idx := Job.Alloc_Lvl_Idx + 1;

            if Job.Old_Generations (Job.Alloc_Lvl_Idx) = Job.Curr_Gen then

               Job.New_PBAs (Job.Alloc_Lvl_Idx) :=
                  Job.Old_PBAs (Job.Alloc_Lvl_Idx);

               Job.State := Alloc_PBA_Completed;
               Progress := True;

            else

               Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Primitive_Operation_Type'First,
                  Succ   => False,
                  Tg     => Primitive.Tag_FT_Rszg_MT_Alloc,
                  Blk_Nr => Block_Number_Type'First,
                  Idx    => Primitive.Index_Type (Job_Idx));

               Job.State := Alloc_PBA_Pending;
               Progress := True;

            end if;

         else

            pragma Debug (Debug.Print_String (
               "   PBAS ALLOCATED CURR_GEN " &
               Debug.To_String (Debug.Uint64_Type (Job.Curr_Gen)) &
               " "));

            Set_Args_For_Write_Back_Of_Inner_Lvl (
               Max_Lvl_Idx => Job.FT_Max_Lvl_Idx,
               Lvl_Idx     => Job.Lvl_Idx,
               PBA         => Job.New_PBAs (Job.Lvl_Idx),
               Prim_Idx    => Primitive.Index_Type (Job_Idx),
               Job_State   => Job.State,
               Progress    => Progress,
               Prim        => Job.Generated_Prim);

         end if;

      when Write_Inner_Node_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         if Job.Lvl_Idx > 1 then

            Declare_Child_Idx_14 :
            declare
               Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
                  Job.Lvl_Idx + 1;

               Child_Lvl_Idx : constant Tree_Level_Index_Type :=
                  Job.Lvl_Idx;

               Child_Idx : constant Type_1_Node_Block_Index_Type :=
                  T1_Child_Idx_For_VBA (
                     Job.VBA, Parent_Lvl_Idx, Job.FT_Degree);
            begin

               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx) := (
                  PBA => Job.New_PBAs (Child_Lvl_Idx),
                  Gen => Job.Curr_Gen,
                  Hash => Hash_Of_T1_Node_Blk (Job.T1_Blks (Child_Lvl_Idx)));

               pragma Debug (Debug.Print_String (
                  "   SET LVL " &
                  Debug.To_String (Debug.Uint64_Type (Parent_Lvl_Idx)) &
                  " CHILD " &
                  Debug.To_String (Debug.Uint64_Type (Child_Idx)) &
                  " PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).PBA)) &
                  " GEN " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Gen)) &
                  " " &
                  Debug.To_String (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash) &
                  " "));

               Set_Args_For_Write_Back_Of_Inner_Lvl (
                  Max_Lvl_Idx => Job.FT_Max_Lvl_Idx,
                  Lvl_Idx     => Parent_Lvl_Idx,
                  PBA         => Job.New_PBAs (Parent_Lvl_Idx),
                  Prim_Idx    => Primitive.Index_Type (Job_Idx),
                  Job_State   => Job.State,
                  Progress    => Progress,
                  Prim        => Job.Generated_Prim);

               Job.Lvl_Idx := Job.Lvl_Idx + 1;

            end Declare_Child_Idx_14;

         else

            Declare_Child_Idx_15 :
            declare
               Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
                  Job.Lvl_Idx + 1;

               Child_Lvl_Idx : constant Tree_Level_Index_Type :=
                  Job.Lvl_Idx;

               Child_Idx : constant Type_1_Node_Block_Index_Type :=
                  T1_Child_Idx_For_VBA (
                     Job.VBA, Parent_Lvl_Idx, Job.FT_Degree);
            begin

               Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx) := (
                  PBA => Job.New_PBAs (Child_Lvl_Idx),
                  Gen => Job.Curr_Gen,
                  Hash => Hash_Of_T2_Node_Blk (Job.T2_Blk));

               pragma Debug (Debug.Print_String (
                  "   SET LVL " &
                  Debug.To_String (Debug.Uint64_Type (
                     Parent_Lvl_Idx)) &
                  " CHILD " &
                  Debug.To_String (Debug.Uint64_Type (
                     Child_Idx)) &
                  " PBA " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).PBA)) &
                  " GEN " &
                  Debug.To_String (Debug.Uint64_Type (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Gen)) &
                  " " &
                  Debug.To_String (
                     Job.T1_Blks (Parent_Lvl_Idx) (Child_Idx).Hash) &
                  " "));

               Set_Args_For_Write_Back_Of_Inner_Lvl (
                  Max_Lvl_Idx => Job.FT_Max_Lvl_Idx,
                  Lvl_Idx     => Parent_Lvl_Idx,
                  PBA         => Job.New_PBAs (Parent_Lvl_Idx),
                  Prim_Idx    => Primitive.Index_Type (Job_Idx),
                  Job_State   => Job.State,
                  Progress    => Progress,
                  Prim        => Job.Generated_Prim);

               Job.Lvl_Idx := Job.Lvl_Idx + 1;

            end Declare_Child_Idx_15;

         end if;

      when Write_Root_Node_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         --
         --  Update parent
         --
         Declare_Child_Idx_7 :
         declare
            Child_Lvl_Idx : constant Tree_Level_Index_Type := Job.Lvl_Idx;
            Child_PBA : constant Physical_Block_Address_Type :=
               Job.New_PBAs (Child_Lvl_Idx);
         begin

            Job.FT_Root := (
               PBA => Child_PBA,
               Gen => Job.Curr_Gen,
               Hash => Hash_Of_T1_Node_Blk (Job.T1_Blks (Child_Lvl_Idx)));

            Job.FT_Nr_Of_Leaves := Job.FT_Nr_Of_Leaves + Job.Nr_Of_Leaves;

         end Declare_Child_Idx_7;

         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_FT_Extension_Step;

   --
   --  Execute
   --
   procedure Execute (
      Rszg     : in out Resizing_Type;
      Progress : in out Boolean)
   is
   begin

      Execute_Each_Valid_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when FT_Extension_Step =>

            Execute_FT_Extension_Step (Rszg.Jobs (Idx), Idx, Progress);

         when Allocate_PBAs =>

            Execute_Allocate_PBAs (Rszg.Jobs (Idx), Idx, Rszg.VBA, Progress);

         when Invalid =>

            null;

         end case;

      end loop Execute_Each_Valid_Job;

   end Execute;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Rszg : Resizing_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation /= Invalid then

            case Rszg.Jobs (Idx).State is
            when
               Read_Root_Node_Pending |
               Read_Inner_Node_Pending |
               Write_Inner_Node_Pending |
               Write_Root_Node_Pending
            =>

               return Rszg.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Cache_Primitive;

   --
   --  Peek_Generated_MT_Primitive
   --
   function Peek_Generated_MT_Primitive (Rszg : Resizing_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation /= Invalid then

            case Rszg.Jobs (Idx).State is
            when Alloc_PBA_Pending =>

               return Rszg.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_MT_Primitive;

   --
   --  Peek_Generated_MT_Rszg_Primitive
   --
   function Peek_Generated_MT_Rszg_Primitive (Rszg : Resizing_Type)
   return Primitive.Object_Type
   is
   begin

      Inspect_Each_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation /= Invalid then

            case Rszg.Jobs (Idx).State is
            when Extend_MT_By_One_Leaf_Pending =>

               return Rszg.Jobs (Idx).Generated_Prim;

            when others =>

               null;

            end case;

         end if;

      end loop Inspect_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_MT_Rszg_Primitive;

   --
   --  Peek_Generated_PBA
   --
   function Peek_Generated_PBA (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Extend_MT_By_One_Leaf_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim)
            then
               raise Program_Error;
            end if;

            return Rszg.Jobs (Idx).PBA;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_PBA;

   --
   --  Peek_Generated_Nr_Of_PBAs
   --
   function Peek_Generated_Nr_Of_PBAs (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Extend_MT_By_One_Leaf_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim)
            then
               raise Program_Error;
            end if;

            return Rszg.Jobs (Idx).Nr_Of_PBAs;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Nr_Of_PBAs;

   --
   --  Peek_Generated_Curr_Gen
   --
   function Peek_Generated_Curr_Gen (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Alloc_PBA_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim)
            then
               raise Program_Error;
            end if;

            return Rszg.Jobs (Idx).Curr_Gen;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Curr_Gen;

   --
   --  Peek_Generated_Old_PBA
   --
   function Peek_Generated_Old_PBA (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Alloc_PBA_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim)
            then
               raise Program_Error;
            end if;

            return Rszg.Jobs (Idx).Old_PBAs (Rszg.Jobs (Idx).Alloc_Lvl_Idx);

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Old_PBA;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when
            Write_Inner_Node_Pending |
            Write_Root_Node_Pending
         =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Declare_Blk_Data :
            declare
               Blk_Data : Block_Data_Type;
               Lvl_Idx : constant Tree_Level_Index_Type :=
                  Rszg.Jobs (Idx).Lvl_Idx;
            begin

               if Lvl_Idx > 1 then

                  Block_Data_From_Type_1_Node_Block (
                     Blk_Data, Rszg.Jobs (Idx).T1_Blks (Lvl_Idx));

               elsif Lvl_Idx = 1 then

                  Block_Data_From_Type_2_Node_Block (
                     Blk_Data, Rszg.Jobs (Idx).T2_Blk);

               else

                  raise Program_Error;

               end if;

               return Blk_Data;

            end Declare_Blk_Data;

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
      Rszg : in out Resizing_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Read_Root_Node_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Read_Root_Node_In_Progress;
            return;

         when Read_Inner_Node_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Read_Inner_Node_In_Progress;
            return;

         when Write_Inner_Node_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Write_Inner_Node_In_Progress;
            return;

         when Write_Root_Node_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Write_Root_Node_In_Progress;
            return;

         when Alloc_PBA_Pending =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Alloc_PBA_In_Progress;
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
      Rszg     : in out Resizing_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Read_Root_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Read_Root_Node_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            Type_1_Node_Block_From_Block_Data (
               Rszg.Jobs (Idx).T1_Blks (Rszg.Jobs (Idx).Lvl_Idx), Blk_Data);

            return;

         when Read_Inner_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Read_Inner_Node_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            Debug.Print_String ("YYY");

            if Rszg.Jobs (Idx).Lvl_Idx > 1 then

               Type_1_Node_Block_From_Block_Data (
                  Rszg.Jobs (Idx).T1_Blks (Rszg.Jobs (Idx).Lvl_Idx), Blk_Data);

            elsif Rszg.Jobs (Idx).Lvl_Idx = 1 then

               Type_2_Node_Block_From_Block_Data (
                  Rszg.Jobs (Idx).T2_Blk, Blk_Data);

            else

               raise Program_Error;

            end if;

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
      Rszg : in out Resizing_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Write_Inner_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Write_Inner_Node_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            return;

         when Write_Root_Node_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Write_Root_Node_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed;

   --
   --  Mark_Generated_Prim_Completed_New_PBA
   --
   procedure Mark_Generated_Prim_Completed_New_PBA (
      Rszg    : in out Resizing_Type;
      Prim    :        Primitive.Object_Type;
      New_PBA :        Physical_Block_Address_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Alloc_PBA_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Alloc_PBA_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            Rszg.Jobs (Idx).New_PBAs (Rszg.Jobs (Idx).Alloc_Lvl_Idx) :=
               New_PBA;

            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed_New_PBA;

   --
   --  Mark_Generated_Prim_Completed_MT_Ext
   --
   procedure Mark_Generated_Prim_Completed_MT_Ext (
      Rszg         : in out Resizing_Type;
      Prim         :        Primitive.Object_Type;
      First_PBA    :        Physical_Block_Address_Type;
      Nr_Of_PBAs   :        Number_Of_Blocks_Type;
      Nr_Of_Leaves :        Tree_Number_Of_Leafs_Type)
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin
      if Rszg.Jobs (Idx).Operation /= Invalid then

         case Rszg.Jobs (Idx).State is
         when Alloc_PBA_In_Progress =>

            if not Primitive.Equal (Prim, Rszg.Jobs (Idx).Generated_Prim) then
               raise Program_Error;
            end if;

            Rszg.Jobs (Idx).State := Alloc_PBA_Completed;
            Rszg.Jobs (Idx).Generated_Prim := Prim;
            Rszg.Jobs (Idx).PBA := First_PBA;
            Rszg.Jobs (Idx).Nr_Of_PBAs := Nr_Of_PBAs;
            Rszg.Jobs (Idx).MT_Nr_Of_Leaves := Nr_Of_Leaves;
            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed_MT_Ext;

end CBE.FT_Resizing;
