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

package body CBE.MT_Resizing
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
            MT_Root => Type_1_Node_Invalid,
            MT_Max_Lvl_Idx => Tree_Level_Index_Type'First,
            MT_Nr_Of_Leaves => Tree_Number_Of_Leafs_Type'First,
            MT_Degree => Tree_Degree_Type'First,
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
            Curr_Gen => Generation_Type'First);
      end loop Initialize_Each_Job;
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
      MT_Root          :        Type_1_Node_Type;
      MT_Max_Lvl_Idx   :        Tree_Level_Index_Type;
      MT_Nr_Of_Leaves  :        Tree_Number_Of_Leafs_Type;
      MT_Degree        :        Tree_Degree_Type;
      First_PBA        :        Physical_Block_Address_Type;
      Nr_Of_PBAs       :        Number_Of_Blocks_Type)
   is
   begin

      Find_Invalid_Job :
      for Idx in Rszg.Jobs'Range loop

         if Rszg.Jobs (Idx).Operation = Invalid then

            case Primitive.Tag (Prim) is
            when Primitive.Tag_FT_Rszg_MT_Rszg_Extend_By_One_Leaf =>

               Rszg.Jobs (Idx).Operation        := Extend_By_One_Leaf;
               Rszg.Jobs (Idx).Submitted_Prim   := Prim;
               Rszg.Jobs (Idx).Curr_Gen         := Curr_Gen;
               Rszg.Jobs (Idx).MT_Root          := MT_Root;
               Rszg.Jobs (Idx).MT_Max_Lvl_Idx   := MT_Max_Lvl_Idx;
               Rszg.Jobs (Idx).MT_Nr_Of_Leaves  := MT_Nr_Of_Leaves;
               Rszg.Jobs (Idx).MT_Degree        := MT_Degree;
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
   --  Peek_Completed_MT_Root
   --
   function Peek_Completed_MT_Root (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when Extend_By_One_Leaf =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).MT_Root;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_MT_Root;

   --
   --  Peek_Completed_MT_Max_Lvl_Idx
   --
   function Peek_Completed_MT_Max_Lvl_Idx (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when Extend_By_One_Leaf =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).MT_Max_Lvl_Idx;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_MT_Max_Lvl_Idx;

   --
   --  Peek_Completed_MT_Nr_Of_Leaves
   --
   function Peek_Completed_MT_Nr_Of_Leaves (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type
   is
   begin

      Find_Corresponding_Job :
      for Idx in Rszg.Jobs'Range loop

         case Rszg.Jobs (Idx).Operation is
         when Extend_By_One_Leaf =>

            if Rszg.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Rszg.Jobs (Idx).Submitted_Prim)
            then
               return Rszg.Jobs (Idx).MT_Nr_Of_Leaves;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_MT_Nr_Of_Leaves;

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
         when Extend_By_One_Leaf =>

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
         when Extend_By_One_Leaf =>

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
         when Extend_By_One_Leaf =>

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
   --  Add_New_Root_Lvl_To_MT_Using_PBA_Contingent
   --
   procedure Add_New_Root_Lvl_To_MT_Using_PBA_Contingent (
      MT_Root          : in out Type_1_Node_Type;
      MT_Max_Lvl_Idx   : in out Tree_Level_Index_Type;
      MT_Nr_Of_Leaves  :        Tree_Number_Of_Leafs_Type;
      Curr_Gen         :        Generation_Type;
      T1_Blks          : in out Type_1_Node_Blocks_Type;
      New_PBAs         : in out Tree_Level_PBAs_Type;
      First_PBA        : in out Physical_Block_Address_Type;
      Nr_Of_PBAs       : in out Number_Of_Blocks_Type)
   is
   begin

      if MT_Max_Lvl_Idx = Tree_Level_Index_Type'Last then
         raise Program_Error;
      end if;

      MT_Max_Lvl_Idx := MT_Max_Lvl_Idx + 1;

      T1_Blks (MT_Max_Lvl_Idx) := (
         0      => MT_Root,
         others => Type_1_Node_Invalid);

      Alloc_PBA_From_Resizing_Contingent (
         First_PBA, Nr_Of_PBAs, New_PBAs (MT_Max_Lvl_Idx));

      MT_Root := (
         PBA => New_PBAs (MT_Max_Lvl_Idx),
         Gen => Curr_Gen,
         Hash => (others => 0));

      pragma Debug (Debug.Print_String (
         "   SET MT_ROOT PBA " &
         Debug.To_String (Debug.Uint64_Type (MT_Root.PBA)) &
         " GEN " &
         Debug.To_String (Debug.Uint64_Type (MT_Root.Gen)) &
         " LEAVES " &
         Debug.To_String (Debug.Uint64_Type (MT_Nr_Of_Leaves)) &
         " MAX_LVL " &
         Debug.To_String (Debug.Uint64_Type (MT_Max_Lvl_Idx)) &
         " " &
         Debug.To_String (MT_Root.Hash) &
         " "));

      pragma Debug (Debug.Print_String (
         "   SET LVL " &
         Debug.To_String (Debug.Uint64_Type (MT_Max_Lvl_Idx)) &
         " CHILD 0 PBA " &
         Debug.To_String (Debug.Uint64_Type (
            T1_Blks (MT_Max_Lvl_Idx) (0).PBA)) &
         " GEN " &
         Debug.To_String (Debug.Uint64_Type (
            T1_Blks (MT_Max_Lvl_Idx) (0).Gen)) &
         " " &
         Debug.To_String (
            T1_Blks (MT_Max_Lvl_Idx) (0).Hash) &
         " "));

      pragma Unreferenced (MT_Nr_Of_Leaves);

   end Add_New_Root_Lvl_To_MT_Using_PBA_Contingent;

   --
   --  Add_New_Branch_To_MT_Using_PBA_Contingent
   --
   procedure Add_New_Branch_To_MT_Using_PBA_Contingent (
      Mount_Point_Lvl_Idx   :        Tree_Level_Index_Type;
      Mount_Point_Child_Idx :        Tree_Child_Index_Type;
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
                  Child_Idx :
                     constant Type_2_Node_Block_Index_Type := (
                        if Lvl_Idx = Mount_Point_Lvl_Idx
                        then
                           Type_2_Node_Block_Index_Type (
                              Mount_Point_Child_Idx)
                        else 0);

                  Child_PBA : Physical_Block_Address_Type;
               begin

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

               end Declare_First_Child_Idx;

            end if;

         end loop Set_Child_PBAs_In_New_Branch;

      end if;

   end Add_New_Branch_To_MT_Using_PBA_Contingent;

   --
   --  Execute_MT_Ext_Step_Read_Inner_Node_Completed
   --
   procedure Execute_MT_Ext_Step_Read_Inner_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean)
   is
   begin

      if not Primitive.Success (Job.Generated_Prim) then
         raise Program_Error;
      end if;

      if Job.Lvl_Idx > 1 then

         if Job.Lvl_Idx = Job.MT_Max_Lvl_Idx then

            if Hash_Of_T1_Node_Blk (Job.T1_Blks (Job.Lvl_Idx)) /=
               Job.MT_Root.Hash
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
                     Job.VBA, Parent_Lvl_Idx, Job.MT_Degree);
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
               T1_Child_Idx_For_VBA (Job.VBA, Parent_Lvl_Idx, Job.MT_Degree);

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
                  Tg     => Primitive.Tag_MT_Rszg_Cache,
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

               Declare_New_PBAs_Set_For_All_Read_Lvls :
               declare
                  New_PBAs_Set_For_All_Read_Lvls : Boolean := True;
               begin

                  Find_Read_Lvl_That_Needs_PBA_Alloc :
                  for Lvl_Idx in Parent_Lvl_Idx .. Job.MT_Max_Lvl_Idx loop

                     if Job.New_PBAs (Lvl_Idx) = PBA_Invalid then

                        if Job.Old_Generations (Lvl_Idx) = Job.Curr_Gen then

                           Job.New_PBAs (Lvl_Idx) := Job.Old_PBAs (Lvl_Idx);

                        else

                           Job.Alloc_Lvl_Idx := Lvl_Idx;

                           pragma Debug (Debug.Print_String (
                              "   Alloc LVL " &
                              Debug.To_String (Debug.Uint64_Type (
                                 Job.Alloc_Lvl_Idx)) &
                              " "));

                           Job.Generated_Prim :=
                              Primitive.Valid_Object_No_Pool_Idx (
                                 Op     => Primitive_Operation_Type'First,
                                 Succ   => False,
                                 Tg     => Primitive.Tag_MT_Rszg_MT_Alloc,
                                 Blk_Nr => Block_Number_Type'First,
                                 Idx    => Primitive.Index_Type (Job_Idx));

                           Job.State := Alloc_PBA_Pending;
                           Progress := True;

                           New_PBAs_Set_For_All_Read_Lvls := False;
                           exit Find_Read_Lvl_That_Needs_PBA_Alloc;

                        end if;

                     elsif
                        Job.Old_Generations (Lvl_Idx) /= Job.Curr_Gen and then
                        Job.New_PBAs (Lvl_Idx) = Job.Old_PBAs (Lvl_Idx)
                     then
                        raise Program_Error;
                     elsif
                        Job.Old_Generations (Lvl_Idx) = Job.Curr_Gen and then
                        Job.New_PBAs (Lvl_Idx) /= Job.Old_PBAs (Lvl_Idx)
                     then
                        raise Program_Error;
                     end if;

                  end loop Find_Read_Lvl_That_Needs_PBA_Alloc;

                  if New_PBAs_Set_For_All_Read_Lvls then

                     Add_New_Branch_To_MT_Using_PBA_Contingent (
                        Mount_Point_Lvl_Idx   => Parent_Lvl_Idx,
                        Mount_Point_Child_Idx =>
                           Tree_Child_Index_Type (Child_Idx),
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
                        Max_Lvl_Idx => Job.MT_Max_Lvl_Idx,
                        Lvl_Idx     => Job.Lvl_Idx,
                        PBA         => Job.New_PBAs (Job.Lvl_Idx),
                        Prim_Idx    => Primitive.Index_Type (Job_Idx),
                        Job_State   => Job.State,
                        Progress    => Progress,
                        Prim        => Job.Generated_Prim);

                  end if;

               end Declare_New_PBAs_Set_For_All_Read_Lvls;

            end if;

         end Declare_Child_1;

      else

         Declare_Child_Idx_4 :
         declare
            Parent_Lvl_Idx : constant Type_1_Node_Blocks_Index_Type :=
               Job.Lvl_Idx + 1;

            Child_Idx : constant Type_1_Node_Block_Index_Type :=
               T1_Child_Idx_For_VBA (
                  Job.VBA, Parent_Lvl_Idx, Job.MT_Degree);
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
               T2_Child_Idx_For_VBA (Job.VBA, Job.MT_Degree);

            Child : constant Type_2_Node_Type := Job.T2_Blk (Child_Idx);
         begin

            if Type_2_Node_Valid (Child) then
               raise Program_Error;
            end if;

            Declare_New_PBAs_Set_For_All_Read_Lvls_1 :
            declare
               New_PBAs_Set_For_All_Read_Lvls : Boolean := True;
            begin

               Find_Read_Lvl_That_Needs_PBA_Alloc_1 :
               for Lvl_Idx in Parent_Lvl_Idx .. Job.MT_Max_Lvl_Idx loop

                  if Job.New_PBAs (Lvl_Idx) = PBA_Invalid then

                     if Job.Old_Generations (Lvl_Idx) = Job.Curr_Gen then

                        Job.New_PBAs (Lvl_Idx) := Job.Old_PBAs (Lvl_Idx);

                     else

                        Job.Alloc_Lvl_Idx := Lvl_Idx;

                        pragma Debug (Debug.Print_String (
                           "   Alloc LVL " &
                           Debug.To_String (Debug.Uint64_Type (
                              Job.Alloc_Lvl_Idx)) &
                           " "));

                        Job.Generated_Prim :=
                           Primitive.Valid_Object_No_Pool_Idx (
                              Op     => Primitive_Operation_Type'First,
                              Succ   => False,
                              Tg     => Primitive.Tag_MT_Rszg_MT_Alloc,
                              Blk_Nr => Block_Number_Type'First,
                              Idx    => Primitive.Index_Type (Job_Idx));

                        Job.State := Alloc_PBA_Pending;
                        Progress := True;

                        New_PBAs_Set_For_All_Read_Lvls := False;
                        exit Find_Read_Lvl_That_Needs_PBA_Alloc_1;

                     end if;

                  elsif
                     Job.Old_Generations (Lvl_Idx) /= Job.Curr_Gen and then
                     Job.New_PBAs (Lvl_Idx) = Job.Old_PBAs (Lvl_Idx)
                  then
                     raise Program_Error;
                  elsif
                     Job.Old_Generations (Lvl_Idx) = Job.Curr_Gen and then
                     Job.New_PBAs (Lvl_Idx) /= Job.Old_PBAs (Lvl_Idx)
                  then
                     raise Program_Error;
                  end if;

               end loop Find_Read_Lvl_That_Needs_PBA_Alloc_1;

               if New_PBAs_Set_For_All_Read_Lvls then

                  Add_New_Branch_To_MT_Using_PBA_Contingent (
                     Mount_Point_Lvl_Idx   => Parent_Lvl_Idx,
                     Mount_Point_Child_Idx =>
                        Tree_Child_Index_Type (Child_Idx),
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
                     Max_Lvl_Idx => Job.MT_Max_Lvl_Idx,
                     Lvl_Idx     => Job.Lvl_Idx,
                     PBA         => Job.New_PBAs (Job.Lvl_Idx),
                     Prim_Idx    => Primitive.Index_Type (Job_Idx),
                     Job_State   => Job.State,
                     Progress    => Progress,
                     Prim        => Job.Generated_Prim);

               end if;

            end Declare_New_PBAs_Set_For_All_Read_Lvls_1;

         end Declare_Child_3;

      end if;

   end Execute_MT_Ext_Step_Read_Inner_Node_Completed;

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
         Tg     => Primitive.Tag_MT_Rszg_Cache,
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
   --  Execute_Extend_By_One_Leaf
   --
   procedure Execute_Extend_By_One_Leaf (
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
               Job.MT_Nr_Of_Leaves);

         Job.Old_PBAs := (others => PBA_Invalid);
         Job.Old_Generations := (others => 0);
         Job.New_PBAs := (others => PBA_Invalid);

         Job.Lvl_Idx := Job.MT_Max_Lvl_Idx;
         Job.Old_PBAs (Job.Lvl_Idx) := Job.MT_Root.PBA;
         Job.Old_Generations (Job.Lvl_Idx) := Job.MT_Root.Gen;

         if Job.VBA <= Tree_Max_Max_VBA (Job.MT_Degree, Job.MT_Max_Lvl_Idx)
         then

            Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Read,
               Succ   => False,
               Tg     => Primitive.Tag_MT_Rszg_Cache,
               Blk_Nr => Block_Number_Type (Job.MT_Root.PBA),
               Idx    => Primitive.Index_Type (Job_Idx));

            pragma Debug (Debug.Print_String (
               "   READ LVL " &
               Debug.To_String (Debug.Uint64_Type (Job.Lvl_Idx)) &
               " PARENT MT_ROOT PBA " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.MT_Root.PBA)) &
               " GEN " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.MT_Root.Gen)) &
               " LEAVES " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.MT_Nr_Of_Leaves)) &
               " MAX_LVL " &
               Debug.To_String (Debug.Uint64_Type (
                  Job.MT_Max_Lvl_Idx)) &
               " " &
               Debug.To_String (Job.MT_Root.Hash) &
               " "));

            Job.State := Read_Root_Node_Pending;
            Progress := True;

         else

            Add_New_Root_Lvl_To_MT_Using_PBA_Contingent (
               MT_Root          => Job.MT_Root,
               MT_Max_Lvl_Idx   => Job.MT_Max_Lvl_Idx,
               MT_Nr_Of_Leaves  => Job.MT_Nr_Of_Leaves,
               Curr_Gen         => Job.Curr_Gen,
               T1_Blks          => Job.T1_Blks,
               New_PBAs         => Job.New_PBAs,
               First_PBA        => Job.PBA,
               Nr_Of_PBAs       => Job.Nr_Of_PBAs);

            Add_New_Branch_To_MT_Using_PBA_Contingent (
               Mount_Point_Lvl_Idx   => Job.MT_Max_Lvl_Idx,
               Mount_Point_Child_Idx => 1,
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
               Max_Lvl_Idx => Job.MT_Max_Lvl_Idx,
               Lvl_Idx     => Job.Lvl_Idx,
               PBA         => Job.New_PBAs (Job.Lvl_Idx),
               Prim_Idx    => Primitive.Index_Type (Job_Idx),
               Job_State   => Job.State,
               Progress    => Progress,
               Prim        => Job.Generated_Prim);

         end if;

      when Read_Root_Node_Completed =>

         Execute_MT_Ext_Step_Read_Inner_Node_Completed (
            Job, Job_Idx, Progress);

      when Read_Inner_Node_Completed =>

         Execute_MT_Ext_Step_Read_Inner_Node_Completed (
            Job, Job_Idx, Progress);

      when Alloc_PBA_Completed =>

         Job.Old_PBAs := (others => PBA_Invalid);
         Job.Old_Generations := (others => 0);

         Job.Lvl_Idx := Job.MT_Max_Lvl_Idx;
         Job.Old_PBAs (Job.Lvl_Idx) := Job.MT_Root.PBA;
         Job.Old_Generations (Job.Lvl_Idx) := Job.MT_Root.Gen;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_MT_Rszg_Cache,
            Blk_Nr => Block_Number_Type (Job.MT_Root.PBA),
            Idx    => Primitive.Index_Type (Job_Idx));

         pragma Debug (Debug.Print_String (
            "   READ LVL " &
            Debug.To_String (Debug.Uint64_Type (Job.Lvl_Idx)) &
            " PARENT MT_ROOT PBA " &
            Debug.To_String (Debug.Uint64_Type (
               Job.MT_Root.PBA)) &
            " GEN " &
            Debug.To_String (Debug.Uint64_Type (
               Job.MT_Root.Gen)) &
            " LEAVES " &
            Debug.To_String (Debug.Uint64_Type (
               Job.MT_Nr_Of_Leaves)) &
            " MAX_LVL " &
            Debug.To_String (Debug.Uint64_Type (
               Job.MT_Max_Lvl_Idx)) &
            " " &
            Debug.To_String (Job.MT_Root.Hash) &
            " "));

         Job.State := Read_Root_Node_Pending;
         Progress := True;

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
                     Job.VBA, Parent_Lvl_Idx, Job.MT_Degree);
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
                  Max_Lvl_Idx => Job.MT_Max_Lvl_Idx,
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
                     Job.VBA, Parent_Lvl_Idx, Job.MT_Degree);
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
                  Max_Lvl_Idx => Job.MT_Max_Lvl_Idx,
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

            Job.MT_Root := (
               PBA => Child_PBA,
               Gen => Job.Curr_Gen,
               Hash => Hash_Of_T1_Node_Blk (Job.T1_Blks (Child_Lvl_Idx)));

            Job.MT_Nr_Of_Leaves := Job.MT_Nr_Of_Leaves + Job.Nr_Of_Leaves;

         end Declare_Child_Idx_7;

         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Extend_By_One_Leaf;

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
         when Extend_By_One_Leaf =>

            Execute_Extend_By_One_Leaf (Rszg.Jobs (Idx), Idx, Progress);

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
   --  Peek_Generated_MT_Root
   --
   function Peek_Generated_MT_Root (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
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

            return Rszg.Jobs (Idx).MT_Root;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_MT_Root;

   --
   --  Peek_Generated_MT_Geom
   --
   function Peek_Generated_MT_Geom (
      Rszg : Resizing_Type;
      Prim : Primitive.Object_Type)
   return Tree_Geometry_Type
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

            return (
               Rszg.Jobs (Idx).MT_Max_Lvl_Idx,
               Rszg.Jobs (Idx).MT_Degree,
               Rszg.Jobs (Idx).MT_Nr_Of_Leaves);

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_MT_Geom;

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
   --  Mark_Generated_Prim_Completed_PBA_Alloc
   --
   procedure Mark_Generated_Prim_Completed_PBA_Alloc (
      Rszg    : in out Resizing_Type;
      Prim    :        Primitive.Object_Type;
      MT_Root :        Type_1_Node_Type;
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
            Rszg.Jobs (Idx).MT_Root := MT_Root;
            Rszg.Jobs (Idx).New_PBAs (Rszg.Jobs (Idx).Alloc_Lvl_Idx) :=
               New_PBA;

            return;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Prim_Completed_PBA_Alloc;

end CBE.MT_Resizing;
