--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Cache
with SPARK_Mode
is
   --
   --  Slot_Initialize
   --
   procedure Slot_Initialize (Slot : out Slot_Type)
   is
   begin
      Slot.State := Invalid;
      Slot.Prim := Primitive.Invalid_Object;
      Slot.PBA := 0;
   end Slot_Initialize;

   --
   --  Job_Initialize
   --
   procedure Job_Initialize (Job : out Job_Type)
   is
   begin
      Job.State := Invalid;
      Job.Prim := Primitive.Invalid_Object;
      Job.PBA := 0;
   end Job_Initialize;

   --
   --  Initialize
   --
   procedure Initialize (Cache : out Cache_Type)
   is
   begin
      Initialize_Each_Slot :
      for Idx in Cache.Slots'Range loop
         Slot_Initialize (Cache.Slots (Idx));
      end loop Initialize_Each_Slot;

      Initialize_Each_Job :
      for Idx in Cache.Jobs'Range loop
         Job_Initialize (Cache.Jobs (Idx));
      end loop Initialize_Each_Job;

   end Initialize;

   --
   --  Dirty
   --
   function Dirty (Cache : Cache_Type)
   return Boolean
   is (
      for some Slot of Cache.Slots =>
         Slot.State = Dirty or else
         Slot.State = Write_Generated or else
         Slot.State = Write_In_Progress);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Cache : Cache_Type)
   return Boolean
   is (for some Job of Cache.Jobs => Job.State = Invalid);

   --
   --  Assert_PBA_Not_Affected_By_Any_Job
   --
   procedure Assert_PBA_Not_Affected_By_Any_Job (
      Jobs : Jobs_Type;
      PBA  : Physical_Block_Address_Type)
   is
   begin
      Find_Job_That_Affects_PBA :
      for Job of Jobs loop

         case Job.State is
         when Submitted | Completed =>

            case Primitive.Operation (Job.Prim) is
            when Read | Write =>

               if Job.PBA = PBA then
                  raise Program_Error;
               end if;

            when Sync =>

               raise Program_Error;

            end case;

         when Invalid => null;
         end case;

      end loop Find_Job_That_Affects_PBA;

   end Assert_PBA_Not_Affected_By_Any_Job;

   --
   --  Assert_PBA_Not_Writtem_By_Any_Job
   --
   procedure Assert_PBA_Not_Written_By_Any_Job (
      Jobs : Jobs_Type;
      PBA  : Physical_Block_Address_Type)
   is
   begin
      Find_Job_That_Writes_PBA :
      for Job of Jobs loop

         case Job.State is
         when Submitted | Completed =>

            case Primitive.Operation (Job.Prim) is
            when Write =>

               if Job.PBA = PBA then
                  raise Program_Error;
               end if;

            when Sync | Read =>

               null;

            end case;

         when Invalid => null;
         end case;

      end loop Find_Job_That_Writes_PBA;

   end Assert_PBA_Not_Written_By_Any_Job;

   --
   --  Submit_Primitive_Assertions
   --
   procedure Submit_Primitive_Assertions (
      Jobs     : Jobs_Type;
      Prim     : Primitive.Object_Type;
      Prim_PBA : Physical_Block_Address_Type)
   is
   begin
      if not Primitive.Valid (Prim) or else
         Primitive.Success (Prim)
      then
         raise Program_Error;
      end if;

      case Primitive.Operation (Prim) is
      when Write => Assert_PBA_Not_Affected_By_Any_Job (Jobs, Prim_PBA);
      when Read  => Assert_PBA_Not_Written_By_Any_Job (Jobs, Prim_PBA);
      when Sync  => Assert_PBA_Not_Written_By_Any_Job (Jobs, Prim_PBA);
      end case;
   end Submit_Primitive_Assertions;

   --
   --  Submit_Primitive_Without_Data
   --
   procedure Submit_Primitive_Without_Data (
      Cache   : in out Cache_Type;
      Prim    :        Primitive.Object_Type)
   is
      Prim_PBA : constant Physical_Block_Address_Type :=
         Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      Submit_Primitive_Assertions (Cache.Jobs, Prim, Prim_PBA);

      Find_Invalid_Job :
      for Idx in Cache.Jobs'Range loop

         case Cache.Jobs (Idx).State is
         when Invalid =>

            Cache.Jobs (Idx).State := Submitted;
            Cache.Jobs (Idx).Prim := Prim;
            Cache.Jobs (Idx).PBA := Prim_PBA;
            return;

         when Submitted | Completed => null;
         end case;

      end loop Find_Invalid_Job;

      raise Program_Error;

   end Submit_Primitive_Without_Data;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Cache   : in out Cache_Type;
      Prim    :        Primitive.Object_Type;
      Job_Idx :    out Jobs_Index_Type)
   is
      Prim_PBA : constant Physical_Block_Address_Type :=
         Physical_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      Submit_Primitive_Assertions (Cache.Jobs, Prim, Prim_PBA);

      Find_Invalid_Job :
      for Idx in Cache.Jobs'Range loop

         case Cache.Jobs (Idx).State is
         when Invalid =>

            Job_Idx := Idx;
            Cache.Jobs (Idx).State := Submitted;
            Cache.Jobs (Idx).Prim := Prim;
            Cache.Jobs (Idx).PBA := Prim_PBA;
            return;

         when Submitted | Completed => null;
         end case;

      end loop Find_Invalid_Job;

      raise Program_Error;

   end Submit_Primitive;

   --
   --  Clean_Slot_Evictable
   --
   function Clean_Slot_Evictable (
      Slot : Slot_Type;
      Jobs : Jobs_Type)
   return Boolean
   is
   begin

      Find_Job_That_Accesses_Slot_Data :
      for Job of Jobs loop

         case Primitive.Operation (Job.Prim) is
         when Read | Write =>

            if Job.PBA = Slot.PBA then
               return False;
            end if;

         when Sync =>

            null;

         end case;

      end loop Find_Job_That_Accesses_Slot_Data;
      return True;

   end Clean_Slot_Evictable;

   --
   --  Write_Back_One_Dirty_Slot
   --
   procedure Write_Back_One_Dirty_Slot (
      Slots    : in out Slots_Type;
      Progress : in out Boolean)
   is
   begin
      Find_Dirty_Slot :
      for Idx in Slots'Range loop

         if Slots (Idx).State = Dirty then

            Slots (Idx).State := Write_Generated;
            Slots (Idx).Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_Cache_Blk_IO,
                  Block_Number_Type (Slots (Idx).PBA),
                  Primitive.Index_Type (Idx));

            Progress := True;
            return;

         end if;

      end loop Find_Dirty_Slot;

      raise Program_Error;

   end Write_Back_One_Dirty_Slot;

   --
   --  Job_Execute_Sync
   --
   procedure Job_Execute_Sync (
      Slots    : in out Slots_Type;
      Job      : in out Job_Type;
      Progress : in out Boolean)
   is
      Dirty_Slot_Found : Boolean := False;
   begin
      Find_Dirty_Slots :
      for Idx in Slots'Range loop

         case Slots (Idx).State is
         when Invalid | Read_Generated | Read_In_Progress | Clean =>
            null;

         when Dirty =>

            Slots (Idx).State := Write_Generated;
            Slots (Idx).Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_Cache_Blk_IO,
                  Block_Number_Type (Slots (Idx).PBA),
                  Primitive.Index_Type (Idx));

            Progress := True;
            Dirty_Slot_Found := True;

         when Write_Generated | Write_In_Progress =>

            Dirty_Slot_Found := True;

         end case;

      end loop Find_Dirty_Slots;

      if not Dirty_Slot_Found then

         Job.State := Completed;
         Primitive.Success (Job.Prim, True);
         Progress := True;

      end if;

   end Job_Execute_Sync;

   --
   --  Job_Execute_Read
   --
   procedure Job_Execute_Read (
      Slots      : in out Slots_Type;
      Slots_Data :        Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   : in out Block_Data_Type;
      Progress   : in out Boolean)
   is
      Write_Back_In_Progress : Boolean := False;
   begin
      Find_Matching_Slot :
      for Idx in Slots'Range loop

         case Slots (Idx).State is
         when Clean | Dirty | Write_Generated | Write_In_Progress =>

            if Slots (Idx).PBA = Jobs (Job_Idx).PBA then

               Job_Data := Slots_Data (Idx);
               Primitive.Success (Jobs (Job_Idx).Prim, True);
               Jobs (Job_Idx).State := Completed;
               Progress := True;
               return;

            end if;

         when Read_Generated | Read_In_Progress =>

            if Slots (Idx).PBA = Jobs (Job_Idx).PBA then
               return;
            end if;

         when Invalid => null;
         end case;

      end loop Find_Matching_Slot;

      Find_Evictable_Slot :
      for Idx in Slots'Range loop

         case Slots (Idx).State is
         when Invalid =>

            Slots (Idx).State := Read_Generated;
            Slots (Idx).PBA := Jobs (Job_Idx).PBA;
            Slots (Idx).Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_Cache_Blk_IO,
                  Block_Number_Type (Slots (Idx).PBA),
                  Primitive.Index_Type (Idx));

            Progress := True;
            return;

         when Clean =>

            if Clean_Slot_Evictable (Slots (Idx), Jobs) then

               Slots (Idx).State := Read_Generated;
               Slots (Idx).PBA := Jobs (Job_Idx).PBA;
               Slots (Idx).Prim :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_Cache_Blk_IO,
                     Block_Number_Type (Slots (Idx).PBA),
                     Primitive.Index_Type (Idx));

               Progress := True;
               return;

            end if;

         when Write_Generated | Write_In_Progress =>

            Write_Back_In_Progress := True;

         when Dirty | Read_Generated | Read_In_Progress =>

            null;

         end case;

      end loop Find_Evictable_Slot;

      if not Write_Back_In_Progress then
         Write_Back_One_Dirty_Slot (Slots, Progress);
      end if;

   end Job_Execute_Read;

   --
   --  Job_Execute_Write
   --
   procedure Job_Execute_Write (
      Slots      : in out Slots_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   :        Block_Data_Type;
      Progress   : in out Boolean)
   is
      Write_Back_In_Progress : Boolean := False;
   begin
      Find_Matching_Slot :
      for Idx in Slots'Range loop

         case Slots (Idx).State is
         when Clean | Dirty =>

            if Slots (Idx).PBA = Jobs (Job_Idx).PBA then

               Slots_Data (Idx) := Job_Data;
               Slots (Idx).State := Dirty;

               Primitive.Success (Jobs (Job_Idx).Prim, True);
               Jobs (Job_Idx).State := Completed;
               Progress := True;
               return;

            end if;

         when
            Read_Generated | Read_In_Progress | Write_Generated |
            Write_In_Progress
         =>
            if Slots (Idx).PBA = Jobs (Job_Idx).PBA then
               return;
            end if;

         when Invalid => null;
         end case;

      end loop Find_Matching_Slot;

      Find_Evictable_Slot :
      for Idx in Slots'Range loop

         case Slots (Idx).State is
         when Invalid =>

            Slots_Data (Idx) := Job_Data;
            Slots (Idx).State := Dirty;
            Slots (Idx).PBA := Jobs (Job_Idx).PBA;

            Primitive.Success (Jobs (Job_Idx).Prim, True);
            Jobs (Job_Idx).State := Completed;
            Progress := True;
            return;

         when Clean =>

            if Clean_Slot_Evictable (Slots (Idx), Jobs) then

               Slots_Data (Idx) := Job_Data;
               Slots (Idx).State := Dirty;
               Slots (Idx).PBA := Jobs (Job_Idx).PBA;

               Primitive.Success (Jobs (Job_Idx).Prim, True);
               Jobs (Job_Idx).State := Completed;
               Progress := True;
               return;

            end if;

         when Write_Generated | Write_In_Progress =>

            Write_Back_In_Progress := True;

         when Dirty | Read_Generated | Read_In_Progress =>

            null;

         end case;

      end loop Find_Evictable_Slot;

      if not Write_Back_In_Progress then
         Write_Back_One_Dirty_Slot (Slots, Progress);
      end if;

   end Job_Execute_Write;

   --
   --  Job_Execute
   --
   procedure Job_Execute (
      Slots      : in out Slots_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   : in out Block_Data_Type;
      Progress   : in out Boolean)
   is
   begin
      case Jobs (Job_Idx).State is
      when Submitted =>

         case Primitive.Operation (Jobs (Job_Idx).Prim) is
         when Sync =>
            Job_Execute_Sync (Slots, Jobs (Job_Idx), Progress);

         when Read =>
            Job_Execute_Read (
               Slots, Slots_Data, Jobs, Job_Idx, Job_Data, Progress);

         when Write =>
            Job_Execute_Write (
               Slots, Slots_Data, Jobs, Job_Idx, Job_Data, Progress);

         end case;

      when Invalid | Completed => null;
      end case;

   end Job_Execute;

   --
   --  Execute
   --
   procedure Execute (
      Cache      : in out Cache_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs_Data  : in out Jobs_Data_Type;
      Progress   : in out Boolean)
   is
   begin

      Execute_Each_Job :
      for Idx in Cache.Jobs'Range loop

         Job_Execute (
            Cache.Slots, Slots_Data, Cache.Jobs, Idx, Jobs_Data (Idx),
            Progress);

      end loop Execute_Each_Job;

   end Execute;

   --
   --  Peek_Completed_Primitive
   --
   procedure Peek_Completed_Primitive (
      Cache   :     Cache_Type;
      Prim    : out Primitive.Object_Type;
      Job_Idx : out Jobs_Index_Type)
   is
   begin
      Find_Completed_Job :
      for Idx in Cache.Jobs'Range loop

         case Cache.Jobs (Idx).State is
         when Completed =>

            Prim := Cache.Jobs (Idx).Prim;
            Job_Idx := Idx;
            return;

         when Invalid | Submitted => null;
         end case;

      end loop Find_Completed_Job;

      Prim := Primitive.Invalid_Object;
      Job_Idx := Jobs_Index_Type'First;

   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Cache   : in out Cache_Type;
      Job_Idx :        Jobs_Index_Type)
   is
   begin
      case Cache.Jobs (Job_Idx).State is
      when Completed =>
         Cache.Jobs (Job_Idx).State := Invalid;
      when Invalid | Submitted =>
         raise Program_Error;
      end case;
   end Drop_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Cache    :     Cache_Type;
      Prim     : out Primitive.Object_Type;
      Slot_Idx : out Slots_Index_Type)
   is
   begin
      Find_Slot_With_Generated_Prim :
      for Idx in Cache.Slots'Range loop

         case Cache.Slots (Idx).State is
         when Read_Generated | Write_Generated =>

            Prim := Cache.Slots (Idx).Prim;
            Slot_Idx := Idx;
            return;

         when Invalid | Read_In_Progress | Write_In_Progress | Clean | Dirty =>

            null;

         end case;

      end loop Find_Slot_With_Generated_Prim;

      Prim := Primitive.Invalid_Object;
      Slot_Idx := 0;

   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Cache    : in out Cache_Type;
      Slot_Idx :        Slots_Index_Type)
   is
   begin
      case Cache.Slots (Slot_Idx).State is
      when Read_Generated =>
         Cache.Slots (Slot_Idx).State := Read_In_Progress;

      when Write_Generated =>
         Cache.Slots (Slot_Idx).State := Write_In_Progress;

      when Invalid | Read_In_Progress | Write_In_Progress | Clean | Dirty =>
         raise Program_Error;

      end case;
   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Cache    : in out Cache_Type;
      Slot_Idx :        Slots_Index_Type;
      Success  :        Boolean)
   is
   begin
      if not Success then
         raise Program_Error;
      end if;

      case Cache.Slots (Slot_Idx).State is
      when Read_In_Progress | Write_In_Progress =>

         Cache.Slots (Slot_Idx).State := Clean;
         Primitive.Success (Cache.Slots (Slot_Idx).Prim, True);

      when Invalid | Read_Generated | Write_Generated | Clean | Dirty =>
         raise Program_Error;

      end case;
   end Mark_Generated_Primitive_Complete;

end CBE.Cache;
