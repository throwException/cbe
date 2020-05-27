--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Cache
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Jobs : constant := 4;
   Nr_Of_Slots : constant := 32;

   type Cache_Type is private;

   type Slots_Index_Type is range 0 .. Nr_Of_Slots - 1;

   type Slots_Data_Type is array (Slots_Index_Type) of Block_Data_Type;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   type Jobs_Data_Type is array (Jobs_Index_Type) of Block_Data_Type;

   --
   --  Initialize
   --
   procedure Initialize (Cache : out Cache_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Cache : Cache_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Cache   : in out Cache_Type;
      Prim    :        Primitive.Object_Type;
      Job_Idx :    out Jobs_Index_Type);

   --
   --  Submit_Primitive_Without_Data
   --
   procedure Submit_Primitive_Without_Data (
      Cache   : in out Cache_Type;
      Prim    :        Primitive.Object_Type);

   --
   --  Peek_Completed_Primitive
   --
   procedure Peek_Completed_Primitive (
      Cache   :     Cache_Type;
      Prim    : out Primitive.Object_Type;
      Job_Idx : out Jobs_Index_Type);

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Cache   : in out Cache_Type;
      Job_Idx :        Jobs_Index_Type);

   --
   --  Execute
   --
   procedure Execute (
      Cache      : in out Cache_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs_Data  : in out Jobs_Data_Type;
      Progress   : in out Boolean);

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Cache    :     Cache_Type;
      Prim     : out Primitive.Object_Type;
      Slot_Idx : out Slots_Index_Type);

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Cache    : in out Cache_Type;
      Slot_Idx :        Slots_Index_Type);

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Cache    : in out Cache_Type;
      Slot_Idx :        Slots_Index_Type;
      Success  :        Boolean);

   --
   --  Dirty
   --
   function Dirty (Cache : Cache_Type)
   return Boolean;

private

   type Slot_State_Type is (
      Invalid,
      Read_Generated,
      Read_In_Progress,
      Clean,
      Dirty,
      Write_Generated,
      Write_In_Progress);

   type Slot_Type is record
      State : Slot_State_Type;
      Prim : Primitive.Object_Type;
      PBA : Physical_Block_Address_Type;
   end record;

   type Slots_Type is array (Slots_Index_Type) of Slot_Type;

   type Job_State_Type is (
      Invalid,
      Submitted,
      Completed);

   type Job_Type is record
      State : Job_State_Type;
      Prim : Primitive.Object_Type;
      PBA : Physical_Block_Address_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Cache_Type is record
      Slots : Slots_Type;
      Jobs  : Jobs_Type;
   end record;

   --
   --  Slot_Initialize
   --
   procedure Slot_Initialize (Slot : out Slot_Type);

   --
   --  Job_Initialize
   --
   procedure Job_Initialize (Job : out Job_Type);

   --
   --  Job_Execute_Sync
   --
   procedure Job_Execute_Sync (
      Slots      : in out Slots_Type;
      Job        : in out Job_Type;
      Progress   : in out Boolean);

   --
   --  Job_Execute_Read
   --
   procedure Job_Execute_Read (
      Slots      : in out Slots_Type;
      Slots_Data :        Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   : in out Block_Data_Type;
      Progress   : in out Boolean);

   --
   --  Job_Execute_Write
   --
   procedure Job_Execute_Write (
      Slots      : in out Slots_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   :        Block_Data_Type;
      Progress   : in out Boolean);

   --
   --  Job_Execute
   --
   procedure Job_Execute (
      Slots      : in out Slots_Type;
      Slots_Data : in out Slots_Data_Type;
      Jobs       : in out Jobs_Type;
      Job_Idx    :        Jobs_Index_Type;
      Job_Data   : in out Block_Data_Type;
      Progress   : in out Boolean);

   --
   --  Clean_Slot_Evictable
   --
   function Clean_Slot_Evictable (
      Slot : Slot_Type;
      Jobs : Jobs_Type)
   return Boolean;

   --
   --  Write_Back_One_Dirty_Slot
   --
   procedure Write_Back_One_Dirty_Slot (
      Slots      : in out Slots_Type;
      Progress   :    out Boolean);

   --
   --  Submit_Primitive_Assertions
   --
   procedure Submit_Primitive_Assertions (
      Jobs     : Jobs_Type;
      Prim     : Primitive.Object_Type;
      Prim_PBA : Physical_Block_Address_Type);

   --
   --  Assert_PBA_Not_Affected_By_Any_Job
   --
   procedure Assert_PBA_Not_Affected_By_Any_Job (
      Jobs : Jobs_Type;
      PBA  : Physical_Block_Address_Type);

   --
   --  Assert_PBA_Not_Writtem_By_Any_Job
   --
   procedure Assert_PBA_Not_Written_By_Any_Job (
      Jobs : Jobs_Type;
      PBA  : Physical_Block_Address_Type);

end CBE.Cache;
