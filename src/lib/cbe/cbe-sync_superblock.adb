--
--  Copyright (C) 2020 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;
with CBE.Debug;
pragma Unreferenced (CBE.Debug);

package body CBE.Sync_Superblock
with SPARK_Mode
is
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj.State           := Invalid;
      Obj.Index           := 0;
      Obj.Pool_Index_Slot := Pool_Idx_Slot_Invalid;
      Obj.Gen             := 0;
      Obj.Success         := False;
   end Initialize_Object;

   function Request_Acceptable (Obj : Object_Type)
   return Boolean is (Obj.State = Invalid);

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj        : out Object_Type;
      Pool_Index :     Pool_Index_Type;
      Idx        :     Superblocks_Index_Type;
      Gen        :     Generation_Type)
   is
   begin
      Obj.State           := Cache_Flush_Pending;
      Obj.Index           := Idx;
      Obj.Pool_Index_Slot := Pool_Idx_Slot_Valid (Pool_Index);
      Obj.Gen             := Gen;
      Obj.Success         := False;
   end Submit_Request;

   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      case Obj.State is
      when Cache_Flush_Pending | Cache_Flush_In_Progress =>
         null;
      when Cache_Flush_Complete =>
         Obj.State := Write_SB_Pending;
         Progress := True;
      when Write_SB_Pending | Write_SB_In_Progress =>
         null;
      when Write_SB_Complete =>
         Obj.State := Sync_Pending;
         Progress := True;
      when Sync_Pending | Sync_In_Progress =>
         null;
      when Invalid | Sync_Complete =>
         null;
      end case;
   end Execute;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State = Sync_Complete then
         return Primitive.Valid_Object (
            Op     => Sync,
            Succ   => Obj.Success,
            Tg     => Primitive.Tag_Sync_SB_Sync,
            Pl_Idx => Pool_Idx_Slot_Content (Obj.Pool_Index_Slot),
            Blk_Nr => 0, --  XXX add proper block number
            Idx    => 0);
      else
         return Primitive.Invalid_Object;
      end if;
   end Peek_Completed_Primitive;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type
   is
   begin
      if Obj.State = Sync_Complete
         and then Primitive.Pool_Idx_Slot (Prim) = Obj.Pool_Index_Slot
      then
         return Obj.Gen;
      else
         raise Program_Error;
      end if;
   end Peek_Completed_Generation;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State = Sync_Complete
         and then Primitive.Pool_Idx_Slot (Prim) = Obj.Pool_Index_Slot
      then
         Obj.State := Invalid;
      else
         raise Program_Error;
      end if;
   end Drop_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      case Obj.State is
      when Cache_Flush_Pending =>
         return Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => False,
            Tg     => Primitive.Tag_Sync_SB_Cache_Flush,
            Blk_Nr => 0,
            Idx    => 0);
      when Write_SB_Pending =>
         return Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => Request.Success_Type (False),
            Tg     => Primitive.Tag_Sync_SB_Write_SB,
            --  there is currently a 1:1 mapping between SB slot and pba
            Blk_Nr => Block_Number_Type (Obj.Index),
            Idx    => 0);
      when Sync_Pending =>
         return Primitive.Valid_Object_No_Pool_Idx (
            Op     => Sync,
            Succ   => Request.Success_Type (False),
            Tg     => Primitive.Tag_Sync_SB_Sync,
            Blk_Nr => 0,
            Idx    => 0);
      when others =>
         return Primitive.Invalid_Object;
      end case;
   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.State is
      when Cache_Flush_Pending =>
         Obj.State := Cache_Flush_In_Progress;
      when Write_SB_Pending =>
         Obj.State := Write_SB_In_Progress;
      when Sync_Pending =>
         Obj.State := Sync_In_Progress;
      when others =>
         raise Program_Error;
      end case;
   end Drop_Generated_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.State is
      when Cache_Flush_In_Progress =>
         Obj.State := Cache_Flush_Complete;
      when Write_SB_In_Progress =>
         Obj.State := Write_SB_Complete;
      when Sync_In_Progress =>
         Obj.State := Sync_Complete;
      when others =>
         raise Program_Error;
      end case;

      Obj.Success := Primitive.Success (Prim);

      --  shortcut in case any primitive was unsuccessful
      if not Obj.Success then
         Obj.State := Sync_Complete;
      end if;
   end Mark_Generated_Primitive_Complete;
end CBE.Sync_Superblock;
