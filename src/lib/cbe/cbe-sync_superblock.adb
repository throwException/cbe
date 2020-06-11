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
      Obj.SB_Cipher                := Superblock_Ciphertext_Invalid;
      Obj.Current_Key_Value_Plain  := (others => Byte_Type'First);
      Obj.Previous_Key_Value_Plain := (others => Byte_Type'First);

      Obj.State           := Invalid;
      Obj.Index           := 0;
      Obj.Pool_Index_Slot := Pool_Idx_Slot_Invalid;
      Obj.Gen             := 0;
      Obj.Success         := False;
   end Initialize_Object;

   function Request_Acceptable (Obj : Object_Type)
   return Boolean is (Obj.State = Invalid);

   --
   --  Init_SB_Ciphertext_Without_Key_Values
   --
   procedure Init_SB_Ciphertext_Without_Key_Values (
      SB_Plain  :     Superblock_Type;
      SB_Cipher : out Superblock_Ciphertext_Type);

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
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj        : out Object_Type;
      Pool_Index :     Pool_Index_Type;
      SB_Plain   :     Superblock_Type;
      Idx        :     Superblocks_Index_Type;
      Gen        :     Generation_Type)
   is
   begin
      Init_SB_Ciphertext_Without_Key_Values (SB_Plain, Obj.SB_Cipher);
      Obj.Current_Key_Value_Plain  := SB_Plain.Current_Key.Value;
      Obj.Previous_Key_Value_Plain := SB_Plain.Previous_Key.Value;

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
         Obj.State := Encrypt_Current_Key_Pending;
         Progress := True;
      when Encrypt_Current_Key_Pending | Encrypt_Current_Key_In_Progress =>
         null;
      when Encrypt_Current_Key_Complete =>
         Obj.Current_Key_Value_Plain := (others => Byte_Type'First);
         Obj.State := Encrypt_Previous_Key_Pending;
         Progress := True;
      when Encrypt_Previous_Key_Pending | Encrypt_Previous_Key_In_Progress =>
         null;
      when Encrypt_Previous_Key_Complete =>
         Obj.Previous_Key_Value_Plain := (others => Byte_Type'First);
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
      when Encrypt_Current_Key_Pending =>
         return Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Sync_SB_TA_Encrypt_Key,
            Blk_Nr => 0,
            Idx    => 0);
      when Encrypt_Previous_Key_Pending =>
         return Primitive.Valid_Object_No_Pool_Idx (
            Op     => Primitive_Operation_Type'First,
            Succ   => False,
            Tg     => Primitive.Tag_Sync_SB_TA_Encrypt_Key,
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

   function Peek_Generated_Superblock_Ciphertext (
      Obj       : Object_Type;
      Prim      : Primitive.Object_Type)
   return Superblock_Ciphertext_Type
   is
   begin
      if not Primitive.Has_Tag_Sync_SB_Write_SB (Prim) then
         raise Program_Error;
      end if;

      case Obj.State is
      when Write_SB_Pending =>
         return Obj.SB_Cipher;
      when others =>
         raise Program_Error;
      end case;
   end Peek_Generated_Superblock_Ciphertext;

   function Peek_Generated_Key_Value_Plaintext (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_Value_Plaintext_Type
   is
   begin
      if not Primitive.Has_Tag_Sync_SB_TA_Encrypt_Key (Prim) then
         raise Program_Error;
      end if;

      case Obj.State is
      when Encrypt_Current_Key_Pending =>
         return Obj.Current_Key_Value_Plain;
      when Encrypt_Previous_Key_Pending =>
         return Obj.Previous_Key_Value_Plain;
      when others =>
         raise Program_Error;
      end case;
   end Peek_Generated_Key_Value_Plaintext;

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
      when Encrypt_Current_Key_Pending =>
         Obj.State := Encrypt_Current_Key_In_Progress;
      when Encrypt_Previous_Key_Pending =>
         Obj.State := Encrypt_Previous_Key_In_Progress;
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

   procedure Mark_Generated_Prim_Complete_Key_Value_Ciphertext (
      Obj       : in out Object_Type;
      Prim      :        Primitive.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type)
   is
   begin
      case Obj.State is
      when Encrypt_Current_Key_In_Progress =>
         Obj.SB_Cipher.Current_Key.Value := Key_Value;
         Obj.State := Encrypt_Current_Key_Complete;
      when Encrypt_Previous_Key_In_Progress =>
         Obj.SB_Cipher.Previous_Key.Value := Key_Value;
         Obj.State := Encrypt_Previous_Key_Complete;
      when others =>
         raise Program_Error;
      end case;

      Obj.Success := Primitive.Success (Prim);

      --  shortcut in case any primitive was unsuccessful
      if not Obj.Success then
         Obj.State := Sync_Complete;
      end if;
   end Mark_Generated_Prim_Complete_Key_Value_Ciphertext;
end CBE.Sync_Superblock;
