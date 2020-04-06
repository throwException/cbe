--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Debug
with SPARK_Mode => Off
is
   procedure Print_String (Str : String)
   is
   begin
      Print_Cstring (Str, Str'Length);
   end Print_String;

   procedure Print_String_Buffered (Str : String)
   is
   begin
      Print_Cstring_Buffered (Str, Str'Length);
   end Print_String_Buffered;

   --  XXX based on ada-runtime/src/minimal/a-except.adb
   function Image (Int : Uint64_Type)
   return String
   is
      Str        : String (1 .. 24) := (others => '_');
      Int_Remain : Uint64_Type := Int;
   begin
      for Idx in reverse Str'First + 1 .. Str'Last loop
         Str (Idx) := Character'Val (48 + (Int_Remain rem 10));
         Int_Remain := Int_Remain / 10;
         if Int_Remain = 0 then
            return Str (Idx .. Str'Last);
         end if;
      end loop;
      return Str;
   end Image;

   function Hex_Image (Int : Uint64_Type)
   return String
   is
      Str        : String (1 .. 16) := (others => '_');
      Int_Remain : Uint64_Type := Int;
   begin
      for Idx in reverse Str'First + 1 .. Str'Last loop
         declare
            Int_Curr : constant Uint64_Type := Int_Remain rem 16;
         begin
            if Int_Curr > 9 then
               Str (Idx) := Character'Val (87 + Int_Curr);
            else
               Str (Idx) := Character'Val (48 + Int_Curr);
            end if;
         end;
         Int_Remain := Int_Remain / 16;
         if Int_Remain = 0 then
            return Str (Idx .. Str'Last);
         end if;
      end loop;
      return Str;
   end Hex_Image;

   function To_String (Int : Uint64_Type)
   return String
   is (
      Image (Int));

   function To_String (Bool : Boolean)
   return String
   is (
      if Bool then "True" else "False");

   function To_String (PBA : Physical_Block_Address_Type)
   return String
   is (
      if PBA = Physical_Block_Address_Type'Last then
         "Invalid"
      else
         To_String (Uint64_Type (PBA)));

   function To_String (VBA : Virtual_Block_Address_Type)
   return String
   is (
      if VBA = Virtual_Block_Address_Type'Last then
         "Invalid"
      else
         To_String (Uint64_Type (VBA)));

   function To_String (G : Generation_Type)
   return String
   is (To_String (Uint64_Type (G)));

   function To_String (Blk : Block_Data_Type)
   return String
   is (
      "Block_Data (" &
      To_String (Uint64_Type (Blk (1))) & ", " &
      To_String (Uint64_Type (Blk (2))) & ", " &
      To_String (Uint64_Type (Blk (3))) & ", " &
      To_String (Uint64_Type (Blk (4))) & ", " &
      To_String (Uint64_Type (Blk (5))) & ", " &
      "...)");

   function To_String (H : Hash_Type)
   return String
   is (
      "HASH (" &
      To_String (Uint64_Type (H (1))) & "," &
      To_String (Uint64_Type (H (2))) & "," &
      To_String (Uint64_Type (H (3))) & "," &
      To_String (Uint64_Type (H (4))) & "," &
      To_String (Uint64_Type (H (5))) & "," &
      To_String (Uint64_Type (H (6))) & "," &
      To_String (Uint64_Type (H (7))) & "," &
      To_String (Uint64_Type (H (8))) & "," &
      "...)");

   function Byte_To_Hex_String (Byte : Byte_Type)
   return String
   is
      Str : String (1 .. 2);
      X : constant Byte_Type := Byte rem 16;
      Y : constant Byte_Type := (Byte / 16) rem 16;
   begin
      if Y > 9 then
         Str (1) := Character'Val (87 + Y);
      else
         Str (1) := Character'Val (48 + Y);
      end if;
      if X > 9 then
         Str (2) := Character'Val (87 + X);
      else
         Str (2) := Character'Val (48 + X);
      end if;
      return Str;
   end Byte_To_Hex_String;

   function Hash_To_Hex_String (Hash : Hash_Type)
   return String
   is (
      "0x" &
      Byte_To_Hex_String (Hash (0)) &
      Byte_To_Hex_String (Hash (1)) &
      Byte_To_Hex_String (Hash (2)) &
      Byte_To_Hex_String (Hash (3)) &
      Byte_To_Hex_String (Hash (4)) &
      Byte_To_Hex_String (Hash (5)) &
      Byte_To_Hex_String (Hash (6)) &
      Byte_To_Hex_String (Hash (7)) &
      Byte_To_Hex_String (Hash (8)) &
      Byte_To_Hex_String (Hash (9)) &
      Byte_To_Hex_String (Hash (10)) &
      Byte_To_Hex_String (Hash (11)) &
      Byte_To_Hex_String (Hash (12)) &
      Byte_To_Hex_String (Hash (13)) &
      Byte_To_Hex_String (Hash (14)) &
      Byte_To_Hex_String (Hash (15)) &
      Byte_To_Hex_String (Hash (16)) &
      Byte_To_Hex_String (Hash (17)) &
      Byte_To_Hex_String (Hash (18)) &
      Byte_To_Hex_String (Hash (19)) &
      Byte_To_Hex_String (Hash (20)) &
      Byte_To_Hex_String (Hash (21)) &
      Byte_To_Hex_String (Hash (22)) &
      Byte_To_Hex_String (Hash (23)) &
      Byte_To_Hex_String (Hash (24)) &
      Byte_To_Hex_String (Hash (25)) &
      Byte_To_Hex_String (Hash (26)) &
      Byte_To_Hex_String (Hash (27)) &
      Byte_To_Hex_String (Hash (28)) &
      Byte_To_Hex_String (Hash (29)) &
      Byte_To_Hex_String (Hash (30)) &
      Byte_To_Hex_String (Hash (31)));

   procedure Dump_Superblock (
      SB_Index : Superblocks_Index_Type;
      SB       : Superblock_Type)
   is
   begin
      Debug.Print_String (
         "Dump SB: Slot: " &
         Debug.To_String (Debug.Uint64_Type (SB_Index)) &
         " SN: " &
         Debug.To_String (Debug.Uint64_Type (SB.Curr_Snap)) &
         " LSGEN: " &
         Debug.To_String (Debug.Uint64_Type (SB.Last_Secured_Generation)) &
         " state: " &
         SB.State'Image);

      for I in Snapshots_Index_Type loop
         if SB.Snapshots (I).Valid then
            Debug.Print_String ("SB: "
               & Debug.To_String (Debug.Uint64_Type (SB_Index))
               & " SN: "
               & Debug.To_String (Debug.Uint64_Type (I))
               & " PBA: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).PBA))
               & " GEN: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).Gen))
               & " ID: "
               & Debug.To_String (Debug.Uint64_Type (SB.Snapshots (I).ID))
               & " KEEP: "
               & Debug.To_String (SB.Snapshots (I).Keep)
               & " "
               & Debug.Hash_To_Hex_String (SB.Snapshots (I).Hash));
         end if;
      end loop;
   end Dump_Superblock;

end CBE.Debug;
