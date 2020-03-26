--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;
with Interfaces;
use Interfaces;

package body CBE
with SPARK_Mode
is
   function Snap_XML_Tag_Invalid (Snap_Idx : Snapshots_Index_Type)
   return String
   is (
      "<snap idx=""" &
      Debug.To_String (Debug.Uint64_Type (Snap_Idx)) &
      """/>");

   function Snap_XML_Tag_Open (
      Snap      : Snapshot_Type;
      Snap_Idx  : Snapshots_Index_Type;
      Show_Hash : Boolean)
   return String
   is (
      "<snap idx=""" &
      Debug.To_String (Debug.Uint64_Type (Snap_Idx)) &
      """ id=""" &
      Debug.To_String (Debug.Uint64_Type (Snap.ID)) &
      """ pba=""" &
      Debug.To_String (Debug.Uint64_Type (Snap.PBA)) &
      """ gen=""" &
      Debug.To_String (Debug.Uint64_Type (Snap.Gen)) &
      """ lvls=""" &
      Debug.To_String (Debug.Uint64_Type (Integer (Snap.Max_Level) + 1)) &
      """ leafs=""" &
      Debug.To_String (Debug.Uint64_Type (Snap.Nr_Of_Leafs)) &
      """ keep=""" &
      Debug.To_String (Snap.Keep) &
      (if Show_Hash then
         """ hash=""" &
         Debug.Hash_To_Hex_String (Snap.Hash) &
         """>"
       else
         """>"));

   function Snap_XML_Tag_Close
   return String
   is ("</snap>");

   function Free_Tree_XML_Tag_Open (
      SB        : Superblock_Type;
      Show_Hash : Boolean)
   return String
   is (
      "<ft pba=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Free_Number)) &
      """ gen=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Free_Gen)) &
      """ lvls=""" &
      Debug.To_String (Debug.Uint64_Type (Integer (SB.Free_Max_Level) + 1)) &
      """ degr=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Free_Degree)) &
      """ leafs=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Free_Leafs)) &
      (if Show_Hash then
         """ hash=""" &
         Debug.Hash_To_Hex_String (SB.Free_Hash) &
         """>"
       else
         """>"));

   function Free_Tree_XML_Tag_Close
   return String
   is ("</ft>");

   function Meta_Tree_XML_Tag_Open (
      SB        : Superblock_Type;
      Show_Hash : Boolean)
   return String
   is (
      "<mt pba=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Meta_Number)) &
      """ gen=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Meta_Gen)) &
      """ lvls=""" &
      Debug.To_String (Debug.Uint64_Type (Integer (SB.Meta_Max_Level) + 1)) &
      """ degr=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Meta_Degree)) &
      """ leafs=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Meta_Leafs)) &
      (if Show_Hash then
         """ hash=""" &
         Debug.Hash_To_Hex_String (SB.Meta_Hash) &
         """>"
       else
         """>"));

   function Meta_Tree_XML_Tag_Close
   return String
   is ("</mt>");

   function Superblock_XML_Tag_Open (SB : Superblock_Type)
   return String
   is (
      "<sb lsgen=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Last_Secured_Generation)) &
      """ snap=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Curr_Snap)) &
      """ degr=""" &
      Debug.To_String (Debug.Uint64_Type (SB.Degree)) &
      """>");

   function Superblock_XML_Tag_Invalid
   return String
   is ("<sb/>");

   function Superblock_XML_Tag_Close
   return String
   is ("</sb>");

   function Type_2_Node_XML_Tag (
      Node     : Type_2_Node_Type;
      Node_Idx : Type_2_Node_Block_Index_Type)
   return String
   is (
      "<t2 idx=""" &
      Debug.To_String (Debug.Uint64_Type (Node_Idx)) &
      """ pba=""" &
      Debug.To_String (Debug.Uint64_Type (Node.PBA)) &
      """ vba=""" &
      Debug.To_String (Debug.Uint64_Type (Node.Last_VBA)) &
      """ agen=""" &
      Debug.To_String (Debug.Uint64_Type (Node.Alloc_Gen)) &
      """ fgen=""" &
      Debug.To_String (Debug.Uint64_Type (Node.Free_Gen)) &
      """ key=""" &
      Debug.To_String (Debug.Uint64_Type (Node.Last_Key_ID)) &
      """ res=""" &
      Debug.To_String (Node.Reserved) &
      """/>");

   function Type_1_Node_XML_Attributes (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String
   is (
      "idx=""" &
      Debug.To_String (Debug.Uint64_Type (Node_Idx)) &
      """ pba=""" &
      Debug.To_String (Debug.Uint64_Type (Node.PBA)) &
      """ gen=""" &
      Debug.To_String (Debug.Uint64_Type (Node.Gen)) &
      " vba=""0x" &
      Debug.Hex_Image (Debug.Uint64_Type (VBA)) &
      (if Show_Hash then
         """ hash=""" &
         Debug.Hash_To_Hex_String (Node.Hash) &
         """"
       else
         """"));

   function Type_1_Node_XML_Tag (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String
   is (
      "<t1 " & Type_1_Node_XML_Attributes (Node, Node_Idx, Show_Hash, VBA) &
      "/>");

   function Type_1_Node_XML_Tag_Open (
      Node      : Type_1_Node_Type;
      Node_Idx  : Type_1_Node_Block_Index_Type;
      Show_Hash : Boolean;
      VBA       : Virtual_Block_Address_Type)
   return String
   is (
      "<t1 " & Type_1_Node_XML_Attributes (Node, Node_Idx, Show_Hash, VBA) &
      ">");

   function Type_1_Node_XML_Tag_Close
   return String
   is ("</t1>");

   --
   --  Key_Invalid
   --
   function Key_Invalid
   return Key_Type
   is
      Result : Key_Type;
   begin
      Result.Value := (others => Byte_Type'First);
      Result.ID    := Key_ID_Invalid;
      return Result;
   end Key_Invalid;

   --
   --  Key_Valid
   --
   function Key_Valid (ID : Key_ID_Type)
   return Key_Type
   is
      Result : Key_Type;
   begin
      if ID = Key_ID_Invalid then
         raise Program_Error;
      end if;
      Result.Value := (others => Byte_Type'First);
      Result.ID    := ID;
      return Result;
   end Key_Valid;

   function Superblock_Valid (SB : Superblock_Type)
   return Boolean
   is (SB.Last_Secured_Generation /= Generation_Type'Last);

   function Superblock_Invalid
   return Superblock_Type
   is
      Result : Superblock_Type;
   begin
      Result.Keys                    := (others => Key_Invalid);
      Result.Snapshots               := (others => Snapshot_Invalid);
      Result.Last_Secured_Generation := Generation_Type'Last;
      Result.Curr_Snap               := Snapshots_Index_Type'First;
      Result.Degree                  := Tree_Degree_Type'First;
      Result.Free_Number             := Physical_Block_Address_Type'First;
      Result.Free_Hash               := (others => Byte_Type'First);
      Result.Free_Max_Level          := Tree_Level_Index_Type'First;
      Result.Free_Degree             := Tree_Degree_Type'First;
      Result.Free_Leafs              := Tree_Number_Of_Leafs_Type'First;
      Result.Free_Gen                := Generation_Type'First;
      Result.Meta_Number             := Physical_Block_Address_Type'First;
      Result.Meta_Hash               := (others => Byte_Type'First);
      Result.Meta_Max_Level          := Tree_Level_Index_Type'First;
      Result.Meta_Degree             := Tree_Degree_Type'First;
      Result.Meta_Leafs              := Tree_Number_Of_Leafs_Type'First;
      Result.Meta_Gen                := Generation_Type'First;
      return Result;
   end Superblock_Invalid;

   procedure Block_Data_From_Unsigned_64 (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Int  :        Unsigned_64)
   is
   begin
      Data (Off + 7) := Byte_Type (Shift_Right (Int, 56) and 16#ff#);
      Data (Off + 6) := Byte_Type (Shift_Right (Int, 48) and 16#ff#);
      Data (Off + 5) := Byte_Type (Shift_Right (Int, 40) and 16#ff#);
      Data (Off + 4) := Byte_Type (Shift_Right (Int, 32) and 16#ff#);
      Data (Off + 3) := Byte_Type (Shift_Right (Int, 24) and 16#ff#);
      Data (Off + 2) := Byte_Type (Shift_Right (Int, 16) and 16#ff#);
      Data (Off + 1) := Byte_Type (Shift_Right (Int,  8) and 16#ff#);
      Data (Off + 0) := Byte_Type (Int                   and 16#ff#);
   end Block_Data_From_Unsigned_64;

   procedure Block_Data_From_Unsigned_32 (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Int  :        Unsigned_32)
   is
   begin
      Data (Off + 3) := Byte_Type (Shift_Right (Int, 24) and 16#ff#);
      Data (Off + 2) := Byte_Type (Shift_Right (Int, 16) and 16#ff#);
      Data (Off + 1) := Byte_Type (Shift_Right (Int,  8) and 16#ff#);
      Data (Off + 0) := Byte_Type (Int                   and 16#ff#);
   end Block_Data_From_Unsigned_32;

   procedure Block_Data_From_Boolean (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Bool :        Boolean)
   is
   begin
      Data (Off) := (if Bool then 1 else 0);
   end Block_Data_From_Boolean;

   procedure Block_Data_Zero_Fill (
      Data : in out Block_Data_Type;
      Off  :        Block_Data_Index_Type;
      Size :        Block_Data_Index_Type)
   is
   begin
      for Off_2 in 0 .. Size - 1 loop
         Data (Off + Off_2) := 0;
      end loop;
   end Block_Data_Zero_Fill;

   procedure Block_Data_From_Hash (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Hash   :        Hash_Type)
   is
      Off : Block_Data_Index_Type := Off_In;
   begin
      for Idx in Hash'Range loop
         Data (Off) := Hash (Idx);
         Off := Off + 1;
      end loop;
   end Block_Data_From_Hash;

   procedure Block_Data_From_Type_1_Node (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Node   :        Type_1_Node_Type)
   is
      Off : Block_Data_Index_Type := Off_In;
   begin
      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.PBA));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.Gen));
      Off := Off + 8;

      Block_Data_From_Hash (Data, Off, Node.Hash);
   end Block_Data_From_Type_1_Node;

   procedure Block_Data_From_Type_1_Node_Block (
      Data  : out Block_Data_Type;
      Nodes :     Type_1_Node_Block_Type)
   is
      Off : Block_Data_Index_Type;
   begin
      Data := (others => 0);
      for Idx in Nodes'Range loop
         Off := Block_Data_Index_Type (Idx * Type_1_Node_Storage_Size_Bytes);
         Block_Data_From_Type_1_Node (Data, Off, Nodes (Idx));
      end loop;
   end Block_Data_From_Type_1_Node_Block;

   procedure Block_Data_From_Type_2_Node (
      Data   : in out Block_Data_Type;
      Off_In :        Block_Data_Index_Type;
      Node   :        Type_2_Node_Type)
   is
      Off : Block_Data_Index_Type := Off_In;
   begin
      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.PBA));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.Last_VBA));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.Alloc_Gen));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (Node.Free_Gen));
      Off := Off + 8;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (Node.Last_Key_ID));
      Off := Off + 4;

      Block_Data_From_Boolean (Data, Off, Node.Reserved);
   end Block_Data_From_Type_2_Node;

   procedure Block_Data_From_Type_2_Node_Block (
      Data  : out Block_Data_Type;
      Nodes :     Type_2_Node_Block_Type)
   is
      Off : Block_Data_Index_Type;
   begin
      Data := (others => 0);
      for Idx in Nodes'Range loop
         Off := Block_Data_Index_Type (Idx * Type_2_Node_Storage_Size_Bytes);
         Block_Data_From_Type_2_Node (Data, Off, Nodes (Idx));
      end loop;
   end Block_Data_From_Type_2_Node_Block;

   function Boolean_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Boolean
   is (
      if Data (Off + 0) = 1 then True else False);

   function Unsigned_32_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Unsigned_32
   is (
      Shift_Left (Unsigned_32 (Data (Off + 3)), 24) +
      Shift_Left (Unsigned_32 (Data (Off + 2)), 16) +
      Shift_Left (Unsigned_32 (Data (Off + 1)),  8) +
                  Unsigned_32 (Data (Off + 0)));

   function Unsigned_64_From_Block_Data (
      Data : Block_Data_Type;
      Off  : Block_Data_Index_Type)
   return Unsigned_64
   is (
      Shift_Left (Unsigned_64 (Data (Off + 7)), 56) +
      Shift_Left (Unsigned_64 (Data (Off + 6)), 48) +
      Shift_Left (Unsigned_64 (Data (Off + 5)), 40) +
      Shift_Left (Unsigned_64 (Data (Off + 4)), 32) +
      Shift_Left (Unsigned_64 (Data (Off + 3)), 24) +
      Shift_Left (Unsigned_64 (Data (Off + 2)), 16) +
      Shift_Left (Unsigned_64 (Data (Off + 1)),  8) +
                  Unsigned_64 (Data (Off + 0)));

   function Hash_From_Block_Data (
      Data : Block_Data_Type;
      Base : Block_Data_Index_Type)
   return Hash_Type
   is
      Result : Hash_Type;
      Off    : Block_Data_Index_Type := 0;
   begin
      for Idx in Result'Range loop
         Result (Idx) := Data (Base + Off);
         Off := Off + 1;
      end loop;
      return Result;
   end Hash_From_Block_Data;

   procedure Key_From_Block_Data (
      Key      : out Key_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type)
   is
      Key_Off : Block_Data_Index_Type := Data_Off;
   begin
      Declare_Value_Off : declare
         Value_Off : Block_Data_Index_Type;
      begin
         For_Value_Items : for Idx in Key.Value'Range loop
            Value_Off := Key_Off + Block_Data_Index_Type (Idx);
            Key.Value (Idx) := Data (Value_Off);
         end loop For_Value_Items;
      end Declare_Value_Off;
      Key_Off := Key_Off + Key_Value_Size_Bytes;
      Key.ID := Key_ID_Type (Unsigned_32_From_Block_Data (Data, Key_Off));
   end Key_From_Block_Data;

   procedure Keys_From_Block_Data (
      Keys     : out Keys_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type)
   is
      Keys_Off : Block_Data_Index_Type;
   begin
      For_Keys : for Idx in Keys'Range loop
         Keys_Off :=
           Data_Off + Block_Data_Index_Type (Idx * Key_Storage_Size_Bytes);

         Key_From_Block_Data (Keys (Idx), Data, Keys_Off);
      end loop For_Keys;
   end Keys_From_Block_Data;

   procedure Snapshot_From_Block_Data (
      Snap     : out Snapshot_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type)
   is
      Snap_Off : Block_Data_Index_Type := Data_Off;
   begin
      Snap.Hash := Hash_From_Block_Data (Data, Snap_Off);
      Snap_Off := Snap_Off + Hash_Size_Bytes;

      Snap.PBA := Physical_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Snap_Off));
      Snap_Off := Snap_Off + 8;

      Snap.Gen := Generation_Type (
         Unsigned_64_From_Block_Data (Data, Snap_Off));
      Snap_Off := Snap_Off + 8;

      Snap.Nr_Of_Leafs := Tree_Number_Of_Leafs_Type (
         Unsigned_64_From_Block_Data (Data, Snap_Off));
      Snap_Off := Snap_Off + 8;

      Snap.Max_Level := Tree_Level_Index_Type (
         Unsigned_32_From_Block_Data (Data, Snap_Off));
      Snap_Off := Snap_Off + 4;

      Snap.Valid := Boolean_From_Block_Data (Data, Snap_Off);
      Snap_Off := Snap_Off + 1;

      Snap.ID := Snapshot_ID_Type (
         Unsigned_32_From_Block_Data (Data, Snap_Off));
      Snap_Off := Snap_Off + 4;

      Snap.Keep := (
         if (Unsigned_8 (Data (Snap_Off)) and 1) = 1 then True else False);
   end Snapshot_From_Block_Data;

   procedure Snapshots_From_Block_Data (
      Snaps    : out Snapshots_Type;
      Data     :     Block_Data_Type;
      Data_Off :     Block_Data_Index_Type)
   is
      Snaps_Off : Block_Data_Index_Type;
   begin
      For_Snaps : for Idx in Snaps'Range loop
         Snaps_Off :=
           Data_Off +
           Block_Data_Index_Type (Idx * Snapshot_Storage_Size_Bytes);

         Snapshot_From_Block_Data (Snaps (Idx), Data, Snaps_Off);
      end loop For_Snaps;
   end Snapshots_From_Block_Data;

   procedure Superblock_From_Block_Data (
      SB   : out Superblock_Type;
      Data :     Block_Data_Type)
   is
      Off : Block_Data_Index_Type := 0;
   begin
      Keys_From_Block_Data (SB.Keys, Data, Off);
      Off := Off + Superblock_Keys_Storage_Size_Bytes;

      Snapshots_From_Block_Data (SB.Snapshots, Data, Off);
      Off := Off + Superblock_Snapshots_Storage_Size_Bytes;

      SB.Last_Secured_Generation :=
         Generation_Type (Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Curr_Snap :=
         Snapshots_Index_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Degree :=
         Tree_Degree_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Free_Gen :=
         Generation_Type (Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Free_Number := Physical_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Free_Hash := Hash_From_Block_Data (Data, Off);
      Off := Off + Hash_Size_Bytes;

      SB.Free_Max_Level :=
         Tree_Level_Index_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Free_Degree :=
         Tree_Degree_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Free_Leafs :=
         Tree_Number_Of_Leafs_Type (Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Meta_Gen :=
         Generation_Type (Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Meta_Number := Physical_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      SB.Meta_Hash := Hash_From_Block_Data (Data, Off);
      Off := Off + Hash_Size_Bytes;

      SB.Meta_Max_Level :=
         Tree_Level_Index_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Meta_Degree :=
         Tree_Degree_Type (Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      SB.Meta_Leafs :=
         Tree_Number_Of_Leafs_Type (Unsigned_64_From_Block_Data (Data, Off));
   end Superblock_From_Block_Data;

   function Type_2_Node_From_Block_Data (
      Data   : Block_Data_Type;
      Off_In : Block_Data_Index_Type)
   return Type_2_Node_Type;

   function Type_2_Node_From_Block_Data (
      Data   : Block_Data_Type;
      Off_In : Block_Data_Index_Type)
   return Type_2_Node_Type
   is
      Node : Type_2_Node_Type;
      Off  : Block_Data_Index_Type := Off_In;
   begin
      Node.PBA := Physical_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Last_VBA := Virtual_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Alloc_Gen := Generation_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Free_Gen := Generation_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Last_Key_ID := Key_ID_Type (
         Unsigned_32_From_Block_Data (Data, Off));
      Off := Off + 4;

      Node.Reserved := Boolean_From_Block_Data (Data, Off);
      return Node;
   end Type_2_Node_From_Block_Data;

   function Type_1_Node_From_Block_Data (
      Data   : Block_Data_Type;
      Off_In : Block_Data_Index_Type)
   return Type_1_Node_Type;

   function Type_1_Node_From_Block_Data (
      Data   : Block_Data_Type;
      Off_In : Block_Data_Index_Type)
   return Type_1_Node_Type
   is
      Node : Type_1_Node_Type;
      Off  : Block_Data_Index_Type := Off_In;
   begin
      Node.PBA := Physical_Block_Address_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Gen := Generation_Type (
         Unsigned_64_From_Block_Data (Data, Off));
      Off := Off + 8;

      Node.Hash := Hash_From_Block_Data (Data, Off);
      return Node;
   end Type_1_Node_From_Block_Data;

   procedure Type_2_Node_Block_From_Block_Data (
      Nodes : out Type_2_Node_Block_Type;
      Data  :     Block_Data_Type)
   is
      Off : Block_Data_Index_Type;
   begin
      For_Nodes :
      for Idx in Nodes'Range loop
         Off := Block_Data_Index_Type (Idx * Type_2_Node_Storage_Size_Bytes);
         Nodes (Idx) := Type_2_Node_From_Block_Data (Data, Off);
      end loop For_Nodes;
   end Type_2_Node_Block_From_Block_Data;

   procedure Type_1_Node_Block_From_Block_Data (
      Nodes : out Type_1_Node_Block_Type;
      Data  :     Block_Data_Type)
   is
      Off : Block_Data_Index_Type;
   begin
      For_Nodes :
      for Idx in Nodes'Range loop
         Off := Block_Data_Index_Type (Idx * Type_1_Node_Storage_Size_Bytes);
         Nodes (Idx) := Type_1_Node_From_Block_Data (Data, Off);
      end loop For_Nodes;
   end Type_1_Node_Block_From_Block_Data;

   procedure Block_Data_From_Key (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Key      :        Key_Type)
   is
      Key_Off : Block_Data_Index_Type := Data_Off;
      Value_Off : Block_Data_Index_Type;
   begin
      For_Value_Items : for Idx in Key.Value'Range loop
         Value_Off := Key_Off + Block_Data_Index_Type (Idx);
         Data (Value_Off) := Key.Value (Idx);
      end loop For_Value_Items;
      Key_Off := Key_Off + Key_Value_Size_Bytes;

      Block_Data_From_Unsigned_32 (Data, Key_Off, Unsigned_32 (Key.ID));
   end Block_Data_From_Key;

   procedure Block_Data_From_Snapshot (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Snap     :        Snapshot_Type)
   is
      Snap_Off : Block_Data_Index_Type := Data_Off;
   begin
      Block_Data_From_Hash (Data, Snap_Off, Snap.Hash);
      Snap_Off := Snap_Off + Hash_Size_Bytes;

      Block_Data_From_Unsigned_64 (Data, Snap_Off, Unsigned_64 (Snap.PBA));
      Snap_Off := Snap_Off + 8;

      Block_Data_From_Unsigned_64 (Data, Snap_Off, Unsigned_64 (Snap.Gen));
      Snap_Off := Snap_Off + 8;

      Block_Data_From_Unsigned_64 (
         Data, Snap_Off, Unsigned_64 (Snap.Nr_Of_Leafs));
      Snap_Off := Snap_Off + 8;

      Block_Data_From_Unsigned_32 (
         Data, Snap_Off, Unsigned_32 (Snap.Max_Level));
      Snap_Off := Snap_Off + 4;

      Block_Data_From_Boolean (Data, Snap_Off, Snap.Valid);
      Snap_Off := Snap_Off + 1;

      Block_Data_From_Unsigned_32 (Data, Snap_Off, Unsigned_32 (Snap.ID));
      Snap_Off := Snap_Off + 4;

      Block_Data_From_Unsigned_32 (
         Data, Snap_Off, Unsigned_32 (if Snap.Keep then 1 else 0));
   end Block_Data_From_Snapshot;

   procedure Block_Data_From_Keys (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Keys     :        Keys_Type)
   is
      Keys_Off : Block_Data_Index_Type;
   begin
      For_Keys : for Idx in Keys'Range loop
         Keys_Off :=
           Data_Off + Block_Data_Index_Type (Idx * Key_Storage_Size_Bytes);

         Block_Data_From_Key (Data, Keys_Off, Keys (Idx));
      end loop For_Keys;
   end Block_Data_From_Keys;

   procedure Block_Data_From_Snapshots (
      Data     : in out Block_Data_Type;
      Data_Off :        Block_Data_Index_Type;
      Snaps    :        Snapshots_Type)
   is
      Snaps_Off : Block_Data_Index_Type;
   begin
      For_Snaps : for Idx in Snaps'Range loop
         Snaps_Off :=
           Data_Off +
           Block_Data_Index_Type (Idx * Snapshot_Storage_Size_Bytes);

         Block_Data_From_Snapshot (Data, Snaps_Off, Snaps (Idx));
      end loop For_Snaps;
   end Block_Data_From_Snapshots;

   procedure Block_Data_From_Superblock (
      Data  : out Block_Data_Type;
      SB    :     Superblock_Type)
   is
      Off : Block_Data_Index_Type := 0;
   begin
      Data := (others => 0);

      Block_Data_From_Keys (Data, Off, SB.Keys);
      Off := Off + Superblock_Keys_Storage_Size_Bytes;

      Block_Data_From_Snapshots (Data, Off, SB.Snapshots);
      Off := Off + Superblock_Snapshots_Storage_Size_Bytes;

      Block_Data_From_Unsigned_64 (
         Data, Off, Unsigned_64 (SB.Last_Secured_Generation));
      Off := Off + 8;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Curr_Snap));
      Off := Off + 4;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Degree));
      Off := Off + 4;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Free_Gen));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Free_Number));
      Off := Off + 8;

      Block_Data_From_Hash (Data, Off, SB.Free_Hash);
      Off := Off + Hash_Size_Bytes;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Free_Max_Level));
      Off := Off + 4;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Free_Degree));
      Off := Off + 4;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Free_Leafs));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Meta_Gen));
      Off := Off + 8;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Meta_Number));
      Off := Off + 8;

      Block_Data_From_Hash (Data, Off, SB.Meta_Hash);
      Off := Off + Hash_Size_Bytes;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Meta_Max_Level));
      Off := Off + 4;

      Block_Data_From_Unsigned_32 (Data, Off, Unsigned_32 (SB.Meta_Degree));
      Off := Off + 4;

      Block_Data_From_Unsigned_64 (Data, Off, Unsigned_64 (SB.Meta_Leafs));
   end Block_Data_From_Superblock;

   function Pool_Idx_Slot_Valid (Cont : Pool_Index_Type)
   return Pool_Index_Slot_Type
   is (
      Valid   => True,
      Content => Cont);

   function Pool_Idx_Slot_Invalid
   return Pool_Index_Slot_Type
   is (
      Valid   => False,
      Content => Pool_Index_Type'First);

   function Pool_Idx_Slot_Valid (Slot : Pool_Index_Slot_Type)
   return Boolean
   is (Slot.Valid);

   function Pool_Idx_Slot_Content (Slot : Pool_Index_Slot_Type)
   return Pool_Index_Type
   is
   begin
      if not Slot.Valid then
         raise Program_Error;
      end if;
      return Slot.Content;
   end Pool_Idx_Slot_Content;

   function To_String (Pool_Idx_Slot : Pool_Index_Slot_Type)
   return String
   is (
      if Pool_Idx_Slot_Valid (Pool_Idx_Slot) then
         Debug.To_String (
            Debug.Uint64_Type (Pool_Idx_Slot_Content (Pool_Idx_Slot)))
      else
         "<Invalid>");

   function Idx_Slot_Valid (Cont : Index_Type)
   return Index_Slot_Type
   is (
      Valid   => True,
      Content => Cont);

   function Idx_Slot_Invalid
   return Index_Slot_Type
   is (
      Valid   => False,
      Content => Index_Type'First);

   function Idx_Slot_Valid (Slot : Index_Slot_Type)
   return Boolean
   is (Slot.Valid);

   function Idx_Slot_Content (Slot : Index_Slot_Type)
   return Index_Type
   is
   begin
      if not Slot.Valid then
         raise Program_Error;
      end if;
      return Slot.Content;
   end Idx_Slot_Content;

   function Advance_Index (I : T)
   return T
   is
   begin
      if I < T'Last then
         return T'Succ (I);
      else
         return T'First;
      end if;
   end Advance_Index;

   --
   --  Prim_Op_From_Req_Op
   --
   function Prim_Op_From_Req_Op (Input : Request_Operation_Type)
   return Primitive_Operation_Type
   is
   begin
      case Input is
      when Read => return Read;
      when Write => return Write;
      when Sync => return Sync;
      when others => raise Program_Error;
      end case;
   end Prim_Op_From_Req_Op;

end CBE;
