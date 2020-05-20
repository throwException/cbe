--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package CBE.CXX
with SPARK_Mode
is
   pragma Pure;

   type CXX_UInt8_Type  is range 0 .. 2**8  - 1 with Size => 1 * 8;
   type CXX_UInt32_Type is range 0 .. 2**32 - 1 with Size => 4 * 8;
   type CXX_UInt64_Type is mod        2**64     with Size => 8 * 8;

   type CXX_Bool_Type                 is new CXX_UInt8_Type;
   type CXX_Operation_Type            is new CXX_UInt32_Type;
   type CXX_Success_Type              is new CXX_UInt32_Type;
   type CXX_Object_Size_Type          is new CXX_UInt32_Type;
   type CXX_Block_Number_Type         is new CXX_UInt64_Type;
   type CXX_Timestamp_Type            is new CXX_UInt64_Type;
   type CXX_Tag_Type                  is new CXX_UInt32_Type;
   type CXX_Block_Offset_Type         is new CXX_UInt64_Type;
   type CXX_Block_Count_Type          is new CXX_UInt32_Type;
   type CXX_Primitive_Index_Type      is new CXX_UInt64_Type;
   type CXX_Snapshot_ID_Type          is new CXX_UInt64_Type;
   type CXX_Token_Type                is new CXX_UInt64_Type;
   type CXX_Key_ID_Type               is new CXX_UInt32_Type;
   type CXX_Tree_Level_Index_Type     is new CXX_UInt64_Type;
   type CXX_Tree_Degree_Type          is new CXX_UInt64_Type;
   type CXX_Tree_Number_Of_Leafs_Type is new CXX_UInt64_Type;

   type CXX_Superblocks_Type
   is array (Superblocks_Index_Type) of Block_Data_Type;

   type CXX_Key_Value_Type is array (Key_Value_Index_Type) of CXX_UInt8_Type;

   type CXX_Key_Type is record
      Value : CXX_Key_Value_Type;
      ID    : CXX_Key_ID_Type;
   end record;

   type CXX_Superblocks_Index_Type is record
      Value : CXX_UInt64_Type;
   end record;

   type CXX_IO_Buffer_Index_Type is record
      Value : CXX_UInt32_Type;
   end record;
   pragma Pack (CXX_IO_Buffer_Index_Type);

   type CXX_Crypto_Plain_Buffer_Index_Type is record
      Value : CXX_UInt32_Type;
   end record;
   pragma Pack (CXX_Crypto_Plain_Buffer_Index_Type);

   type CXX_Crypto_Cipher_Buffer_Index_Type is record
      Value : CXX_UInt32_Type;
   end record;
   pragma Pack (CXX_Crypto_Cipher_Buffer_Index_Type);

   type CXX_Dump_Configuration_Type is record
      Unused_Nodes           : CXX_Bool_Type;
      Max_Superblocks        : CXX_UInt32_Type;
      Max_Snapshots          : CXX_UInt32_Type;
      VBD                    : CXX_Bool_Type;
      VBD_PBA_Filter_Enabled : CXX_Bool_Type;
      VBD_PBA_Filter         : CXX_UInt64_Type;
      VBD_VBA_Filter_Enabled : CXX_Bool_Type;
      VBD_VBA_Filter         : CXX_UInt64_Type;
      Free_Tree              : CXX_Bool_Type;
      Meta_Tree              : CXX_Bool_Type;
      Hashes                 : CXX_Bool_Type;
   end record;
   pragma Pack (CXX_Dump_Configuration_Type);

   type CXX_Request_Type is record
      Operation    : CXX_Operation_Type;
      Success      : CXX_Success_Type;
      Block_Number : CXX_Block_Number_Type;
      Offset       : CXX_Block_Offset_Type;
      Count        : CXX_Block_Count_Type;
      Key_ID       : CXX_Key_ID_Type;
      Tag          : CXX_Tag_Type;
   end record;
   pragma Pack (CXX_Request_Type);

   function CXX_Bool_From_SPARK (Input : Boolean)
   return CXX_Bool_Type
   is (
      case Input is
      when False => 0,
      when True  => 1);

   --
   --  CXX_Bool_To_SPARK
   --
   function CXX_Bool_To_SPARK (Input : CXX_Bool_Type)
   return Boolean;

   function CXX_Success_From_SPARK (Input : Request.Success_Type)
   return CXX_Success_Type
   is (
      case Input is
      when False => 0,
      when True  => 1);

   --
   --  CXX_Success_To_SPARK
   --
   function CXX_Success_To_SPARK (Input : CXX_Success_Type)
   return Request.Success_Type;

   function CXX_Operation_From_SPARK (Input : Request_Operation_Type)
   return CXX_Operation_Type
   is (
      case Input is
      when Read             => 1,
      when Write            => 2,
      when Sync             => 3,
      when Create_Snapshot  => 4,
      when Discard_Snapshot => 5,
      when Rekey            => 6,
      when Extend_VBD       => 7,
      when Extend_FT        => 8);

   function CXX_Request_Valid_To_SPARK (
      Req : CXX_Request_Type;
      Op  : Request_Operation_Type)
   return Request.Object_Type
   is (
      Request.Valid_Object (
         Op,
         CXX_Success_To_SPARK (Req.Success),
         Block_Number_Type    (Req.Block_Number),
         Request.Offset_Type  (Req.Offset),
         Request.Count_Type   (Req.Count),
         Key_ID_Type          (Req.Key_ID),
         Request.Tag_Type     (Req.Tag)));

   --
   --  CXX_Request_To_SPARK
   --
   function CXX_Request_To_SPARK (Input : CXX_Request_Type)
   return Request.Object_Type;

   function CXX_Request_From_SPARK (Obj : Request.Object_Type)
   return CXX_Request_Type
   is (
      case Request.Valid (Obj) is
      when True => (
         Operation    => CXX_Operation_From_SPARK (Request.Operation (Obj)),
         Success      => CXX_Success_From_SPARK   (Request.Success (Obj)),
         Block_Number => CXX_Block_Number_Type    (Request.Block_Number (Obj)),
         Offset       => CXX_Block_Offset_Type    (Request.Offset (Obj)),
         Count        => CXX_Block_Count_Type     (Request.Count (Obj)),
         Key_ID       => CXX_Key_ID_Type          (Request.Key_ID (Obj)),
         Tag          => CXX_Tag_Type             (Request.Tag (Obj))),
      when False => (
         Operation    => 0,
         Success      => 0,
         Block_Number => 0,
         Offset       => 0,
         Count        => 0,
         Key_ID       => 0,
         Tag          => 0));

   function CXX_Dump_Configuration_To_SPARK (Cfg : CXX_Dump_Configuration_Type)
   return Dump_Configuration_Type
   is (
      Unused_Nodes => CXX_Bool_To_SPARK (Cfg.Unused_Nodes),
      Max_Superblocks => Dump_Cfg_Max_Superblocks_Type (Cfg.Max_Superblocks),
      Max_Snapshots => Dump_Cfg_Max_Snapshots_Type (Cfg.Max_Snapshots),
      VBD => CXX_Bool_To_SPARK (Cfg.VBD),
      VBD_PBA_Filter_Enabled => CXX_Bool_To_SPARK (Cfg.VBD_PBA_Filter_Enabled),
      VBD_PBA_Filter => Physical_Block_Address_Type (Cfg.VBD_PBA_Filter),
      VBD_VBA_Filter_Enabled => CXX_Bool_To_SPARK (Cfg.VBD_VBA_Filter_Enabled),
      VBD_VBA_Filter => Virtual_Block_Address_Type (Cfg.VBD_VBA_Filter),
      Free_Tree => CXX_Bool_To_SPARK (Cfg.Free_Tree),
      Meta_Tree => CXX_Bool_To_SPARK (Cfg.Meta_Tree),
      Hashes => CXX_Bool_To_SPARK (Cfg.Hashes));

   --
   --  CXX_Key_From_SPARK
   --
   function CXX_Key_From_SPARK (SPARK_Key : Key_Plaintext_Type)
   return CXX_Key_Type;

end CBE.CXX;
