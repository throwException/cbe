--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request_Pool;
with CBE.Crypto;
with CBE.Block_IO;
with CBE.Request;
with CBE.Cache;
with CBE.New_Free_Tree;
with CBE.Meta_Tree;
with CBE.Superblock_Control;
with CBE.Trust_Anchor;
with CBE.VBD_Rekeying;
with CBE.FT_Resizing;
with CBE.MT_Resizing;
with CBE.TA_Request;

package CBE.Library
with SPARK_Mode
is
   --  FIXME cannot be pure yet because of CBE.Crypto
   --  pragma Pure;

   type Object_Type is private;

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Active_Snapshot_IDs
   --
   procedure Active_Snapshot_IDs (
      Obj  :     Object_Type;
      List : out Active_Snapshot_IDs_Type);

   --
   --  Info
   --
   procedure Info (
      Obj  :     Object_Type;
      Info : out Info_Type);

   --
   --  Client_Request_Acceptable
   --
   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Client_Request
   --
   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      ID  :        Snapshot_ID_Type);

   --
   --  Peek_Completed_Client_Request
   --
   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type;

   --
   --  Drop_Completed_Client_Request
   --
   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Has_IO_Request
   --
   procedure Has_IO_Request (
      Obj      :     Object_Type;
      Req      : out Request.Object_Type;
      Data_Idx : out Block_IO.Data_Index_Type);

   --
   --  IO_Request_In_Progress
   --
   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type);

   --
   --  IO_Request_Completed
   --
   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean);

   --
   --  Client_Transfer_Read_Data_Required
   --
   procedure Client_Transfer_Read_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Read_Data_In_Progress
   --
   procedure Client_Transfer_Read_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Read_Data_Completed
   --
   procedure Client_Transfer_Read_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean);

   --
   --  Client_Transfer_Write_Data_Required
   --
   procedure Client_Transfer_Write_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Write_Data_In_Progress
   --
   procedure Client_Transfer_Write_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Write_Data_Completed
   --
   procedure Client_Transfer_Write_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean);

   --
   --  Crypto_Add_Key_Required
   --
   procedure Crypto_Add_Key_Required (
      Obj :     Object_Type;
      Req : out Request.Object_Type;
      Key : out Key_Plaintext_Type);

   --
   --  Crypto_Add_Key_Requested
   --
   procedure Crypto_Add_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Add_Key_Completed
   --
   procedure Crypto_Add_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Remove_Key_Required
   --
   procedure Crypto_Remove_Key_Required (
      Obj    :     Object_Type;
      Req    : out Request.Object_Type;
      Key_ID : out Key_ID_Type);

   --
   --  Crypto_Remove_Key_Requested
   --
   procedure Crypto_Remove_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Remove_Key_Completed
   --
   procedure Crypto_Remove_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Cipher_Data_Required
   --
   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Crypto_Cipher_Data_Requested
   --
   procedure Crypto_Cipher_Data_Requested (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Supply_Crypto_Cipher_Data
   --
   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Crypto_Plain_Data_Required
   --
   procedure Crypto_Plain_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Cipher_Buffer_Index_Type);

   --
   --  Crypto_Plain_Data_Requested
   --
   procedure Crypto_Plain_Data_Requested (
      Obj            : in out Library.Object_Type;
      Cipher_Buf_Idx :        Crypto.Cipher_Buffer_Index_Type);

   --
   --  Supply_Crypto_Plain_Data
   --
   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Peek_Generated_TA_Request
   --
   procedure Peek_Generated_TA_Request (
      Obj :     Object_Type;
      Req : out TA_Request.Object_Type);

   --
   --  Drop_Generated_TA_Request
   --
   procedure Drop_Generated_TA_Request (
      Obj : in out Object_Type;
      Req :        TA_Request.Object_Type);

   --
   --  Peek_Generated_TA_SB_Hash
   --
   procedure Peek_Generated_TA_SB_Hash (
      Obj  :     Object_Type;
      Req  :     TA_Request.Object_Type;
      Hash : out Hash_Type);

   --
   --  Peek_Generated_TA_Key_Cipher
   --
   procedure Peek_Generated_TA_Key_Cipher (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Ciphertext_Type);

   --
   --  Peek_Generated_TA_Key_Plain
   --
   procedure Peek_Generated_TA_Key_Plain (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Plaintext_Type);

   --
   --  Mark_Generated_TA_Create_Key_Request_Complete
   --
   procedure Mark_Generated_TA_Create_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark_Generated_TA_Secure_SB_Request_Complete
   --
   procedure Mark_Generated_TA_Secure_SB_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type);

   --
   --  Mark_Generated_TA_Decrypt_Key_Request_Complete
   --
   procedure Mark_Generated_TA_Decrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark_Generated_TA_Encrypt_Key_Request_Complete
   --
   procedure Mark_Generated_TA_Encrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type);

   --
   --  Max_VBA
   --
   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type;

   --
   --  Execute_Progress
   --
   function Execute_Progress (Obj : Object_Type)
   return Boolean;

private

   type Object_Type is record

      Execute_Progress  : Boolean;
      Cache_Obj         : Cache.Cache_Type;
      Cache_Jobs_Data   : Cache.Jobs_Data_Type;
      Cache_Slots_Data  : Cache.Slots_Data_Type;
      Request_Pool_Obj  : Request_Pool.Object_Type;
      Crypto_Obj        : Crypto.Object_Type;
      IO_Obj            : Block_IO.Object_Type;
      Trans_Data        : Translation_Data_Type;
      New_Free_Tree_Obj : New_Free_Tree.Object_Type;
      Meta_Tree_Obj     : Meta_Tree.Object_Type;
      Cur_SB            : Superblocks_Index_Type;
      Cur_Gen           : Generation_Type;
      Superblock        : Superblock_Type;
      SB_Ctrl           : Superblock_Control.Control_Type;
      TA                : Trust_Anchor.Anchor_Type;
      VBD_Rkg           : VBD_Rekeying.Rekeying_Type;
      FT_Rszg           : FT_Resizing.Resizing_Type;
      MT_Rszg           : MT_Resizing.Resizing_Type;

   end record;

   --
   --  Idx_Of_Any_Invalid_Snap
   --
   function Idx_Of_Any_Invalid_Snap (Snapshots : Snapshots_Type)
   return Snapshots_Index_Type;

   --
   --  Execute_Free_Tree
   --
   procedure Execute_Free_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_Meta_Tree
   --
   procedure Execute_Meta_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_Request_Pool
   --
   procedure Execute_Request_Pool (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_SB_Ctrl
   --
   procedure Execute_SB_Ctrl (
      Obj        : in out Object_Type;
      Blk_IO_Buf : in out Block_IO.Data_Type;
      Progress   : in out Boolean);

   --
   --  Execute_TA
   --
   procedure Execute_TA (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_VBD_Rkg
   --
   procedure Execute_VBD_Rkg (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   --
   --  Execute_FT_Rszg
   --
   procedure Execute_FT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_MT_Rszg
   --
   procedure Execute_MT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_Crypto
   --
   procedure Execute_Crypto (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  :        Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf :        Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   --
   --  Execute_Blk_IO
   --
   procedure Execute_Blk_IO (
      Obj               : in out Object_Type;
      IO_Buf            :        Block_IO.Data_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   --
   --  Execute_Cache
   --
   procedure Execute_Cache (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean);

   --
   --  Execute_Cache_Generated_Prims
   --
   procedure Execute_Cache_Generated_Prims (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean);

   --
   --  Execute_Cache_Completed_Prims
   --
   procedure Execute_Cache_Completed_Prims (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

end CBE.Library;
