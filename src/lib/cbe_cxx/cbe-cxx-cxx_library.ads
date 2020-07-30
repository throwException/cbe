--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Library;
with CBE.Crypto;
with CBE.Block_IO;

package CBE.CXX.CXX_Library
with SPARK_Mode
is
   pragma Pure;

   function Object_Size (Obj : Library.Object_Type)
   return CXX_Object_Size_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe11object_sizeERKNS_7LibraryE";

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Library.Object_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN3Cbe7LibraryC2Ev";

   --
   --  Info
   --
   procedure Info (
      Obj  :     Library.Object_Type;
      Info : out CXX_Info_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library5_infoERNS_4InfoE";

   procedure Active_Snapshot_IDs (
      Obj :     Library.Object_Type;
      IDs : out Active_Snapshot_IDs_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library19active_snapshot_idsERNS_19Active_snapshot_idsE";

   function Max_VBA (Obj : Library.Object_Type)
   return Virtual_Block_Address_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library7max_vbaEv";

   --
   --  Execute
   --
   procedure Execute (
      Obj               : in out Library.Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library7executeERNS_9Io_bufferERNS_" &
         "19Crypto_plain_bufferERNS_20Crypto_cipher_bufferE";

   function Client_Request_Acceptable (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library25client_request_acceptableEv";

   procedure Submit_Client_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type;
      ID  :        CXX_Snapshot_ID_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library21submit_client_requestERKNS_7RequestEj";

   function Peek_Completed_Client_Request (Obj : Library.Object_Type)
   return CXX_Request_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library29peek_completed_client_requestEv";

   procedure Drop_Completed_Client_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library29drop_completed_client_requestERKNS_7RequestE";

   procedure IO_Request_Completed (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type;
      Success    :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library20io_request_completedERKNS_9Io_buffer5IndexEb";

   procedure Has_IO_Request (
      Obj        :     Library.Object_Type;
      Req        : out CXX_Request_Type;
      Data_Index : out CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library15_has_io_requestERNS_7RequestERNS_9Io_buffer" &
         "5IndexE";

   procedure IO_Request_In_Progress (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library22io_request_in_progressERKNS_9Io_buffer5IndexE";

   --
   --  Client_Transfer_Read_Data_Required
   --
   procedure Client_Transfer_Read_Data_Required (
      Obj           :     Library.Object_Type;
      Req           : out CXX_Request_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library34client_transfer_read_data_requiredERNS_" &
         "7RequestERyRNS_19Crypto_plain_buffer5IndexE";

   --
   --  Client_Transfer_Read_Data_In_Progress
   --
   procedure Client_Transfer_Read_Data_In_Progress (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library37client_transfer_read_data_in_progressERKNS_" &
         "19Crypto_plain_buffer5IndexE";

   --
   --  Client_Transfer_Read_Data_Completed
   --
   procedure Client_Transfer_Read_Data_Completed (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        CXX_Crypto_Plain_Buffer_Index_Type;
      Success       :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library35client_transfer_read_data_completedERKNS_" &
         "19Crypto_plain_buffer5IndexEb";

   --
   --  Client_Transfer_Write_Data_Required
   --
   procedure Client_Transfer_Write_Data_Required (
      Obj           :     Library.Object_Type;
      Req           : out CXX_Request_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library35client_transfer_write_data_requiredERNS_" &
         "7RequestERyRNS_19Crypto_plain_buffer5IndexE";

   --
   --  Client_Transfer_Write_Data_In_Progress
   --
   procedure Client_Transfer_Write_Data_In_Progress (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library38client_transfer_write_data_in_progressERKNS_" &
         "19Crypto_plain_buffer5IndexE";

   --
   --  Client_Transfer_Write_Data_Completed
   --
   procedure Client_Transfer_Write_Data_Completed (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        CXX_Crypto_Plain_Buffer_Index_Type;
      Success       :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library36client_transfer_write_data_completedERKNS_" &
         "19Crypto_plain_buffer5IndexEb";

   function Execute_Progress (Obj : Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK3Cbe7Library16execute_progressEv";

   procedure Crypto_Add_Key_Required (
      Obj :     Library.Object_Type;
      Req : out CXX_Request_Type;
      Key : out CXX_Key_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library24_crypto_add_key_requiredERNS_7RequestERNS_3KeyE";

   procedure Crypto_Add_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library24crypto_add_key_requestedERKNS_7RequestE";

   procedure Crypto_Add_Key_Completed (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library24crypto_add_key_completedERKNS_7RequestE";

   procedure Crypto_Remove_Key_Required (
      Obj    :     Library.Object_Type;
      Req    : out CXX_Request_Type;
      Key_ID : out CXX_Key_ID_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library27_crypto_remove_key_requiredERNS_7RequestERNS_" &
         "3Key2IdE";

   procedure Crypto_Remove_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library27crypto_remove_key_requestedERKNS_7RequestE";

   procedure Crypto_Remove_Key_Completed (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library27crypto_remove_key_completedERKNS_7RequestE";

   procedure Crypto_Cipher_Data_Required (
      Obj        :     Library.Object_Type;
      Req        : out CXX_Request_Type;
      Data_Index : out CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library28_crypto_cipher_data_requiredERNS_7RequestERNS_" &
         "19Crypto_plain_buffer5IndexE";

   procedure Crypto_Cipher_Data_Requested (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        CXX_Crypto_Plain_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library28crypto_cipher_data_requestedERKNS_" &
         "19Crypto_plain_buffer5IndexE";

   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type;
      Data_Valid :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library25supply_crypto_cipher_dataERKNS_" &
         "20Crypto_cipher_buffer5IndexEb";

   procedure Crypto_Plain_Data_Required (
      Obj        :     Library.Object_Type;
      Req        : out CXX_Request_Type;
      Data_Index : out CXX_Crypto_Cipher_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library27_crypto_plain_data_requiredERNS_7RequestERNS_" &
         "20Crypto_cipher_buffer5IndexE";

   --
   --  Crypto_Plain_Data_Requested
   --
   procedure Crypto_Plain_Data_Requested (
      Obj            : in out Library.Object_Type;
      Cipher_Buf_Idx :        CXX_Crypto_Cipher_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library27crypto_plain_data_requestedERKNS_" &
         "20Crypto_cipher_buffer5IndexE";

   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Library.Object_Type;
      Data_Index :        CXX_Crypto_Plain_Buffer_Index_Type;
      Data_Valid :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library24supply_crypto_plain_dataERKNS_" &
         "19Crypto_plain_buffer5IndexEb";

   procedure Peek_Generated_TA_Request (
      Obj :     Library.Object_Type;
      Req : out CXX_TA_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library26_peek_generated_ta_requestERNS_" &
         "20Trust_anchor_requestE";

   procedure Drop_Generated_TA_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_TA_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library25drop_generated_ta_requestERKNS_" &
         "20Trust_anchor_requestE";

   procedure Peek_Generated_TA_SB_Hash (
      Obj :      Library.Object_Type;
      Req :      CXX_TA_Request_Type;
      Hash : out CXX_Hash_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library26_peek_generated_ta_sb_hashERKNS_" &
         "20Trust_anchor_requestERNS_4HashE";

   procedure Peek_Generated_TA_Key_Cipher (
      Obj :     Library.Object_Type;
      Req :     CXX_TA_Request_Type;
      Key : out CXX_Key_Value_Ciphertext_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library39_peek_generated_ta_key_value_ciphertextERKNS_" &
         "20Trust_anchor_requestERNS_20Key_ciphertext_valueE";

   procedure Peek_Generated_TA_Key_Plain (
      Obj :     Library.Object_Type;
      Req :     CXX_TA_Request_Type;
      Key : out CXX_Key_Value_Plaintext_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK3Cbe7Library38_peek_generated_ta_key_value_plaintextERKNS_" &
         "20Trust_anchor_requestERNS_19Key_plaintext_valueE";

   procedure Mark_Generated_TA_Create_Key_Request_Complete (
      Obj : in out Library.Object_Type;
      Req :        CXX_TA_Request_Type;
      Key :        CXX_Key_Value_Plaintext_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library" &
         "45mark_generated_ta_create_key_request_completeERKNS_" &
         "20Trust_anchor_requestERKNS_19Key_plaintext_valueE";

   procedure Mark_Generated_TA_Secure_SB_Request_Complete (
      Obj : in out Library.Object_Type;
      Req :        CXX_TA_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library" &
         "44mark_generated_ta_secure_sb_request_completeERKNS_" &
         "20Trust_anchor_requestE";

   procedure Mark_Generated_TA_Decrypt_Key_Request_Complete (
      Obj : in out Library.Object_Type;
      Req :        CXX_TA_Request_Type;
      Key :        CXX_Key_Value_Plaintext_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library" &
         "46mark_generated_ta_decrypt_key_request_completeERKNS_" &
         "20Trust_anchor_requestERKNS_19Key_plaintext_valueE";

   procedure Mark_Generated_TA_Encrypt_Key_Request_Complete (
      Obj : in out Library.Object_Type;
      Req :        CXX_TA_Request_Type;
      Key :        CXX_Key_Value_Ciphertext_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN3Cbe7Library" &
         "46mark_generated_ta_encrypt_key_request_completeERKNS_" &
         "20Trust_anchor_requestERKNS_20Key_ciphertext_valueE";

end CBE.CXX.CXX_Library;
