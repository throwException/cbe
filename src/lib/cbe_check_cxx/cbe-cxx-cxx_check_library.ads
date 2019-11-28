--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Check_Library;
with CBE.Block_IO;

package CBE.CXX.CXX_Check_Library
with SPARK_Mode
is
   pragma Pure;

   function Object_Size (Obj : Check_Library.Object_Type)
   return CXX_Object_Size_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZN9Cbe_check11object_sizeERKNS_7LibraryE";

   procedure Initialize_Object (Obj : out Check_Library.Object_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN9Cbe_check7LibraryC1Ev";

   function Client_Request_Acceptable (Obj : Check_Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK9Cbe_check7Library25client_request_acceptableEv";

   procedure Submit_Client_Request (
      Obj : in out Check_Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN9Cbe_check7Library21submit_client_requestERKN3Cbe7RequestE";

   function Peek_Completed_Client_Request (Obj : Check_Library.Object_Type)
   return CXX_Request_Type
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK9Cbe_check7Library29peek_completed_client_requestEv";

   procedure Drop_Completed_Client_Request (
      Obj : in out Check_Library.Object_Type;
      Req :        CXX_Request_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN9Cbe_check7Library29drop_completed_client_requestERKN3Cbe" &
         "7RequestE";

   procedure Execute (
      Obj        : in out Check_Library.Object_Type;
      Blk_IO_Buf :        Block_IO.Data_Type)
   with
      Export,
      Convention    => C,
      External_Name => "_ZN9Cbe_check7Library7executeERKN3Cbe9Io_bufferE";

   function Execute_Progress (Obj : Check_Library.Object_Type)
   return CXX_Bool_Type
   with
      Export,
      Convention    => C,
      External_Name => "_ZNK9Cbe_check7Library16execute_progressEv";

   procedure IO_Request_Completed (
      Obj        : in out Check_Library.Object_Type;
      Data_Index :        CXX_Crypto_Cipher_Buffer_Index_Type;
      Success    :        CXX_Bool_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN9Cbe_check7Library20io_request_completedERKN3Cbe9Io_buffer" &
         "5IndexEb";

   procedure Has_IO_Request (
      Obj        :     Check_Library.Object_Type;
      Req        : out CXX_Request_Type;
      Data_Index : out CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZNK9Cbe_check7Library14has_io_requestERN3Cbe7RequestERNS1_" &
         "9Io_buffer5IndexE";

   procedure IO_Request_In_Progress (
      Obj        : in out Check_Library.Object_Type;
      Data_Index :        CXX_IO_Buffer_Index_Type)
   with
      Export,
      Convention    => C,
      External_Name =>
         "_ZN9Cbe_check7Library22io_request_in_progressERKN3Cbe9Io_buffer" &
         "5IndexE";

end CBE.CXX.CXX_Check_Library;
