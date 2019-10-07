--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package body CBE.CXX.CXX_Library
with SPARK_Mode
is
   function Object_Size (Obj : Library.Object_Type)
   return CXX_Object_Size_Type
   is (Obj'Size / 8);

   procedure Initialize_Object (
      Obj     : out Library.Object_Type;
      SBs     :     Superblocks_Type;
      Curr_SB :     Superblocks_Index_Type)
   is
   begin
      Library.Initialize_Object (Obj, SBs, Curr_SB);
   end Initialize_Object;

   function Cache_Dirty (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Cache_Dirty (Obj)));

   function Superblock_Dirty (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Superblock_Dirty (Obj)));

   function Is_Securing_Superblock (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Is_Securing_Superblock (Obj)));

   function Is_Sealing_Generation (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Is_Sealing_Generation (Obj)));

   procedure Start_Securing_Superblock (Obj : in out Library.Object_Type)
   is
   begin
      Library.Start_Securing_Superblock (Obj);
   end Start_Securing_Superblock;

   procedure Start_Sealing_Generation (Obj : in out Library.Object_Type)
   is
   begin
      Library.Start_Sealing_Generation (Obj);
   end Start_Sealing_Generation;

   function Max_VBA (Obj : Library.Object_Type)
   return Virtual_Block_Address_Type
   is
   begin
      return Library.Max_VBA (Obj);
   end Max_VBA;

   procedure Execute (
      Obj               : in out Library.Object_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Now               :        Timestamp_Type)
   is
   begin
      Library.Execute (Obj, Crypto_Plain_Buf, Crypto_Cipher_Buf, Now);
   end Execute;

   function Request_Acceptable (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Request_Acceptable (Obj)));

   procedure Submit_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   is
   begin
      Library.Submit_Request (Obj, CXX_Request_To_SPARK (Req));
   end Submit_Request;

   function Peek_Completed_Request (Obj : Library.Object_Type)
   return CXX_Request_Type
   is (CXX_Request_From_SPARK (Library.Peek_Completed_Request (Obj)));

   procedure Drop_Completed_Request (
      Obj : in out Library.Object_Type;
      Req :        CXX_Request_Type)
   is
   begin
      Library.Drop_Completed_Request (Obj, CXX_Request_To_SPARK (Req));
   end Drop_Completed_Request;

   procedure IO_Data_Required (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.IO_Data_Required (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end IO_Data_Required;

   procedure IO_Data_Read_In_Progress (
      Obj      : in out Library.Object_Type;
      Req      :        CXX_Request_Type;
      Progress :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.IO_Data_Read_In_Progress (
         Obj, CXX_Request_To_SPARK (Req), SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end IO_Data_Read_In_Progress;

   procedure Supply_IO_Data (
      Obj      : in out Library.Object_Type;
      Req      :        CXX_Request_Type;
      Data     :        Block_Data_Type;
      Progress :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Supply_IO_Data (
         Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Supply_IO_Data;

   procedure Has_IO_Data_To_Write (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.Has_IO_Data_To_Write (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end Has_IO_Data_To_Write;

   procedure Obtain_IO_Data (
      Obj      : in out Library.Object_Type;
      Req      :        CXX_Request_Type;
      Data     :    out Block_Data_Type;
      Progress :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Obtain_IO_Data (
         Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Obtain_IO_Data;

   procedure Ack_IO_Data_To_Write (
      Obj      : in out Library.Object_Type;
      Req      :        CXX_Request_Type;
      Progress :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Ack_IO_Data_To_Write (
         Obj, CXX_Request_To_SPARK (Req), SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Ack_IO_Data_To_Write;

   procedure Client_Data_Ready (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.Client_Data_Ready (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end Client_Data_Ready;

   function Give_Data_Index (
      Obj : Library.Object_Type;
      Req : CXX_Request_Type)
   return CXX_Primitive_Index_Type
   is
   begin
      return CXX_Primitive_Index_Type (
         Library.Give_Data_Index (Obj, CXX_Request_To_SPARK (Req)));
   end Give_Data_Index;

   procedure Obtain_Client_Data (
      Obj              : in out Library.Object_Type;
      Req              :        CXX_Request_Type;
      Data_Index       :    out CXX_Crypto_Plain_Buffer_Index_Type;
      Data_Index_Valid :    out CXX_Bool_Type)
   is
      SPARK_Data_Index       : Crypto.Plain_Buffer_Index_Type;
      SPARK_Data_Index_Valid : Boolean;
   begin
      Library.Obtain_Client_Data (
         Obj, CXX_Request_To_SPARK (Req), SPARK_Data_Index,
         SPARK_Data_Index_Valid);

      Data_Index       := (Value => CXX_UInt32_Type (SPARK_Data_Index));
      Data_Index_Valid := CXX_Bool_From_SPARK (SPARK_Data_Index_Valid);
   end Obtain_Client_Data;

   procedure Obtain_Client_Data_2 (
      Obj              : in out Library.Object_Type;
      Req              :        CXX_Request_Type;
      Data             :    out Crypto.Plain_Data_Type;
      Progress         :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Obtain_Client_Data_2 (
         Obj, CXX_Request_To_SPARK (Req), Data, SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Obtain_Client_Data_2;

   procedure Client_Data_Required (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.Client_Data_Required (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end Client_Data_Required;

   function Supply_Client_Data (
      Obj     : in out Library.Object_Type;
      Now     :        Timestamp_Type;
      Req     :        CXX_Request_Type;
      Data    :        Block_Data_Type)
   return CXX_Bool_Type
   is
   begin
      return CXX_Bool_From_SPARK (
         Library.Supply_Client_Data (
            Obj, Now, CXX_Request_To_SPARK (Req), Data));
   end Supply_Client_Data;

   function Execute_Progress (Obj : Library.Object_Type)
   return CXX_Bool_Type
   is (CXX_Bool_From_SPARK (Library.Execute_Progress (Obj)));

   procedure Crypto_Data_Required (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.Crypto_Data_Required (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end Crypto_Data_Required;

   procedure Obtain_Crypto_Plain_Data (
      Obj              : in out Library.Object_Type;
      Req              :        CXX_Request_Type;
      Crypto_Plain_Buf :        Crypto.Plain_Buffer_Type;
      Data             :    out Crypto.Plain_Data_Type;
      Progress         :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Obtain_Crypto_Plain_Data (
         Obj, CXX_Request_To_SPARK (Req), Crypto_Plain_Buf, Data,
         SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Obtain_Crypto_Plain_Data;

   procedure Supply_Crypto_Cipher_Data (
      Obj               : in out Library.Object_Type;
      Req               :        CXX_Request_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Data              :        Crypto.Cipher_Data_Type;
      Progress          :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Supply_Crypto_Cipher_Data (
         Obj, CXX_Request_To_SPARK (Req), Crypto_Cipher_Buf, Data,
         SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Supply_Crypto_Cipher_Data;

   procedure Has_Crypto_Data_To_Decrypt (
      Obj : in out Library.Object_Type;
      Req :    out CXX_Request_Type)
   is
      SPARK_Req : Request.Object_Type;
   begin
      Library.Has_Crypto_Data_To_Decrypt (Obj, SPARK_Req);
      Req := CXX_Request_From_SPARK (SPARK_Req);
   end Has_Crypto_Data_To_Decrypt;

   procedure Obtain_Crypto_Cipher_Data (
      Obj               : in out Library.Object_Type;
      Req               :        CXX_Request_Type;
      Crypto_Cipher_Buf :        Crypto.Cipher_Buffer_Type;
      Data              :    out Crypto.Cipher_Data_Type;
      Progress          :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Obtain_Crypto_Cipher_Data (
         Obj, CXX_Request_To_SPARK (Req), Crypto_Cipher_Buf, Data,
         SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Obtain_Crypto_Cipher_Data;

   procedure Supply_Crypto_Plain_Data (
      Obj              : in out Library.Object_Type;
      Req              :        CXX_Request_Type;
      Crypto_Plain_Buf : in out Crypto.Plain_Buffer_Type;
      Data             :        Crypto.Plain_Data_Type;
      Progress         :    out CXX_Bool_Type)
   is
      SPARK_Progress : Boolean;
   begin
      Library.Supply_Crypto_Plain_Data (
         Obj, CXX_Request_To_SPARK (Req), Crypto_Plain_Buf, Data,
         SPARK_Progress);
      Progress := CXX_Bool_From_SPARK (SPARK_Progress);
   end Supply_Crypto_Plain_Data;

end CBE.CXX.CXX_Library;
