--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Request;
with CBE.Block_IO;

package CBE.Crypto
with SPARK_Mode
is
   pragma Pure;

   subtype Plain_Data_Type is CBE.Block_Data_Type;
   subtype Cipher_Data_Type is CBE.Block_Data_Type;

   type Jobs_Index_Type is range 0 .. 0;

   type Plain_Buffer_Index_Type is new Jobs_Index_Type;

   type Cipher_Buffer_Index_Type is new Jobs_Index_Type;

   type Plain_Buffer_Type is array (Jobs_Index_Type) of Plain_Data_Type;

   type Cipher_Buffer_Type is array (Jobs_Index_Type) of Cipher_Data_Type;

   type Object_Type is private;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type)
   with
      Pre => (
         Primitive_Acceptable (Obj) and then
         Primitive.Valid (Prim));

   --
   --  Submit_Primitive_Key
   --
   procedure Submit_Primitive_Key (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Plaintext_Type);

   --
   --  Submit_Primitive_Key_ID
   --
   procedure Submit_Primitive_Key_ID (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Key_ID :        Key_ID_Type);

   --
   --  Submit_Primitive_Encrypt_Client_Data
   --
   procedure Submit_Primitive_Encrypt_Client_Data (
      Obj             : in out Object_Type;
      Prim            :        Primitive.Object_Type;
      Req             :        Request.Object_Type;
      VBA             :        Virtual_Block_Address_Type;
      Key_ID          :        Key_ID_Type;
      Blk_IO_Data_Idx :        Block_IO.Data_Index_Type);

   --
   --  Submit_Primitive_Decrypt_Client_Data
   --
   procedure Submit_Primitive_Decrypt_Client_Data (
      Obj            : in out Object_Type;
      Prim           :        Primitive.Object_Type;
      Req            :        Request.Object_Type;
      VBA            :        Virtual_Block_Address_Type;
      Key_ID         :        Key_ID_Type;
      Cipher_Buf_Idx :    out Cipher_Buffer_Index_Type);

   --
   --  Submit_Completed_Primitive
   --
   procedure Submit_Completed_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type);

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj     :     Object_Type;
      Job_Idx : out Jobs_Index_Type;
      Prim    : out Primitive.Object_Type);

   --
   --  Peek_Generated_Crypto_Dev_Primitive
   --
   function Peek_Generated_Crypto_Dev_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Client_Primitive
   --
   function Peek_Generated_Client_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_Key_ID_New
   --
   function Peek_Generated_Key_ID_New (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_Cipher_Buf_Idx
   --
   function Peek_Generated_Cipher_Buf_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Cipher_Buffer_Index_Type;

   --
   --  Peek_Generated_Plain_Buf_Idx
   --
   function Peek_Generated_Plain_Buf_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Plain_Buffer_Index_Type;

   --
   --  Peek_Generated_Req
   --
   function Peek_Generated_Req (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Request.Object_Type;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Virtual_Block_Address_Type;

   --
   --  Peek_Generated_Key
   --
   function Peek_Generated_Key (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_Plaintext_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type);

   --
   --  Drop_Generated_Primitive_New
   --
   procedure Drop_Generated_Primitive_New (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Cipher_Buf_Idx
   --
   function Peek_Completed_Cipher_Buf_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Jobs_Index_Type;

   --
   --  Peek_Completed_Blk_IO_Data_Idx
   --
   function Peek_Completed_Blk_IO_Data_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_IO.Data_Index_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type);

   --
   --  Drop_Completed_Primitive_New
   --
   procedure Drop_Completed_Primitive_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type;
      Success :        Boolean);

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Plain_Buffer_Index_Type;
      Success       :        Boolean);

   --
   --  Data_Index
   --
   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Jobs_Index_Type;

private

   type Job_State_Type is (

      DSCD_Submitted,
      DSCD_Completed,

      DSCD_Decrypt_Data_Pending,
      DSCD_Decrypt_Data_In_Progress,
      DSCD_Decrypt_Data_Completed,

      DSCD_Supply_Data_Pending,
      DSCD_Supply_Data_In_Progress,
      DSCD_Supply_Data_Completed,

      OECD_Submitted,
      OECD_Completed,

      OECD_Encrypt_Data_Pending,
      OECD_Encrypt_Data_In_Progress,
      OECD_Encrypt_Data_Completed,

      OECD_Obtain_Data_Pending,
      OECD_Obtain_Data_In_Progress,
      OECD_Obtain_Data_Completed,

      Invalid,
      Pending,
      In_Progress,
      Complete);

   type Job_Type is record
      State           : Job_State_Type;
      Prim            : Primitive.Object_Type;
      Submitted_Prim  : Primitive.Object_Type;
      Generated_Prim  : Primitive.Object_Type;
      Req             : Request.Object_Type;
      VBA             : Virtual_Block_Address_Type;
      Key             : Key_Plaintext_Type;
      Blk_IO_Data_Idx : Block_IO.Data_Index_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Object_Type is record
      Jobs : Jobs_Type;
   end record;

   --
   --  Execute_Decrypt_And_Supply_Client_Data
   --
   procedure Execute_Decrypt_And_Supply_Client_Data (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Obtain_And_Encrypt_Client_Data
   --
   procedure Execute_Obtain_And_Encrypt_Client_Data (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

end CBE.Crypto;
