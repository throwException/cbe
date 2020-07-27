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

package CBE.Block_IO
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   type Data_Index_Type is range 0 .. 0;

   type Num_Entries_Type is range 0 .. Data_Index_Type'Last + 1;

   type Data_Type
   is array (Data_Index_Type) of Block_Data_Type with Size => Block_Size;

   --
   --  Initialize_Object
   --
   --  FIXME will not be used anymore when the library module is in spark
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Check if the module can accept new primitives
   --
   --  \return true if a primitive can be accepted, otherwise false
   --
   function Primitive_Acceptable (Obj : Object_Type) return Boolean;

   --
   --  Submit a new primitive
   --
   --  The primitive will be copied to the internal buffer and the Block_data
   --  reference will be stored as a reference. The method may only be called
   --  after 'acceptable' was executed and returned true. The new primitive is
   --  marked as pending and waits for execution.
   --
   --  \param Prim  reference to the Primitive
   --  \param Data  reference to a Block_data object
   --
   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Tag  :        Primitive.Tag_Type;
      Prim :        Primitive.Object_Type);

   procedure Submit_Primitive (
      Obj        : in out Object_Type;
      Tag        :        Primitive.Tag_Type;
      Prim       :        Primitive.Object_Type;
      Data_Index :    out Data_Index_Type);

   --
   --  Submit_Primitive_Client_Data
   --
   procedure Submit_Primitive_Client_Data (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Req    :        Request.Object_Type;
      VBA    :        Virtual_Block_Address_Type;
      Key_ID :        Key_ID_Type);

   --
   --  FIXME This function is currently only needed for reading actual data
   --        blocks (Tag_Decrypt). In this case we have to remember the
   --        expected hash - that we received from the VBD - somewhere until
   --        the data is available. The VBD forgets the hash once it has
   --        delivered the completed primitive.
   --
   procedure Submit_Primitive_Decrypt (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Hash   :        Hash_Type;
      Key_ID :        Key_ID_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Key_ID
   --
   function Peek_Completed_Key_ID (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Get access to the data of a completed primitive
   --
   --  This method must only be called after executing
   --  'peek_completed_primitive' returned a valid primitive.
   --
   --  \param Prim  reference to the completed primitive
   --
   --  \return reference to the data
   --
   function Peek_Completed_Data_Index (Obj : Object_Type)
   return Data_Index_Type;

   --
   --  Get the original tag of the submitted primitive
   --
   --  This method must only be called after executing
   --  'peek_completed_primitive' returned a valid primitive.
   --
   --  \param Prim  refrence to the completed primitive
   --
   --  \return original tag of the submitted primitive
   --
   function Peek_Completed_Tag (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Primitive.Tag_Type;

   function Peek_Completed_Hash (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type;

   --
   --  Peek_Completed_Hash_New
   --
   function Peek_Completed_Hash_New (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type;

   --
   --  Take the next completed primitive
   --
   --  This method must only be called after executing
   --  'peek_completed_primitive' returned true.
   --
   --  It takes next valid completed primitive and removes it
   --  from the module
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Drop_Completed_Primitive_New
   --
   procedure Drop_Completed_Primitive_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Drop_Generated_Primitive_New
   --
   procedure Drop_Generated_Primitive_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Data_Buf :        Data_Type;
      Progress : in out Boolean);

   --
   --  Peek_Generated_Blk_Dev_Primitive
   --
   function Peek_Generated_Blk_Dev_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Crypto_Primitive
   --
   function Peek_Generated_Crypto_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Get index for the data block of the generated primitive
   --
   --  This method must only be called after 'peek_generated_io_primitive'
   --  returned a valid primitive.
   --
   --  \param p  reference to the completed primitive
   --
   --  \return index for data block
   --
   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

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
   --  Discard given generated primitive
   --
   --  This method must only be called after 'peek_generated_io_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Discard given generated primitive
   --
   --  This method must only be called after 'peek_generated_io_primitive'
   --  returned a valid primitive.
   --
   --  \param Prim  reference to primitive
   --
   procedure Drop_Generated_Primitive_2 (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type);

   procedure Mark_Generated_Primitive_Complete (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type;
      Success  :        Boolean);

   --
   --  Mark_Generated_Primitive_Complete_New
   --
   procedure Mark_Generated_Primitive_Complete_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

private

   type Entry_State_Type is (

      Read_Client_Data_Submitted,
      Read_Client_Data_Completed,

      Read_Client_Data_Read_Data_Pending,
      Read_Client_Data_Read_Data_In_Progress,
      Read_Client_Data_Read_Data_Completed,

      Read_Client_Data_Decrypt_And_Supply_Data_Pending,
      Read_Client_Data_Decrypt_And_Supply_Data_In_Progress,
      Read_Client_Data_Decrypt_And_Supply_Data_Completed,

      Write_Client_Data_Submitted,
      Write_Client_Data_Completed,

      Write_Client_Data_Write_Data_Pending,
      Write_Client_Data_Write_Data_In_Progress,
      Write_Client_Data_Write_Data_Completed,

      Write_Client_Data_Obtain_And_Encrypt_Data_Pending,
      Write_Client_Data_Obtain_And_Encrypt_Data_In_Progress,
      Write_Client_Data_Obtain_And_Encrypt_Data_Completed,

      Unused,
      Pending,
      In_Progress,
      Complete);

   type Entry_Type is record
      Orig_Tag       : Primitive.Tag_Type;
      Prim           : Primitive.Object_Type;
      Hash           : Hash_Type;
      Hash_Valid     : Boolean;
      State          : Entry_State_Type;
      Key_ID         : Key_ID_Type;
      Req            : Request.Object_Type;
      VBA            : Virtual_Block_Address_Type;
      Submitted_Prim : Primitive.Object_Type;
      Generated_Prim : Primitive.Object_Type;
   end record;

   subtype Entries_Index_Type is Data_Index_Type;

   type Entries_Type is array (Entries_Index_Type) of Entry_Type;

   --
   --  Object_Type
   --
   type Object_Type is record
      Entries      : Entries_Type;
      Used_Entries : Num_Entries_Type;
   end record;

   --
   --  Execute_Read_Client_Data
   --
   procedure Execute_Read_Client_Data (
      Entr      : in out Entry_Type;
      Entry_Idx :        Entries_Index_Type;
      Progress  : in out Boolean);

   --
   --  Execute_Write_Client_Data
   --
   procedure Execute_Write_Client_Data (
      Entr      : in out Entry_Type;
      Entry_Idx :        Entries_Index_Type;
      Data_Buf  :        Data_Type;
      Progress  : in out Boolean);

end CBE.Block_IO;
