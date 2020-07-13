--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.TA_Request;

package CBE.Trust_Anchor
with SPARK_Mode
is
   pragma Pure;

   type Anchor_Type is private;

   --
   --  Initialize_Anchor
   --
   procedure Initialize_Anchor (Anchor : out Anchor_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Anchor : Anchor_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type);

   --
   --  Submit_Primitive_Key_Value_Plaintext
   --
   procedure Submit_Primitive_Key_Value_Plaintext (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Key    :        Key_Value_Plaintext_Type);

   --
   --  Submit_Primitive_Key_Value_Ciphertext
   --
   procedure Submit_Primitive_Key_Value_Ciphertext (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Key    :        Key_Value_Ciphertext_Type);

   --
   --  Submit_Primitive_Hash
   --
   procedure Submit_Primitive_Hash (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Hash   :        Hash_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Anchor : Anchor_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Key_Value_Plaintext
   --
   function Peek_Completed_Key_Value_Plaintext (
      Anchor : Anchor_Type;
      Prim   : Primitive.Object_Type)
   return Key_Value_Plaintext_Type;

   --
   --  Peek_Completed_Key_Value_Ciphertext
   --
   function Peek_Completed_Key_Value_Ciphertext (
      Anchor : Anchor_Type;
      Prim   : Primitive.Object_Type)
   return Key_Value_Ciphertext_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Anchor : in out Anchor_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Peek generated request
   --
   procedure Peek_Generated_Request (
      Anchor :     Anchor_Type;
      Req    : out TA_Request.Object_Type);

   --
   --  Drop generated request
   --
   procedure Drop_Generated_Request (
      Anchor : in out Anchor_Type;
      Req    :        TA_Request.Object_Type);

   --
   --  Peek generated superblock hash
   --
   procedure Peek_Generated_SB_Hash (
      Anchor :     Anchor_Type;
      Req    :     TA_Request.Object_Type;
      Hash   : out Hash_Type);

   --
   --  Peek generated key value ciphertext
   --
   procedure Peek_Generated_Key_Value_Ciphertext (
      Anchor    :     Anchor_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Ciphertext_Type);

   --
   --  Peek generated key value plaintext
   --
   procedure Peek_Generated_Key_Value_Plaintext (
      Anchor    :     Anchor_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Plaintext_Type);

   --
   --  Mark generated TA create key request complete
   --
   procedure Mark_Generated_Create_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark generated TA secure superblock request complete
   --
   procedure Mark_Generated_Secure_SB_Request_Complete (
      Anchor : in out Anchor_Type;
      Req    :        TA_Request.Object_Type);

   --
   --  Mark generated TA decrypt key request complete
   --
   procedure Mark_Generated_Decrypt_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark generated TA encrypt key request complete
   --
   procedure Mark_Generated_Encrypt_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type);

private

   Nr_Of_Jobs : constant := 2;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   type Job_Operation_Type is (
      Invalid,
      Create_Key,
      Secure_Superblock,
      Encrypt_Key,
      Decrypt_Key);

   type Job_State_Type is (
      Submitted,
      Dropped,
      Completed);

   type Job_Type is record
      Operation            : Job_Operation_Type;
      State                : Job_State_Type;
      Submitted_Prim       : Primitive.Object_Type;
      Key_Value_Plaintext  : Key_Value_Plaintext_Type;
      Key_Value_Ciphertext : Key_Value_Ciphertext_Type;
      Hash                 : Hash_Type;
      Request              : TA_Request.Object_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Anchor_Type is record
      Jobs : Jobs_Type;
   end record;

end CBE.Trust_Anchor;
