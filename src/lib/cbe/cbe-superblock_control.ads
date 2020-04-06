--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Superblock_Control
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Jobs : constant := 2;

   type Control_Type is private;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   --
   --  Initialize_Control
   --
   procedure Initialize_Control (Ctrl : out Control_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Ctrl : Control_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Ctrl     : in out Control_Type;
      SB       : in out Superblock_Type;
      Progress : in out Boolean);

   --
   --  Peek_Generated_TA_Primitive
   --
   function Peek_Generated_TA_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Plain_Key
   --
   function Peek_Generated_Key_Plaintext (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type)
   return Key_Plaintext_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Complete_Key_Plaintext
   --
   procedure Mark_Generated_Prim_Complete_Key_Plaintext (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Plaintext_Type);

   --
   --  Mark_Generated_Prim_Complete_Key_Ciphertext
   --
   procedure Mark_Generated_Prim_Complete_Key_Ciphertext (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Ciphertext_Type);

private

   type Job_Operation_Type is (
      Invalid,
      Initialize_Rekeying);

   type Job_State_Type is (
      Submitted,
      Create_Key_Pending,
      Create_Key_In_Progress,
      Create_Key_Completed,
      Encrypt_Key_Pending,
      Encrypt_Key_In_Progress,
      Encrypt_Key_Completed,
      Write_SB_Pending,
      Write_SB_In_Progress,
      Write_SB_Completed,
      Sync_Pending,
      Sync_In_Progress,
      Sync_Completed,
      Secure_SB_Pending,
      Secure_SB_In_Progress,
      Secure_SB_Completed,
      Completed);

   type Job_Type is record
      Operation : Job_Operation_Type;
      State : Job_State_Type;
      Submitted_Prim : Primitive.Object_Type;
      Generated_Prim : Primitive.Object_Type;
      Key_Plaintext : Key_Plaintext_Type;
      Key_Ciphertext : Key_Ciphertext_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Control_Type is record
      Jobs : Jobs_Type;
   end record;

   --
   --  Execute_Initialize_Rekeying
   --
   procedure Execute_Initialize_Rekeying (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      SB       : in out Superblock_Type;
      Progress : in out Boolean);

end CBE.Superblock_Control;
