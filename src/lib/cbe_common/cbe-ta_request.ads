--
--  Copyright (C) 2020 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package CBE.TA_Request
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   type Operation_Type is (
      Create_Key,
      Secure_Superblock,
      Encrypt_Key,
      Decrypt_Key);

   subtype Success_Type is Boolean;

   type Tag_Type is range 0 .. 2**32 - 1;

   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   with Post => (not Valid (Invalid_Object'Result));

   --
   --  Valid_Object
   --
   function Valid_Object (
      Op     : Operation_Type;
      Succ   : Success_Type;
      Tg     : Tag_Type)
   return Object_Type
   with
      Post => (
         Valid (Valid_Object'Result) and then (
            Operation    (Valid_Object'Result) = Op     and then
            Success      (Valid_Object'Result) = Succ   and then
            Tag          (Valid_Object'Result) = Tg));

   --
   --  Equal
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   with
      Pre => Valid (Obj_1) and then Valid (Obj_2);

   -----------------
   --  Accessors  --
   -----------------

   function Valid (Obj : Object_Type)
   return Boolean;

   function Operation (Obj : Object_Type)
   return Operation_Type
   with Pre => (Valid (Obj));

   function Success (Obj : Object_Type)
   return Success_Type
   with Pre => (Valid (Obj));

   function Tag (Obj : Object_Type)
   return Tag_Type
   with Pre => (Valid (Obj));

   procedure Success (
      Obj  : in out Object_Type;
      Succ :        Success_Type)
   with Pre => (Valid (Obj));

   --  function To_String (Obj : Object_Type) return String;

private

   --
   --  Object_Type
   --
   type Object_Type is record
      Valid        : Boolean;
      Operation    : Operation_Type;
      Success      : Success_Type;
      Tag          : Tag_Type;
   end record;

end CBE.TA_Request;
