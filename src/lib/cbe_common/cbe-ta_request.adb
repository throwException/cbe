--
--  Copyright (C) 2020 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.TA_Request
with SPARK_Mode
is
   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   is (
      Valid     => False,
      Operation => Operation_Type'First,
      Success   => False,
      Tag       => 0);

   --
   --  Valid_Object
   --
   function Valid_Object (
      Op     : TA_Request.Operation_Type;
      Succ   : TA_Request.Success_Type;
      Tg     : Tag_Type)
   return Object_Type
   is (
      Valid     => True,
      Operation => Op,
      Success   => Succ,
      Tag       => Tg);

   --
   --  Equal
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   is (
      --  Obj_1.Valid     = Obj_2.Valid and then
      Obj_1.Tag       = Obj_2.Tag and then
      Obj_1.Operation = Obj_2.Operation);

   -----------------
   --  Accessors  --
   -----------------

   function Valid (Obj : Object_Type) return Boolean
   is (Obj.Valid);

   function Operation (Obj : Object_Type) return TA_Request.Operation_Type
   is (Obj.Operation);

   function Success (Obj : Object_Type) return Success_Type
   is (Obj.Success);

   function Tag (Obj : Object_Type) return Tag_Type
   is (Obj.Tag);

   procedure Success (
      Obj  : in out Object_Type;
      Succ : Success_Type)
   is
   begin
      Obj.Success := Succ;
   end Success;

end CBE.TA_Request;
