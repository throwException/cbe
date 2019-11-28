--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Block_Allocator
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialize_Object (
      Obj        : out Object_Type;
      First_Addr :     Block_Number_Type);

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean;

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type);

   procedure Execute (Obj : in out Object_Type);

   function Execute_Progress (Obj : Object_Type)
   return Boolean;

private

   type Object_Type is record
      Curr_Addr        : Block_Number_Type;
      Prim             : Primitive.Object_Type;
      Execute_Progress : Boolean;
   end record;

end CBE.Block_Allocator;
