--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Block_Allocator
with SPARK_Mode
is
   procedure Initialize_Object (
      Obj        : out Object_Type;
      First_Addr :     Block_Number_Type)
   is
   begin
      Obj.Curr_Addr        := First_Addr;
      Obj.Prim             := Primitive.Invalid_Object;
      Obj.Execute_Progress := False;
   end Initialize_Object;

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (not Primitive.Valid (Obj.Prim));

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Primitive.Valid (Obj.Prim) or else
         Primitive.Success (Prim)
      then
         raise Program_Error;
      end if;
      Obj.Prim := Prim;
   end Submit_Primitive;

   procedure Execute (Obj : in out Object_Type)
   is
   begin
      Obj.Execute_Progress := False;
      if Obj.Curr_Addr = Block_Number_Type'Last or else
         not Primitive.Valid (Obj.Prim) or else
         Primitive.Success (Obj.Prim)
      then
         return;
      end if;
      Obj.Prim := Primitive.Copy_Valid_Object_New_Succ_Blk_Nr (
         Obj.Prim, True, Obj.Curr_Addr);
      Obj.Curr_Addr := Obj.Curr_Addr + 1;
      Obj.Execute_Progress := True;
   end Execute;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if not Primitive.Valid (Obj.Prim) or else
         not Primitive.Success (Obj.Prim)
      then
         return Primitive.Invalid_Object;
      end if;
      return Obj.Prim;
   end Peek_Completed_Primitive;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if not Primitive.Valid (Obj.Prim) or else
         not Primitive.Success (Obj.Prim) or else
         not Primitive.Equal (Obj.Prim, Prim)
      then
         raise Program_Error;
      end if;
      Obj.Prim := Primitive.Invalid_Object;
   end Drop_Completed_Primitive;

end CBE.Block_Allocator;
