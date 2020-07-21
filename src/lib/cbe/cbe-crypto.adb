--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Crypto
with SPARK_Mode
is
   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Jobs => (
         others => (
            State => Invalid,
            Prim => Primitive.Invalid_Object,
            Key => Key_Plaintext_Invalid)),

      Execute_Progress => False);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (
      for some Itm of Obj.Jobs => Itm.State = Invalid);

   --
   --  Submit_Primitive_Key
   --
   procedure Submit_Primitive_Key (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Plaintext_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key := Key;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive_Key;

   --
   --  Submit_Primitive_Key_ID
   --
   procedure Submit_Primitive_Key_ID (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Key_ID :        Key_ID_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive_Key_ID;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;

            Data_Idx := Job_Idx;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive;

   --
   --  Submit_Completed_Primitive
   --
   procedure Submit_Completed_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Complete;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;

            Data_Idx := Job_Idx;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj     :     Object_Type;
      Job_Idx : out Jobs_Index_Type;
      Prim    : out Primitive.Object_Type)
   is
   begin

      For_Each_Job :
      for Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Idx).State = Pending then

            Prim := Obj.Jobs (Idx).Prim;
            Job_Idx := Idx;
            return;

         end if;

      end loop For_Each_Job;

      Prim := Primitive.Invalid_Object;
      Job_Idx := Jobs_Index_Type'First;

   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type)
   is
   begin

      if Obj.Jobs (Job_Idx).State /= Pending then
         raise Program_Error;
      end if;
      Obj.Jobs (Job_Idx).State := In_Progress;

   end Drop_Generated_Primitive;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_ID_Type
   is
   begin

      if Obj.Jobs (Job_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Jobs (Job_Idx).Key.ID;

   end Peek_Generated_Key_ID;

   --
   --  Peek_Generated_Key
   --
   function Peek_Generated_Key (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_Plaintext_Type
   is
   begin

      if Obj.Jobs (Job_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Jobs (Job_Idx).Key;

   end Peek_Generated_Key;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Complete then
            return Obj.Jobs (Job_Idx).Prim;
         end if;

      end loop For_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Complete then
            Obj.Jobs (Job_Idx).State := Invalid;
            return;
         end if;

      end loop For_Each_Job;

   end Drop_Completed_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type;
      Success :        Boolean)
   is
   begin

      if Obj.Jobs (Job_Idx).State /= In_Progress then
         raise Program_Error;
      end if;

      Obj.Jobs (Job_Idx).State := Complete;
      Primitive.Success (Obj.Jobs (Job_Idx).Prim, Success);

   end Mark_Completed_Primitive;

   --
   --  Data_Index
   --
   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Jobs_Index_Type
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Primitive.Equal (Obj.Jobs (Job_Idx).Prim, Prim) then
            return Job_Idx;
         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Data_Index;

end CBE.Crypto;
