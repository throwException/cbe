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
      Items => (
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
      for some Itm of Obj.Items => Itm.State = Invalid);

   --
   --  Submit_Primitive_Key
   --
   procedure Submit_Primitive_Key (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Plaintext_Type)
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Invalid then

            Obj.Items (Item_Idx).State := Pending;
            Obj.Items (Item_Idx).Prim := Prim;
            Obj.Items (Item_Idx).Key := Key;
            return;

         end if;

      end loop For_Each_Item;
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

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Invalid then

            Obj.Items (Item_Idx).State := Pending;
            Obj.Items (Item_Idx).Prim := Prim;
            Obj.Items (Item_Idx).Key.ID := Key_ID;
            return;

         end if;

      end loop For_Each_Item;
      raise Program_Error;

   end Submit_Primitive_Key_ID;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Item_Index_Type)
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Invalid then

            Obj.Items (Item_Idx).State := Pending;
            Obj.Items (Item_Idx).Prim := Prim;
            Obj.Items (Item_Idx).Key.ID := Key_ID;

            Data_Idx := Item_Idx;
            return;

         end if;

      end loop For_Each_Item;
      raise Program_Error;

   end Submit_Primitive;

   --
   --  Submit_Completed_Primitive
   --
   procedure Submit_Completed_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Item_Index_Type)
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Invalid then

            Obj.Items (Item_Idx).State := Complete;
            Obj.Items (Item_Idx).Prim := Prim;
            Obj.Items (Item_Idx).Key.ID := Key_ID;

            Data_Idx := Item_Idx;
            return;

         end if;

      end loop For_Each_Item;
      raise Program_Error;

   end Submit_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj      :     Object_Type;
      Item_Idx : out Item_Index_Type;
      Prim     : out Primitive.Object_Type)
   is
   begin

      For_Each_Item :
      for Idx in Obj.Items'Range loop

         if Obj.Items (Idx).State = Pending then

            Prim := Obj.Items (Idx).Prim;
            Item_Idx := Idx;
            return;

         end if;

      end loop For_Each_Item;

      Prim := Primitive.Invalid_Object;
      Item_Idx := Item_Index_Type'First;

   end Peek_Generated_Primitive;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj      : in out Object_Type;
      Item_Idx :        Item_Index_Type)
   is
   begin

      if Obj.Items (Item_Idx).State /= Pending then
         raise Program_Error;
      end if;
      Obj.Items (Item_Idx).State := In_Progress;

   end Drop_Generated_Primitive;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj      : Object_Type;
      Item_Idx : Item_Index_Type)
   return Key_ID_Type
   is
   begin

      if Obj.Items (Item_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Items (Item_Idx).Key.ID;

   end Peek_Generated_Key_ID;

   --
   --  Peek_Generated_Key
   --
   function Peek_Generated_Key (
      Obj      : Object_Type;
      Item_Idx : Item_Index_Type)
   return Key_Plaintext_Type
   is
   begin

      if Obj.Items (Item_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Items (Item_Idx).Key;

   end Peek_Generated_Key;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Complete then
            return Obj.Items (Item_Idx).Prim;
         end if;

      end loop For_Each_Item;
      return Primitive.Invalid_Object;

   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Obj.Items (Item_Idx).State = Complete then
            Obj.Items (Item_Idx).State := Invalid;
            return;
         end if;

      end loop For_Each_Item;

   end Drop_Completed_Primitive;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj      : in out Object_Type;
      Item_Idx :        Item_Index_Type;
      Success  :        Boolean)
   is
   begin

      if Obj.Items (Item_Idx).State /= In_Progress then
         raise Program_Error;
      end if;

      Obj.Items (Item_Idx).State := Complete;
      Primitive.Success (Obj.Items (Item_Idx).Prim, Success);

   end Mark_Completed_Primitive;

   --
   --  Data_Index
   --
   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Item_Index_Type
   is
   begin

      For_Each_Item :
      for Item_Idx in Obj.Items'Range loop

         if Primitive.Equal (Obj.Items (Item_Idx).Prim, Prim) then
            return Item_Idx;
         end if;

      end loop For_Each_Item;
      raise Program_Error;

   end Data_Index;

end CBE.Crypto;
