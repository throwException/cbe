--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

generic

   type Index_Type is range <>;
   type Number_Of_Slots_Type is range <>;

package CBE.Generic_Index_Queue
with SPARK_Mode
is
   pragma Pure;

   type Queue_Type is private;

   --
   --  Empty_Queue
   --
   function Empty_Queue
   return Queue_Type;

   --
   --  Enqueue
   --
   procedure Enqueue (
      Obj : in out Queue_Type;
      Idx :        Index_Type);

   --
   --  Next_Item
   --
   function Next_Item (
      Obj : Queue_Type;
      Idx : Index_Type)
   return Index_Type;

   --
   --  Move_One_Item_Towards_Tail
   --
   procedure Move_One_Item_Towards_Tail (
      Obj : in out Queue_Type;
      Idx :        Index_Type);

   --
   --  Item_Is_Tail
   --
   function Item_Is_Tail (
      Obj : Queue_Type;
      Idx : Index_Type)
   return Boolean;

   --
   --  Dequeue
   --
   procedure Dequeue (
      Obj : in out Queue_Type;
      Idx :        Index_Type);

   --
   --  Empty
   --
   function Empty (Obj : Queue_Type)
   return Boolean;

   --
   --  Full
   --
   function Full (Obj : Queue_Type)
   return Boolean;

   --
   --  Head
   --
   function Head (Obj : Queue_Type)
   return Index_Type;

private

   type Slots_Index_Type is new Index_Type;

   type Slots_Type is array (Slots_Index_Type) of Index_Type;

   --
   --  Queue_Type
   --
   type Queue_Type is record
      Head             : Slots_Index_Type;
      Tail             : Slots_Index_Type;
      Nr_Of_Used_Slots : Number_Of_Slots_Type;
      Slots            : Slots_Type;
   end record;

end CBE.Generic_Index_Queue;
