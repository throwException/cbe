--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Generic_Index_Queue
with SPARK_Mode
is
   --
   --  Empty_Queue
   --
   function Empty_Queue
   return Queue_Type
   is (
      Head             => Slots_Index_Type'First,
      Tail             => Slots_Index_Type'First,
      Nr_Of_Used_Slots => Number_Of_Slots_Type'First,
      Slots            => (others => Index_Type'First));

   --
   --  Enqueue
   --
   procedure Enqueue (
      Obj : in out Queue_Type;
      Idx :        Index_Type)
   is
   begin
      if Full (Obj) then
         raise Program_Error;
      end if;

      Obj.Slots (Obj.Tail) := Idx;

      if Obj.Tail < Slots_Index_Type'Last then
         Obj.Tail := Slots_Index_Type'Succ (Obj.Tail);
      else
         Obj.Tail := Slots_Index_Type'First;
      end if;

      Obj.Nr_Of_Used_Slots := Number_Of_Slots_Type'Succ (Obj.Nr_Of_Used_Slots);

   end Enqueue;

   --
   --  Dequeue
   --
   procedure Dequeue (
      Obj : in out Queue_Type;
      Idx :        Index_Type)
   is
   begin
      if Empty (Obj) or else
         Head (Obj) /= Idx
      then
         raise Program_Error;
      end if;

      if Obj.Head < Slots_Index_Type'Last then
         Obj.Head := Slots_Index_Type'Succ (Obj.Head);
      else
         Obj.Head := Slots_Index_Type'First;
      end if;

      Obj.Nr_Of_Used_Slots := Number_Of_Slots_Type'Pred (Obj.Nr_Of_Used_Slots);

   end Dequeue;

   --
   --  Empty
   --
   function Empty (Obj : Queue_Type)
   return Boolean
   is (Obj.Nr_Of_Used_Slots = 0);

   --
   --  Full
   --
   function Full (Obj : Queue_Type)
   return Boolean
   is (Obj.Nr_Of_Used_Slots = Number_Of_Slots_Type'Last);

   --
   --  Head
   --
   function Head (Obj : Queue_Type)
   return Index_Type
   is
   begin
      if Empty (Obj) then
         raise Program_Error;
      end if;

      return Obj.Slots (Obj.Head);

   end Head;

end CBE.Generic_Index_Queue;
