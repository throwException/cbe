--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;

pragma Unreferenced (CBE.Debug);

package body CBE.Superblock_Check
with SPARK_Mode
is
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj.Submitted_Prim := Primitive.Invalid_Object;
      Obj.Execute_Progress := False;
      Obj.State := Inspect_SBs;
      Obj.Highest_Gen := Generation_Type'First;
      Obj.SB_Slot_State := Init;
      Obj.SB_Slot_Idx := Superblocks_Index_Type'First;
      Obj.SB_Slot := Superblock_Invalid;
      Obj.Snap_Idx := Snapshots_Index_Type'First;
      Obj.VBD := Type_1_Node_Invalid;
      Obj.FT := Type_1_Node_Invalid;
      Obj.Generated_Prim := Primitive.Invalid_Object;
   end Initialize_Object;

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (not Primitive.Valid (Obj.Submitted_Prim));

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Primitive.Valid (Obj.Submitted_Prim) then
         raise Program_Error;
      end if;
      Obj.Submitted_Prim := Prim;
      Obj.SB_Slot_State := Init;

      pragma Debug (
         Debug.Print_String (
            "[sb_check] slot " &
            Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
            ", init"));

   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if not Primitive.Valid (Obj.Submitted_Prim) or else
         Obj.SB_Slot_State /= Done
      then
         return Primitive.Invalid_Object;
      end if;
      return Obj.Submitted_Prim;
   end Peek_Completed_Primitive;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if not Primitive.Valid (Obj.Submitted_Prim) or else
         not Primitive.Equal (Obj.Submitted_Prim, Prim) or else
         Obj.SB_Slot_State /= Done
      then
         raise Program_Error;
      end if;
      Initialize_Object (Obj);

   end Drop_Completed_Primitive;

   procedure Execute (Obj : in out Object_Type)
   is
   begin
      Obj.Execute_Progress := False;

      if not Primitive.Valid (Obj.Submitted_Prim) then
         return;
      end if;

      case Obj.State is
      when Inspect_SBs =>

         case Obj.SB_Slot_State is
         when Init =>

            Obj.SB_Slot_State := Read_Started;
            Obj.Generated_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_SB_Check_Blk_IO,
                  Block_Number_Type (Obj.SB_Slot_Idx), 0);

            Obj.Execute_Progress := True;

         when Read_Done =>

            if Superblock_Valid (Obj.SB_Slot) and then
               Obj.SB_Slot.Snapshots (Obj.SB_Slot.Curr_Snap).Gen >
                  Obj.Highest_Gen
            then
               Obj.Highest_Gen :=
                  Obj.SB_Slot.Snapshots (Obj.SB_Slot.Curr_Snap).Gen;
               Obj.Last_SB_Slot_Idx := Obj.SB_Slot_Idx;
            end if;
            if Obj.SB_Slot_Idx < Superblocks_Index_Type'Last then
               Obj.SB_Slot_Idx := Obj.SB_Slot_Idx + 1;
               Obj.SB_Slot_State := Init;
               Obj.Execute_Progress := True;
            else
               Obj.State := Check_SB;
               Obj.SB_Slot_Idx := Obj.Last_SB_Slot_Idx;
               Obj.SB_Slot_State := Init;
               Obj.Execute_Progress := True;

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] check slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx))));
            end if;

         when others =>

            null;

         end case;

      when Check_SB =>

         case Obj.SB_Slot_State is
         when Init =>

            Obj.SB_Slot_State := Read_Started;
            Obj.Generated_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_SB_Check_Blk_IO,
                  Block_Number_Type (Obj.SB_Slot_Idx), 0);

            Obj.Execute_Progress := True;

            pragma Debug (
               Debug.Print_String (
                  "[sb_check] slot " &
                  Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                  ", read started"));

         when Read_Done =>

            if Superblock_Valid (Obj.SB_Slot) then

               if Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Valid then

                  Obj.SB_Slot_State := VBD_Check_Started;
                  Obj.Generated_Prim :=
                     Primitive.Valid_Object_No_Pool_Idx (
                        Read, False, Primitive.Tag_SB_Check_VBD_Check,
                        Block_Number_Type (
                           Obj.SB_Slot.Snapshots (Obj.Snap_Idx).PBA),
                        0);

                  Obj.Execute_Progress := True;

                  pragma Debug (
                     Debug.Print_String (
                        "[sb_check] slot " &
                        Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                        ", snap " &
                        Debug.To_String (Debug.Uint64_Type (Obj.Snap_Idx)) &
                        " started"));

               else

                  pragma Debug (
                     Debug.Print_String (
                        "[sb_check] slot " &
                        Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                        ", snap " &
                        Debug.To_String (Debug.Uint64_Type (Obj.Snap_Idx)) &
                        " done, not in use"));

                  Obj.SB_Slot_State := VBD_Check_Done;
                  Obj.Execute_Progress := True;

               end if;

            else

               Obj.SB_Slot_State := Done;
               Obj.Execute_Progress := True;

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", done, not in use"));

            end if;

         when VBD_Check_Done =>

            if Obj.Snap_Idx < Snapshots_Index_Type'Last then

               Obj.Snap_Idx := Obj.Snap_Idx + 1;
               Obj.SB_Slot_State := Read_Done;
               Obj.Execute_Progress := True;

            else

               Obj.Snap_Idx := Snapshots_Index_Type'First;
               Obj.Generated_Prim :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_SB_Check_FT_Check,
                     Block_Number_Type (Obj.SB_Slot.Free_Number), 0);

               Obj.SB_Slot_State := FT_Check_Started;
               Obj.Execute_Progress := True;

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", ft started"));

            end if;

         when FT_Check_Done =>

            if Obj.FT.Hash = Obj.SB_Slot.Free_Hash then

               Obj.SB_Slot_State := MT_Check_Started;
               Obj.Execute_Progress := True;

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", done"));

            else

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", ft hash mismatch"));

               raise Program_Error;

            end if;

         when MT_Check_Done =>

            if Obj.MT.Hash = Obj.SB_Slot.Meta_Hash then

               Obj.SB_Slot_State := Done;
               Obj.Execute_Progress := True;

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", done"));

            else

               pragma Debug (
                  Debug.Print_String (
                     "[sb_check] slot " &
                     Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                     ", mt hash mismatch"));

               raise Program_Error;

            end if;

         when Done =>

            null;

         when others =>

            null;

         end case;

      end case;

   end Execute;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      case Obj.SB_Slot_State is
      when Read_Started => Obj.Generated_Prim,
      when VBD_Check_Started => Obj.Generated_Prim,
      when FT_Check_Started => Obj.Generated_Prim,
      when MT_Check_Started => Obj.Generated_Prim,
      when others => Primitive.Invalid_Object);

   function Peek_Generated_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return (
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).PBA,
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Gen,
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Hash);

      when FT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return (
            Obj.SB_Slot.Free_Number,
            Obj.SB_Slot.Free_Gen,
            Obj.SB_Slot.Free_Hash);

      when MT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return (
            Obj.SB_Slot.Meta_Number,
            Obj.SB_Slot.Meta_Gen,
            Obj.SB_Slot.Meta_Hash);

      when others =>

         raise Program_Error;

      end case;
   end Peek_Generated_Root;

   function Peek_Generated_Max_Lvl_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Max_Level;

      when FT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Free_Max_Level;

      when MT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Meta_Max_Level;

      when others =>

         raise Program_Error;

      end case;
   end Peek_Generated_Max_Lvl_Idx;

   function Peek_Generated_Max_Child_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Child_Index_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.SB_Slot.Degree - 1);

      when FT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.SB_Slot.Free_Degree - 1);

      when MT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.SB_Slot.Meta_Degree - 1);

      when others =>

         raise Program_Error;

      end case;
   end Peek_Generated_Max_Child_Idx;

   function Peek_Generated_Nr_Of_Leafs (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Nr_Of_Leafs;

      when FT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Free_Leafs;

      when MT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Meta_Leafs;

      when others =>

         raise Program_Error;

      end case;
   end Peek_Generated_Nr_Of_Leafs;

   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when Read_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := Read_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", read dropped"));

      when FT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := FT_Check_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", ft dropped"));

      when MT_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := MT_Check_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt dropped"));

      when VBD_Check_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := VBD_Check_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", snap " &
               Debug.To_String (Debug.Uint64_Type (Obj.Snap_Idx)) &
               " dropped"));

      when others =>

         raise Program_Error;

      end case;
   end Drop_Generated_Primitive;

   procedure Mark_Generated_Blk_IO_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when Read_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := Read_Done;
         Superblock_From_Block_Data (Obj.SB_Slot, Data);

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", read done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_Blk_IO_Primitive_Complete;

   procedure Mark_Generated_VBD_Check_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Check_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := VBD_Check_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", snap " &
               Debug.To_String (Debug.Uint64_Type (Obj.Snap_Idx)) &
               " done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_VBD_Check_Primitive_Complete;

   procedure Mark_Generated_FT_Check_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      FT   :        Type_1_Node_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when FT_Check_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.FT := FT;
         Obj.SB_Slot_State := FT_Check_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", ft done"));

      when MT_Check_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.MT := FT;
         Obj.SB_Slot_State := MT_Check_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_check] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_FT_Check_Primitive_Complete;

end CBE.Superblock_Check;
