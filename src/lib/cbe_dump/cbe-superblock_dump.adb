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

package body CBE.Superblock_Dump
with SPARK_Mode
is
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj.Submitted_Prim := Primitive.Invalid_Object;
      Obj.Submitted_Cfg := Dump_Configuration_Default;
      Obj.Execute_Progress := False;
      Obj.State := Inspect_SBs;
      Obj.Highest_Gen := Generation_Type'First;
      Obj.SB_Slot_State := Init;
      Obj.SB_Slot_Idx := Superblocks_Index_Type'First;
      Obj.SB_Slot := Superblock_Invalid;
      Obj.Snap_Idx := Snapshots_Index_Type'First;
      Obj.VBD := Type_1_Node_Invalid;
      Obj.FT := Type_1_Node_Invalid;
      Obj.MT := Type_1_Node_Invalid;
      Obj.Generated_Prim := Primitive.Invalid_Object;
   end Initialize_Object;

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (not Primitive.Valid (Obj.Submitted_Prim));

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Cfg  :        Dump_Configuration_Type)
   is
   begin
      if Primitive.Valid (Obj.Submitted_Prim) then
         raise Program_Error;
      end if;
      Initialize_Object (Obj);
      Obj.Submitted_Prim := Prim;
      Obj.Submitted_Cfg := Cfg;

   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if not Primitive.Valid (Obj.Submitted_Prim) or else
         Obj.SB_Slot_Idx < Superblocks_Index_Type'Last or else
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
         Obj.SB_Slot_Idx < Superblocks_Index_Type'Last or else
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
                  Read, False, Primitive.Tag_SB_Dump_Blk_IO,
                  Block_Number_Type (Obj.SB_Slot_Idx), 0);

            Obj.Execute_Progress := True;

         when Read_Done =>

            if Superblock_Valid (Obj.SB_Slot) and then
               Obj.SB_Slot.Snapshots (Obj.SB_Slot.Curr_Snap).Gen >
                  Obj.Highest_Gen
            then
               Obj.Highest_Gen :=
                  Obj.SB_Slot.Snapshots (Obj.SB_Slot.Curr_Snap).Gen;
            end if;
            if Obj.SB_Slot_Idx < Superblocks_Index_Type'Last then
               Obj.SB_Slot_Idx := Obj.SB_Slot_Idx + 1;
               Obj.SB_Slot_State := Init;
               Obj.Execute_Progress := True;
            else
               Obj.State := Dump_SBs;
               Obj.SB_Slot_Idx := Superblocks_Index_Type'First;
               Obj.SB_Slot_State := Init;
               Obj.Execute_Progress := True;
            end if;

         when others =>

            null;

         end case;

      when Dump_SBs =>

         case Obj.SB_Slot_State is
         when Init =>

            Obj.SB_Slot_State := Read_Started;
            Obj.Generated_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_SB_Dump_Blk_IO,
                  Block_Number_Type (Obj.SB_Slot_Idx), 0);

            Obj.Execute_Progress := True;

         when Read_Done =>

            if not Superblock_Valid (Obj.SB_Slot) or else
               Dump_Cfg_Max_Superblocks_Type (
                 Obj.Highest_Gen -
                 Obj.SB_Slot.Snapshots (Obj.SB_Slot.Curr_Snap).Gen) + 1 >
                 Obj.Submitted_Cfg.Max_Superblocks
            then

               Obj.SB_Slot_State := Done;
               Obj.Execute_Progress := True;

            elsif Superblock_Valid (Obj.SB_Slot) then

               if Obj.Snap_Idx = Snapshots_Index_Type'First then

                  Debug.Print_String_Buffered (
                     Superblock_XML_Tag_Open (Obj.SB_Slot) &
                     Debug.Line_Feed);

               end if;

               if not Obj.Submitted_Cfg.VBD then

                  Obj.SB_Slot_State := VBD_Dump_Done;
                  Obj.Execute_Progress := True;

               elsif Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Valid then

                  if Dump_Cfg_Max_Snapshots_Type (
                        Obj.Highest_Gen -
                        Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Gen) + 1 <=
                     Obj.Submitted_Cfg.Max_Snapshots
                  then
                     Debug.Print_String_Buffered (
                        Debug.Tabulator &
                        Snap_XML_Tag_Open (
                           Obj.SB_Slot.Snapshots (Obj.Snap_Idx),
                           Obj.Snap_Idx, Obj.Submitted_Cfg.Hashes) &
                        Debug.Line_Feed);

                     Obj.Generated_Prim :=
                        Primitive.Valid_Object_No_Pool_Idx (
                           Read, False, Primitive.Tag_SB_Dump_VBD_Dump,
                           Block_Number_Type (
                              Obj.SB_Slot.Snapshots (Obj.Snap_Idx).PBA),
                           0);

                     Obj.SB_Slot_State := VBD_Dump_Started;
                     Obj.Execute_Progress := True;
                  else
                     Obj.SB_Slot_State := VBD_Dump_Done;
                     Obj.Execute_Progress := True;
                  end if;

               else

                  if Obj.Submitted_Cfg.Unused_Nodes then

                     Debug.Print_String_Buffered (
                        Debug.Tabulator &
                        Snap_XML_Tag_Invalid (Obj.Snap_Idx) &
                        Debug.Line_Feed);

                  end if;

                  Obj.SB_Slot_State := VBD_Dump_Done;
                  Obj.Execute_Progress := True;

               end if;

            else

               if Obj.Submitted_Cfg.Unused_Nodes then

                  Debug.Print_String_Buffered (
                     Superblock_XML_Tag_Invalid &
                     Debug.Line_Feed);

               end if;

               Obj.SB_Slot_State := Done;
               Obj.Execute_Progress := True;

            end if;

         when VBD_Dump_Done =>

            if Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Valid and then
               Obj.Submitted_Cfg.VBD and then
               Dump_Cfg_Max_Snapshots_Type (
                  Obj.Highest_Gen -
                  Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Gen) + 1 <=
               Obj.Submitted_Cfg.Max_Snapshots
            then
               Debug.Print_String_Buffered (
                  Debug.Tabulator & Snap_XML_Tag_Close & Debug.Line_Feed);
            end if;

            if Obj.Snap_Idx < Snapshots_Index_Type'Last then

               Obj.Snap_Idx := Obj.Snap_Idx + 1;
               Obj.SB_Slot_State := Read_Done;
               Obj.Execute_Progress := True;

            elsif not Obj.Submitted_Cfg.Free_Tree then

               Obj.Snap_Idx := Snapshots_Index_Type'First;
               Obj.SB_Slot_State := FT_Dump_Done;
               Obj.Execute_Progress := True;

            else

               Obj.Snap_Idx := Snapshots_Index_Type'First;

               Debug.Print_String_Buffered (
                  Debug.Tabulator &
                  Free_Tree_XML_Tag_Open (
                     Obj.SB_Slot, Obj.Submitted_Cfg.Hashes) &
                  Debug.Line_Feed);

               Obj.Generated_Prim :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_SB_Dump_FT_Dump,
                     Block_Number_Type (Obj.SB_Slot.Free_Number), 0);

               Obj.SB_Slot_State := FT_Dump_Started;
               Obj.Execute_Progress := True;

            end if;

         when FT_Dump_Done =>

            if Obj.Submitted_Cfg.Free_Tree then

               Debug.Print_String_Buffered (
                  Debug.Tabulator &
                  Free_Tree_XML_Tag_Close & Debug.Line_Feed);

            end if;

            if Obj.Submitted_Cfg.Meta_Tree then

               Debug.Print_String_Buffered (
                  Debug.Tabulator &
                  Meta_Tree_XML_Tag_Open (
                     Obj.SB_Slot, Obj.Submitted_Cfg.Hashes) &
                  Debug.Line_Feed);

               Obj.Generated_Prim :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_SB_Dump_MT_Dump,
                     Block_Number_Type (Obj.SB_Slot.Free_Number), 0);

               Obj.SB_Slot_State := MT_Dump_Started;
               Obj.Execute_Progress := True;

            else

               Obj.SB_Slot_State := Done;
               Obj.Execute_Progress := True;

            end if;

         when MT_Dump_Done =>

            if Obj.Submitted_Cfg.Meta_Tree then

               Debug.Print_String_Buffered (
                  Debug.Tabulator &
                  Meta_Tree_XML_Tag_Close & Debug.Line_Feed);

            end if;

               Debug.Print_String_Buffered (
                  Superblock_XML_Tag_Close & Debug.Line_Feed);

            Obj.SB_Slot_State := Done;
            Obj.Execute_Progress := True;

         when Done =>

            if Obj.SB_Slot_Idx < Superblocks_Index_Type'Last then

               Obj.SB_Slot_Idx := Obj.SB_Slot_Idx + 1;
               Obj.SB_Slot_State := Init;
               Obj.Execute_Progress := True;

            end if;

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
      when VBD_Dump_Started => Obj.Generated_Prim,
      when FT_Dump_Started => Obj.Generated_Prim,
      when MT_Dump_Started => Obj.Generated_Prim,
      when others => Primitive.Invalid_Object);

   function Peek_Generated_Root (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return (
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).PBA,
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Gen,
            Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Hash);

      when FT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return (
            Obj.SB_Slot.Free_Number,
            Obj.SB_Slot.Free_Gen,
            Obj.SB_Slot.Free_Hash);

      when MT_Dump_Started =>

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
      when VBD_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Max_Level;

      when FT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Free_Max_Level;

      when MT_Dump_Started =>

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
      when VBD_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.SB_Slot.Degree - 1);

      when FT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.SB_Slot.Free_Degree - 1);

      when MT_Dump_Started =>

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
      when VBD_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Snapshots (Obj.Snap_Idx).Nr_Of_Leafs;

      when FT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.SB_Slot.Free_Leafs;

      when MT_Dump_Started =>

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

      when FT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := FT_Dump_Dropped;

      when MT_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := MT_Dump_Dropped;

      when VBD_Dump_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := VBD_Dump_Dropped;

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

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_Blk_IO_Primitive_Complete;

   procedure Mark_Generated_VBD_Dump_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Dump_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := VBD_Dump_Done;

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_VBD_Dump_Primitive_Complete;

   procedure Mark_Generated_FT_Dump_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      FT   :        Type_1_Node_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when FT_Dump_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.FT := FT;
         Obj.SB_Slot_State := FT_Dump_Done;

      when MT_Dump_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.MT := FT;
         Obj.SB_Slot_State := MT_Dump_Done;

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_FT_Dump_Primitive_Complete;

end CBE.Superblock_Dump;
