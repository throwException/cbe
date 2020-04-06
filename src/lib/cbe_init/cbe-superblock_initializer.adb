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

package body CBE.Superblock_Initializer
with SPARK_Mode
is
   function Valid_Snap_Slot (Obj : Object_Type)
   return Snapshot_Type
   is
      Snap : Snapshot_Type;
   begin

      Snap.Hash        := Obj.VBD.Hash;
      Snap.PBA         := Obj.VBD.PBA;
      Snap.Gen         := 0;
      Snap.Nr_Of_Leafs := Obj.VBD_Nr_Of_Leafs;
      Snap.Max_Level   := Obj.VBD_Max_Lvl_Idx;
      Snap.Valid       := True;
      Snap.ID          := 0;
      Snap.Keep        := False;
      return Snap;

   end Valid_Snap_Slot;

   function Valid_SB_Slot (Obj : Object_Type)
   return Superblock_Type
   is
      SB : Superblock_Type;
   begin
      For_Snapshots :
      for Idx in Snapshots_Index_Type loop
         if Idx = Snapshots_Index_Type'First then
            SB.Snapshots (Idx) := Valid_Snap_Slot (Obj);
         else
            SB.Snapshots (Idx) := Snapshot_Invalid;
         end if;
      end loop For_Snapshots;

      SB.State                   := Normal;
      SB.Current_Key             := Key_Valid (Obj.Key_ID);
      SB.Previous_Key            := Key_Invalid;
      SB.Curr_Snap               := 0;
      SB.Degree                  := Obj.VBD_Degree;
      SB.Last_Secured_Generation := 0;
      SB.Free_Gen                := 0;
      SB.Free_Number             := Obj.FT.PBA;
      SB.Free_Hash               := Obj.FT.Hash;
      SB.Free_Max_Level          := Obj.FT_Max_Lvl_Idx;
      SB.Free_Degree             := Obj.FT_Degree;
      SB.Free_Leafs              := Obj.FT_Nr_Of_Leafs;
      SB.Meta_Gen                := 0;
      SB.Meta_Number             := Obj.MT.PBA;
      SB.Meta_Hash               := Obj.MT.Hash;
      SB.Meta_Max_Level          := Obj.MT_Max_Lvl_Idx;
      SB.Meta_Degree             := Obj.MT_Degree;
      SB.Meta_Leafs              := Obj.MT_Nr_Of_Leafs;
      return SB;

   end Valid_SB_Slot;

   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj.Submitted_Prim := Primitive.Invalid_Object;
      Obj.Execute_Progress := False;
      Obj.SB_Slot_State := Init;
      Obj.SB_Slot_Idx := Superblocks_Index_Type'First;
      Obj.SB_Slot := Superblock_Invalid;
      Obj.VBD := Type_1_Node_Invalid;
      Obj.VBD_Max_Lvl_Idx := Tree_Level_Index_Type'First;
      Obj.VBD_Degree := Tree_Degree_Type'First;
      Obj.VBD_Nr_Of_Leafs := Tree_Number_Of_Leafs_Type'First;
      Obj.FT := Type_1_Node_Invalid;
      Obj.FT_Max_Lvl_Idx := Tree_Level_Index_Type'First;
      Obj.FT_Degree := Tree_Degree_Type'First;
      Obj.FT_Nr_Of_Leafs := Tree_Number_Of_Leafs_Type'First;
      Obj.MT := Type_1_Node_Invalid;
      Obj.MT_Max_Lvl_Idx := Tree_Level_Index_Type'First;
      Obj.MT_Degree := Tree_Degree_Type'First;
      Obj.MT_Nr_Of_Leafs := Tree_Number_Of_Leafs_Type'First;
      Obj.Generated_Prim := Primitive.Invalid_Object;
   end Initialize_Object;

   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (not Primitive.Valid (Obj.Submitted_Prim));

   procedure Submit_Primitive (
      Obj             : in out Object_Type;
      Prim            :        Primitive.Object_Type;
      Key_ID          :        Key_ID_Type;
      VBD_Max_Lvl_Idx :        Tree_Level_Index_Type;
      VBD_Degree      :        Tree_Degree_Type;
      VBD_Nr_Of_Leafs :        Tree_Number_Of_Leafs_Type;
      FT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      FT_Degree       :        Tree_Degree_Type;
      FT_Nr_Of_Leafs  :        Tree_Number_Of_Leafs_Type;
      MT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      MT_Degree       :        Tree_Degree_Type;
      MT_Nr_Of_Leafs  :        Tree_Number_Of_Leafs_Type)
   is
   begin
      if Primitive.Valid (Obj.Submitted_Prim) then
         raise Program_Error;
      end if;
      Obj.Submitted_Prim := Prim;
      Obj.SB_Slot_State := Init;
      Obj.Key_ID := Key_ID;
      Obj.VBD_Max_Lvl_Idx := VBD_Max_Lvl_Idx;
      Obj.VBD_Degree := VBD_Degree;
      Obj.VBD_Nr_Of_Leafs := VBD_Nr_Of_Leafs;
      Obj.FT_Max_Lvl_Idx := FT_Max_Lvl_Idx;
      Obj.FT_Degree := FT_Degree;
      Obj.FT_Nr_Of_Leafs := FT_Nr_Of_Leafs;
      Obj.MT_Max_Lvl_Idx := MT_Max_Lvl_Idx;
      Obj.MT_Degree := MT_Degree;
      Obj.MT_Nr_Of_Leafs := MT_Nr_Of_Leafs;

      pragma Debug (
         Debug.Print_String (
            "[sb_init] slot " &
            Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
            ", init"));

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

      case Obj.SB_Slot_State is
      when Init =>

         if Obj.SB_Slot_Idx = Superblocks_Index_Type'First then

            Obj.SB_Slot_State := VBD_Request_Started;
            Obj.Generated_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_SB_Init_VBD_Init, 0, 0);

            Obj.Execute_Progress := True;

            pragma Debug (
               Debug.Print_String (
                  "[sb_init] slot " &
                  Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                  ", vbd started"));

         else

            Obj.SB_Slot := Superblock_Invalid;
            Obj.SB_Slot_State := Write_Request_Started;
            Obj.Generated_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_SB_Init_Blk_IO,
                  Block_Number_Type (Obj.SB_Slot_Idx), 0);

            Obj.Execute_Progress := True;

            pragma Debug (
               Debug.Print_String (
                  "[sb_init] slot " &
                  Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                  ", write started"));

         end if;

      when VBD_Request_Done =>

         Obj.Generated_Prim :=
            Primitive.Valid_Object_No_Pool_Idx (
               Write, False, Primitive.Tag_SB_Init_FT_Init,
               Block_Number_Type (Obj.SB_Slot_Idx), 0);

         Obj.SB_Slot_State := FT_Request_Started;
         Obj.Execute_Progress := True;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", ft started"));

      when FT_Request_Done =>

         Obj.SB_Slot := Valid_SB_Slot (Obj);
         Obj.Generated_Prim :=
            Primitive.Valid_Object_No_Pool_Idx (
               Write, False, Primitive.Tag_SB_Init_MT_Init,
               Block_Number_Type (Obj.SB_Slot_Idx), 0);

         Obj.SB_Slot_State := MT_Request_Started;
         Obj.Execute_Progress := True;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt started"));

      when MT_Request_Done =>

         Obj.SB_Slot := Valid_SB_Slot (Obj);
         Obj.Generated_Prim :=
            Primitive.Valid_Object_No_Pool_Idx (
               Write, False, Primitive.Tag_SB_Init_Blk_IO,
               Block_Number_Type (Obj.SB_Slot_Idx), 0);

         Obj.SB_Slot_State := Write_Request_Started;
         Obj.Execute_Progress := True;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", write started"));

      when Write_Request_Done =>

         Obj.SB_Slot_State := Done;
         Obj.Execute_Progress := True;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", done"));

      when Done =>

         if Obj.SB_Slot_Idx < Superblocks_Index_Type'Last then

            Obj.SB_Slot_Idx := Obj.SB_Slot_Idx + 1;
            Obj.SB_Slot_State := Init;
            Obj.Execute_Progress := True;

            pragma Debug (
               Debug.Print_String (
                  "[sb_init] slot " &
                  Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
                  ", init"));

         end if;

      when others =>

         null;

      end case;

   end Execute;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function Peek_Generated_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      case Obj.SB_Slot_State is
      when Write_Request_Started => Obj.Generated_Prim,
      when VBD_Request_Started => Obj.Generated_Prim,
      when FT_Request_Started => Obj.Generated_Prim,
      when MT_Request_Started => Obj.Generated_Prim,
      when others => Primitive.Invalid_Object);

   function Peek_Generated_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type
   is
      Data : Block_Data_Type;
   begin
      case Obj.SB_Slot_State is
      when Write_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Block_Data_From_Superblock (Data, Obj.SB_Slot);
         return Data;

      when others =>

         raise Program_Error;

      end case;
   end Peek_Generated_Data;

   function Peek_Generated_Max_Lvl_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.VBD_Max_Lvl_Idx;

      when FT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.FT_Max_Lvl_Idx;

      when MT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.MT_Max_Lvl_Idx;

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
      when VBD_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.VBD_Degree - 1);

      when FT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.FT_Degree - 1);

      when MT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Tree_Child_Index_Type (Obj.MT_Degree - 1);

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
      when VBD_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.VBD_Nr_Of_Leafs;

      when FT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.FT_Nr_Of_Leafs;

      when MT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         return Obj.MT_Nr_Of_Leafs;

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
      when Write_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := Write_Request_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", write dropped"));

      when FT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := FT_Request_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", ft dropped"));

      when MT_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := MT_Request_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt dropped"));

      when VBD_Request_Started =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := VBD_Request_Dropped;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", vbd dropped"));

      when others =>

         raise Program_Error;

      end case;
   end Drop_Generated_Primitive;

   procedure Mark_Generated_Blk_IO_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when Write_Request_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.SB_Slot_State := Write_Request_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", write done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_Blk_IO_Primitive_Complete;

   procedure Mark_Generated_VBD_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      VBD  :        Type_1_Node_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when VBD_Request_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.VBD := VBD;
         Obj.SB_Slot_State := VBD_Request_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", vbd done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_VBD_Init_Primitive_Complete;

   procedure Mark_Generated_FT_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      FT   :        Type_1_Node_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when FT_Request_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.FT := FT;
         Obj.SB_Slot_State := FT_Request_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", ft done"));

      when MT_Request_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.MT := FT;
         Obj.SB_Slot_State := MT_Request_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_FT_Init_Primitive_Complete;

   procedure Mark_Generated_MT_Init_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      MT   :        Type_1_Node_Type)
   is
   begin
      case Obj.SB_Slot_State is
      when MT_Request_Dropped =>

         if not Primitive.Equal (Obj.Generated_Prim, Prim) then
            raise Program_Error;
         end if;
         Obj.MT := MT;
         Obj.SB_Slot_State := MT_Request_Done;

         pragma Debug (
            Debug.Print_String (
               "[sb_init] slot " &
               Debug.To_String (Debug.Uint64_Type (Obj.SB_Slot_Idx)) &
               ", mt done"));

      when others =>

         raise Program_Error;

      end case;
   end Mark_Generated_MT_Init_Primitive_Complete;

end CBE.Superblock_Initializer;
