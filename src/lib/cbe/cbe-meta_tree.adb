--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;
with SHA256_4K;

--
--  The meta-tree module handles the allocation and freeing of meta-data
--  free-tree inner nodes.
--
package body CBE.Meta_Tree
with SPARK_Mode
is
   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type)
   is
      SHA_Idx : SHA256_4K.Hash_Index_Type := SHA256_4K.Hash_Index_Type'First;
   begin
      for CBE_Idx in CBE_Hash'Range loop
         CBE_Hash (CBE_Idx) := Byte_Type (SHA_Hash (SHA_Idx));
         if CBE_Idx < CBE_Hash'Last then
            SHA_Idx := SHA_Idx + 1;
         end if;
      end loop;
   end CBE_Hash_From_SHA256_4K_Hash;

   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type)
   is
      CBE_Idx : Block_Data_Index_Type := Block_Data_Index_Type'First;
   begin
      for SHA_Idx in SHA_Data'Range loop
         SHA_Data (SHA_Idx) := SHA256_4K.Byte (CBE_Data (CBE_Idx));
         if SHA_Idx < SHA_Data'Last then
            CBE_Idx := CBE_Idx + 1;
         end if;
      end loop;
   end SHA256_4K_Data_From_CBE_Data;

   procedure Compute_Node_Hash (
      Block_Data    : in     Block_Data_Type;
      SHA_Hash_Data : in out SHA256_4K.Data_Type;
      CBE_Hash      :    out Hash_Type);

   procedure Initialized_Object (Obj : in out Object_Type)
   is
   begin
      Obj.State         := Invalid;
      Obj.Root_Node     := (
         PBA => PBA_Invalid,
         Gen => 0,
         Hash => (others => 0));
      Obj.Root_Dirty    := False;
      Obj.Tree_Geom     := (
         Max_Level => 0,
         Edges     => Tree_Max_Degree,
         Leafs     => 0);
      Obj.Current_Gen   := 0;
      Obj.Old_PBA       := PBA_Invalid;
      Obj.Complete_Prim := Primitive.Invalid_Object;
      Obj.Finished      := False;

      Obj.Cache_Request := Invalid_Cache_Request;

      Initialize_Type_1_Info_Array (Obj.Level_N_Nodes);
      Obj.Level_1_Node := Type_2_Info_Invalid;

   end Initialized_Object;

   ------------------------
   --  Module interface  --
   ------------------------

   function Request_Acceptable (Obj : Object_Type) return Boolean
   is (Obj.State = Invalid);

   procedure Submit_Request (
      Obj         : in out Object_Type;
      Root_Node   :        Type_1_Node_Type;
      Tree_Geom   :        Tree_Geometry_Type;
      Current_Gen :        Generation_Type;
      Old_PBA     :        Physical_Block_Address_Type)
   is
   begin
      if Obj.State /= Invalid then
         raise Program_Error;
      end if;

      Obj.Root_Node     := Root_Node;
      Obj.Tree_Geom     := Tree_Geom;
      Obj.Current_Gen   := Current_Gen;
      Obj.Old_PBA       := Old_PBA;
      Obj.Complete_Prim := Primitive.Invalid_Object;
      Obj.Finished      := False;

      Initialize_Type_1_Info_Array (Obj.Level_N_Nodes);
      Obj.Level_1_Node := Type_2_Info_Invalid;

      Obj.Level_N_Nodes (Obj.Tree_Geom.Max_Level).Node := Obj.Root_Node;
      Obj.Level_N_Nodes (Obj.Tree_Geom.Max_Level).State := Read;
      Obj.Level_N_Nodes (Obj.Tree_Geom.Max_Level).Volatile :=
         Node_Volatile (Obj.Root_Node, Obj.Current_Gen);

      Debug.Print_String ("MT: " & "Submit_Request: "
         & "root PBA: " & Debug.To_String (Obj.Root_Node.PBA)
         & "PBA: " & Debug.To_String (Obj.Old_PBA));

      Obj.State := Update;
   end Submit_Request;

   --
   --  Execute module
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      if Obj.Cache_Request.State /= Invalid then
         return;
      end if;

      if Obj.State /= Invalid then
         Debug.Print_String ("MT: " & "Execute: " & "state: " & To_String (Obj.State));
      end if;

      case Obj.State is
         when Invalid =>
            null;
         when Update =>
            Execute_Update (Obj, Progress);
         when Complete =>
            null;
         when Tree_Hash_Mismatch =>
            raise Program_Error;
      end case;
   end Execute;

   --
   --  Check for any completed primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State /= Complete then
         return Primitive.Invalid_Object;
      end if;
      Debug.Print_String ("MT: Peek_Completed_Primitive: "
         & Primitive.To_String (Obj.Complete_Prim));
      return Obj.Complete_Prim;
   end Peek_Completed_Primitive;

   --
   --  Get the current root node
   --
   function Peek_Completed_Root_Node (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin
      if Obj.State /= Complete
         or else not Primitive.Equal (Prim, Obj.Complete_Prim)
      then
         raise Program_Error;
      end if;
      return Obj.Root_Node;
   end Peek_Completed_Root_Node;

   --
   --  Discard given completed primitive
   --
   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.State /= Complete
         or else not Primitive.Equal (Prim, Obj.Complete_Prim)
      then
         raise Program_Error;
      end if;

      Obj.State := Invalid;
   end Drop_Completed_Primitive;

   --
   --  Get generated meta-data tree primitive
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.Cache_Request.State = Pending then
         return Obj.Cache_Request.Prim;
      end if;
      return Primitive.Invalid_Object;
   end Peek_Generated_Cache_Primitive;

   function Peek_Generated_Cache_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type
   is
   begin
      if Obj.Cache_Request.State = Pending
         and then Primitive.Operation (Prim) = Write
      then
         return Obj.Cache_Request.Block_Data;
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Data;

   --
   --  Discard generated meta-data tree primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.Cache_Request.State /= Pending
         or else not Primitive.Equal (Prim, Obj.Cache_Request.Prim)
      then
         raise Program_Error;
      end if;

      Obj.Cache_Request.State := In_Progress;
   end Drop_Generated_Cache_Primitive;

   --
   --  Mark generated meta-data tree primitive as complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj        : in out Object_Type;
      Prim       :        Primitive.Object_Type;
      Block_Data : in     Block_Data_Type)
   is
   begin
      if Obj.Cache_Request.State /= In_Progress
         or else not Primitive.Equal (Prim, Obj.Cache_Request.Prim)
      then
         raise Program_Error;
      end if;

      if not Primitive.Success (Prim) then

         Debug.Print_String ("Mark_Generated_Cache_Primitive_Complete: FAILED");
         Obj.Complete_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => Request.Success_Type (False),
            Tg     => Primitive.Tag_FT_MT,
            Blk_Nr => 0,
            Idx    => 0);
         Obj.State := Complete;
         return;
      end if;

      Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: "
         & "Level : " & Debug.To_String (Debug.Uint64_Type (Obj.Cache_Request.Level)));

      case Primitive.Operation (Obj.Cache_Request.Prim) is
         when Sync =>

            Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: " & "SYNC");
            null;

         when Read =>

            Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: " & "READ");

            if Obj.Cache_Request.Level > Type_2_Level then
               if not Check_Node_Hash (Block_Data,
                  Obj.Level_N_Nodes (Obj.Cache_Request.Level).Node.Hash)
               then
                  Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: T1 " & "Tree_Hash_Mismatch " & To_String (Obj.Level_N_Nodes (Obj.Cache_Request.Level)));
                  Obj.State := Tree_Hash_Mismatch;
               else
                  Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: T1");
                  Type_1_Node_Block_From_Block_Data (
                     Obj.Level_N_Nodes (Obj.Cache_Request.Level).Entries,
                     Block_Data);
                  Obj.Level_N_Nodes (Obj.Cache_Request.Level).Index := 0;
                  Obj.Level_N_Nodes (Obj.Cache_Request.Level).State :=
                     Read_Complete;
               end if;
            elsif Obj.Cache_Request.Level = Type_2_Level then
               if not Check_Node_Hash (Block_Data, Obj.Level_1_Node.Node.Hash)
               then
                  Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: T2 " & "Tree_Hash_Mismatch " & To_String (Obj.Level_1_Node));
                  Obj.State := Tree_Hash_Mismatch;
               else
                  Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: T2");
                  Type_2_Node_Block_From_Block_Data (
                     Obj.Level_1_Node.Entries,
                     Block_Data);
                  Obj.Level_1_Node.Index := 0;
                  Obj.Level_1_Node.State := Read_Complete;
               end if;
            else
               raise Program_Error;
            end if;

         when Write =>

            if Obj.Cache_Request.Level > Type_2_Level then

               Obj.Level_N_Nodes (Obj.Cache_Request.Level).State := Write_Complete;

            elsif Obj.Cache_Request.Level = Type_2_Level then

               Obj.Level_1_Node.State := Write_Complete;
            else
               raise Program_Error;
            end if;
      end case;

      Obj.Cache_Request := Invalid_Cache_Request;
   end Mark_Generated_Cache_Primitive_Complete;

   --
   --  private
   --

   function To_String (S : Info_State_Type) return String
   is (case S is
      when Invalid => "Invalid",
      when Read => "Read",
      when Read_Complete => "Read_Complete",
      when Write => "Write",
      when Write_Complete => "Write_Complete",
      when Complete => "Complete");

   function To_String (T : Type_1_Node_Type) return String
   is ("Type_1:"
      & " PBA: " & Debug.To_String (T.PBA)
      & " Gen: " & Debug.To_String (T.Gen));

   function To_String (T : Type_1_Info_Type) return String
   is ("Type_1_Info:"
      & " state: " & To_String (T.State)
      & " Node: " & To_String (T.Node)
      & " index: " & Debug.To_String (Debug.Uint64_Type (T.Index))
      & " volatile: " & Debug.To_String (T.Volatile));

   function To_String (T : Type_2_Info_Type) return String
   is ("Type_2_Info:"
      & " state: " & To_String (T.State)
      & " Node: " & To_String (T.Node)
      & " index: " & Debug.To_String (Debug.Uint64_Type (T.Index))
      & " volatile: " & Debug.To_String (T.Volatile));

   function To_String (S : State_Type) return String
   is (case S is
      when Invalid => "Invalid",
      when Update => "Update",
      when Complete => "Complete",
      when Tree_Hash_Mismatch => "Tree_Hash_Mismatch");

   procedure Initialize_Type_1_Info_Array (
      A : in out Type_1_Info_Array_Type)
   is
   begin
      for I in A'Range loop
         A (I) := Type_1_Info_Invalid;
      end loop;
   end Initialize_Type_1_Info_Array;

   procedure Exchange_NV_Level_1_Node (
      Obj       : in out Object_Type;
      T2_Entry  : in out Type_2_Node_Type;
      Exchanged :    out Boolean)
   is
      PBA : constant Physical_Block_Address_Type := Obj.Level_1_Node.Node.PBA;
   begin
      Exchanged := False;

      Debug.Print_String ("MT: " & "Exchange_NV_Level_1_Node: " & To_String (Obj.Level_1_Node));
      if not Obj.Level_1_Node.Volatile then

         Obj.Level_1_Node.Node.PBA := T2_Entry.PBA;
         Obj.Level_1_Node.Volatile := True;

         T2_Entry.PBA       := PBA;
         T2_Entry.Alloc_Gen := Obj.Current_Gen;
         T2_Entry.Free_Gen  := Obj.Current_Gen;
         T2_Entry.Reserved  := False;

         Exchanged := True;
      end if;
   end Exchange_NV_Level_1_Node;

   procedure Exchange_NV_Inner_Nodes (
      Obj       : in out Object_Type;
      T2_Entry  : in out Type_2_Node_Type;
      Exchanged :    out Boolean)
   is
      T1_Entry : Type_1_Node_Type := Type_1_Node_Invalid;
   begin
      Exchanged := False;

      Loop_Non_Volatile_Inner_Nodes :
      --  for L in Obj.Level_N_Nodes'Range loop
      for L in Obj.Level_N_Nodes'Range loop

         Debug.Print_String ("MT: " & "Exchange_NV_Inner_Nodes: " & To_String (Obj.Level_N_Nodes (L)));

         if Obj.Level_N_Nodes (L).Node /= Type_1_Node_Invalid
            and then not Obj.Level_N_Nodes (L).Volatile
         then
            T1_Entry := Obj.Level_N_Nodes (L).Node;
            Obj.Level_N_Nodes (L).Node.PBA := T2_Entry.PBA;
            Obj.Level_N_Nodes (L).Node.Gen := Obj.Current_Gen;
            Obj.Level_N_Nodes (L).Volatile := True;

            T2_Entry.PBA       := T1_Entry.PBA;
            T2_Entry.Alloc_Gen := Obj.Current_Gen;
            T2_Entry.Free_Gen  := Obj.Current_Gen;
            T2_Entry.Reserved  := False;

            --  Obj.Level_1_Node.Entries (Natural (Index)) := T2_Entry;
            Exchanged := True;
            exit Loop_Non_Volatile_Inner_Nodes;
         end if;
      end loop Loop_Non_Volatile_Inner_Nodes;
   end Exchange_NV_Inner_Nodes;

   procedure Exchange_Request_PBA (
      Obj      : in out Object_Type;
      T2_Entry : in out Type_2_Node_Type)
   is
   begin
      Debug.Print_String ("MT: " & "Exchange_Request_PBA: " & Debug.To_String (Obj.Old_PBA) & " -> " & Debug.To_String (T2_Entry.PBA));

      Obj.Complete_Prim := Primitive.Valid_Object_No_Pool_Idx (
         Op     => Read,
         Succ   => Request.Success_Type (True),
         Tg     => Primitive.Tag_FT_MT,
         Blk_Nr => Block_Number_Type (T2_Entry.PBA),
         Idx    => 0);

      Obj.Finished := True;

      T2_Entry.PBA       := Obj.Old_PBA;
      T2_Entry.Alloc_Gen := Obj.Current_Gen;
      T2_Entry.Free_Gen  := Obj.Current_Gen;
      T2_Entry.Reserved  := False;
   end Exchange_Request_PBA;

   procedure Handle_Level_0_Nodes (
      Obj     : in out Object_Type;
      Handled :    out Boolean)
   is
      TMP_T2_Entry : Type_2_Node_Type := Type_2_Node_Invalid;
   begin
      Handled := False;
      Debug.Print_String ("MT: " & "Handle_Level_0_Nodes: Parent_Node: " & To_String (Obj.Level_1_Node.Node));

      Loop_Level_0_Nodes :
      for I in 0 .. Obj.Tree_Geom.Edges loop
         TMP_T2_Entry := Obj.Level_1_Node.Entries (Natural (I));
         Debug.Print_String ("MT: " & "Handle_Level_0_Nodes: " & Debug.To_String (Debug.Uint64_Type (I)) & ": " & Debug.To_String (TMP_T2_Entry.PBA));

         if TMP_T2_Entry /= Type_2_Node_Invalid
            and then Check_Level_0_Usable (Obj.Current_Gen, TMP_T2_Entry)
         then
            declare
               Exchanged_Level_1 : Boolean := False;
               Exchanged_Level_N : Boolean := False;
               Exchanged_Request_PBA : Boolean := False;
            begin

               --  first try to exchange the level 1 node ...
               Exchange_NV_Level_1_Node (Obj, TMP_T2_Entry, Exchanged_Level_1);

               --  ... next the inner level N nodes ...
               if not Exchanged_Level_1 then
                  Exchange_NV_Inner_Nodes (Obj, TMP_T2_Entry, Exchanged_Level_N);
               end if;

               --  ... and than satisfy the original MT request
               if not Exchanged_Level_1 and then not Exchanged_Level_N then
                  Exchange_Request_PBA (Obj, TMP_T2_Entry);
                  Exchanged_Request_PBA := True;
               end if;

               Debug.Print_String ("MT: " & "Handle_Level_0_Nodes: " & Debug.To_String (Debug.Uint64_Type (I))
                  & " Exchanged_Level_1: " & Debug.To_String (Exchanged_Level_1)
                  & " Exchanged_Level_N: " & Debug.To_String (Exchanged_Level_N)
                  & " Exchanged_Request_PBA: " & Debug.To_String (Exchanged_Request_PBA)
                  & " => " & Debug.To_String (TMP_T2_Entry.PBA));

               Obj.Level_1_Node.Entries (Natural (I)) := TMP_T2_Entry;
               Handled := True;

               exit Loop_Level_0_Nodes when Exchanged_Request_PBA;
            end;
         end if;
      end loop Loop_Level_0_Nodes;
   end Handle_Level_0_Nodes;

   procedure Handle_Level_1_Node (
      Obj     : in out Object_Type;
      Handled :    out Boolean)
   is
   begin
      Debug.Print_String ("MT: Handle_Level_1_Node: " & To_String (Obj.Level_1_Node));
      case Obj.Level_1_Node.State is
         when Invalid =>

            Handled := False;

         when Read =>

            Obj.Cache_Request := New_Cache_Request (
               Obj.Level_1_Node.Node.PBA, Read, 1, (others => 0));
            Handled := True;

         when Read_Complete =>

            Handle_Level_0_Nodes (Obj, Handled);
            if Handled then
               Debug.Print_String ("MT: Handle_Level_1_Node: 0 WRITE");
               Obj.Level_1_Node.State := Write;
            else
               Debug.Print_String ("MT: Handle_Level_1_Node: 0 COMPLETE");
               Obj.Level_1_Node.State := Complete;
               Handled := True;
            end if;

         when Write =>
            Debug.Print_String ("MT: Handle_Level_1_Node: " & To_String (Obj.Level_1_Node));

            Debug.Print_String ("MT: " & "Mark_Generated_Cache_Primitive_Complete: WRITE T2: " & To_String (Obj.Level_1_Node)
            & " UPDATE: " & To_String (Obj.Level_N_Nodes (Obj.Level_N_Nodes'First)));

            declare
               Block_Data : Block_Data_Type := (others => 0);
            begin
               Block_Data_From_Type_2_Node_Block (Block_Data,
                  Obj.Level_1_Node.Entries);

               Update_Parent (Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Entries (Natural (Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Index)),
                  Block_Data, Obj.Current_Gen, Obj.Level_1_Node.Node.PBA);

               Obj.Cache_Request := New_Cache_Request (
                  Obj.Level_1_Node.Node.PBA, Write, 1, Block_Data);
            end;

            Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Dirty := True;

            Handled := True;

         when Write_Complete =>

            Debug.Print_String ("MT: Handle_Level_1_Node: " & To_String (Obj.Level_1_Node));

            Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Index :=
               Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Index + 1;

            Obj.Level_1_Node.State := Invalid;
            Handled := True;

         when Complete =>

            Debug.Print_String ("MT: Handle_Level_1_Node: " & To_String (Obj.Level_1_Node));

            Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Index :=
               Obj.Level_N_Nodes (Obj.Level_N_Nodes'First).Index + 1;

            Obj.Level_1_Node.State := Invalid;
            Handled := True;

      end case;
   end Handle_Level_1_Node;

   procedure Handle_Level_N_Nodes (
      Obj     : in out Object_Type;
      Handled :    out Boolean)
   is
   begin
      Loop_Level_N_Nodes :
      for L in Obj.Level_N_Nodes'Range loop
         Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L)) & " state: " & To_String (Obj.Level_N_Nodes (L).State));
         case Obj.Level_N_Nodes (L).State is
            when Invalid =>
               null;
            when Read =>

               Obj.Cache_Request := New_Cache_Request (
                  Obj.Level_N_Nodes (L).Node.PBA, Read, L, (others => 0));

               Handled := True;
               exit Loop_Level_N_Nodes;

            when Read_Complete =>

               Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L))
                  & " state: " & To_String (Obj.Level_N_Nodes (L).State)
                  & " current index: " & Debug.To_String (Debug.Uint64_Type (Obj.Level_N_Nodes (L).Index)));

               if Obj.Level_N_Nodes (L).Index < Node_Index_Type (Obj.Tree_Geom.Edges)
                  and then Obj.Level_N_Nodes (L).Entries (Natural (
                     Obj.Level_N_Nodes (L).Index)) /= Type_1_Node_Invalid
                  and then not Obj.Finished
               then
                  if L /= Obj.Level_N_Nodes'First then
                     Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L))
                        & " state: " & To_String (Obj.Level_N_Nodes (L).State) & " READ NEXT Level: " & Debug.To_String (Debug.Uint64_Type (L - 1)));
                     Obj.Level_N_Nodes (L - 1) := (
                        State    => Read,
                        Node     => Obj.Level_N_Nodes (L).Entries (Natural (
                           Obj.Level_N_Nodes (L).Index)),
                        Entries  => (others => Type_1_Node_Invalid),
                        Index    => 0,
                        Dirty    => False,
                        Volatile => Node_Volatile (
                           Obj.Level_N_Nodes (L).Node, Obj.Current_Gen));
                  else
                     Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L))
                        & " state: " & To_String (Obj.Level_N_Nodes (L)) & " READ NEXT LEAF");

                     Obj.Level_1_Node := (
                        State    => Read,
                        Node      => Obj.Level_N_Nodes (L).Entries (Natural (
                           Obj.Level_N_Nodes (L).Index)),
                        Entries  => (others => Type_2_Node_Invalid),
                        Index    => 0,
                        Volatile => Node_Volatile (
                           Obj.Level_N_Nodes (L).Node, Obj.Current_Gen));
                     Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L))
                        & " state: " & To_String (Obj.Level_N_Nodes (L)) & " READ NEXT LEAF: " & To_String (Obj.Level_1_Node));
                  end if;
               else
                  Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L)) & " state: " & To_String (Obj.Level_N_Nodes (L).State) & " FINISHED");
                  if Obj.Level_N_Nodes (L).Dirty then
                     Obj.Level_N_Nodes (L).State := Write;
                  else
                     Obj.Level_N_Nodes (L).State := Complete;
                  end if;
               end if;
               Handled := True;
               exit Loop_Level_N_Nodes;

            when Write =>

               Debug.Print_String ("MT: Handle_Level_N_Nodes: Level: " & Debug.To_String (Debug.Uint64_Type (L)) & " state: " & To_String (Obj.Level_N_Nodes (L).State) & " FINISHED");

               declare
                  Block_Data : Block_Data_Type := (others => 0);
               begin
                  Block_Data_From_Type_1_Node_Block (Block_Data,
                     Obj.Level_N_Nodes (L).Entries);

                  if L = Obj.Tree_Geom.Max_Level then

                     Debug.Print_String ("MT: " & "Handle_Level_N_Nodes: WRITE T1: " & To_String (Obj.Level_N_Nodes (L))
                        & " UPDATE: " & To_String (Obj.Root_Node));

                     Update_Parent (
                        Obj.Root_Node,
                        Block_Data, Obj.Current_Gen,
                        Obj.Level_N_Nodes (L).Node.PBA);

                     Obj.Root_Dirty := True;

                  else

                     Debug.Print_String ("MT: " & "Handle_Level_N_Nodes: WRITE T1: " & To_String (Obj.Level_N_Nodes (L))
                        & " UPDATE: " & To_String (Obj.Level_N_Nodes (L + 1)));

                     Update_Parent (
                        Obj.Level_N_Nodes (L + 1).Entries (Natural (Obj.Level_N_Nodes (L + 1).Index)),
                        Block_Data, Obj.Current_Gen,
                        Obj.Level_N_Nodes (L).Node.PBA);
                     Obj.Level_N_Nodes (L + 1).Dirty := True;
                  end if;

                  Obj.Cache_Request := New_Cache_Request (
                     Obj.Level_N_Nodes (L).Node.PBA, Write, L, Block_Data);
               end;

               Handled := True;
               exit Loop_Level_N_Nodes;

            when Write_Complete =>

               Debug.Print_String ("MT: Handle_Level_N_Nodes: Write_Complete: Level: " & Debug.To_String (Debug.Uint64_Type (L)));
               if Tree_Level_Index_Type (L) = Obj.Tree_Geom.Max_Level then
                  Obj.State := Complete;
               else
                  Obj.Level_N_Nodes (L + 1).Index :=
                     Obj.Level_N_Nodes (L + 1).Index + 1;
               end if;

               Obj.Cache_Request := Invalid_Cache_Request;

               Obj.Level_N_Nodes (L).State := Invalid;
               Handled := True;
               exit Loop_Level_N_Nodes;

            when Complete =>

               Debug.Print_String ("MT: Handle_Level_N_Nodes: Complete: Level: " & Debug.To_String (Debug.Uint64_Type (L)));
               if Tree_Level_Index_Type (L) = Obj.Tree_Geom.Max_Level then
                  Obj.State := Complete;
               else
                  Obj.Level_N_Nodes (L + 1).Index :=
                     Obj.Level_N_Nodes (L + 1).Index + 1;
               end if;

               Obj.Level_N_Nodes (L).State := Invalid;
               Handled := True;
               exit Loop_Level_N_Nodes;

         end case;
      end loop Loop_Level_N_Nodes;
   end Handle_Level_N_Nodes;

   procedure Execute_Update (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Handled_Level_1_Node  : Boolean := False;
      Handled_Level_N_Nodes : Boolean := False;
   begin
      Debug.Print_String ("MT: Execute_Update: Leve1: 1 " & To_String (Obj.Level_1_Node));
      Dump_Level_N_Nodes :
      for L in Obj.Level_N_Nodes'Range loop
         Debug.Print_String ("MT: Execute_Update: Level: " & Debug.To_String (Debug.Uint64_Type (L)) & " " & To_String (Obj.Level_N_Nodes (L)));
      end loop Dump_Level_N_Nodes;

      Handle_Level_1_Node (Obj, Handled_Level_1_Node);
      if Handled_Level_1_Node then
         Progress := True;
         return;
      end if;

      Handle_Level_N_Nodes (Obj, Handled_Level_N_Nodes);
      Progress := Progress or else Handled_Level_N_Nodes;
   end Execute_Update;

   procedure Update_Parent (
      Node       : in out Type_1_Node_Type;
      Block_Data : in     Block_Data_Type;
      Gen        :        Generation_Type;
      PBA  :              Physical_Block_Address_Type)
   is
      SHA_Hash_Data : SHA256_4K.Data_Type;
   begin
      Compute_Node_Hash (Block_Data, SHA_Hash_Data, Node.Hash);
      Debug.Print_String ("MT: Update_Parent:"
         & " Node.Gen: " & Debug.To_String (Node.Gen) & " -> " & Debug.To_String (Gen)
         & " Node.PBA: " & Debug.To_String (Node.PBA) & " -> " & Debug.To_String (PBA));
      Node.Gen := Gen;
      Node.PBA := PBA;
   end Update_Parent;

   function Check_Level_0_Usable (
      Gen  : Generation_Type;
      Node : Type_2_Node_Type)
   return Boolean
   is (Node.Alloc_Gen /= Gen);

   procedure Compute_Node_Hash (
      Block_Data    : in     Block_Data_Type;
      SHA_Hash_Data : in out SHA256_4K.Data_Type;
      CBE_Hash      :    out Hash_Type)
   is
      SHA_Hash : SHA256_4K.Hash_Type;
   begin
      SHA256_4K_Data_From_CBE_Data (SHA_Hash_Data, Block_Data);
      SHA256_4K.Hash (SHA_Hash_Data, SHA_Hash);
      CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
   end Compute_Node_Hash;

   function Check_Node_Hash (
      Block_Data : Block_Data_Type;
      Node_Hash  : Hash_Type)
   return Boolean
   is
      SHA_Hash_Data : SHA256_4K.Data_Type;
      Computed_Hash : Hash_Type;
   begin
      Compute_Node_Hash (Block_Data, SHA_Hash_Data, Computed_Hash);

      if Computed_Hash = Node_Hash then
         return True;
      else
         Debug.Print_String ("Check_Node_Hash: "
            & " exp: " & Debug.To_String (Node_Hash)
            & " got: " & Debug.To_String (Computed_Hash));
         return False;
      end if;
   end Check_Node_Hash;

   function Block_From_Level_1_Node (Entries : Type_2_Node_Block_Type)
   return Block_Data_Type
   is
      Block_Data : Block_Data_Type;
   begin
      Block_Data_From_Type_2_Node_Block (Block_Data, Entries);
      return Block_Data;
   end Block_From_Level_1_Node;

   function Block_From_Level_N_Node (Entries : Type_1_Node_Block_Type)
   return Block_Data_Type
   is
      Block_Data : Block_Data_Type;
   begin
      Block_Data_From_Type_1_Node_Block (Block_Data, Entries);
      return Block_Data;
   end Block_From_Level_N_Node;

end CBE.Meta_Tree;
