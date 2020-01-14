--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with SHA256_4K;
with CBE.Request;
with CBE.Debug;
pragma Unreferenced (CBE.Debug);

package body CBE.Free_Tree
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

   procedure Initialize_Object (
      Obj     : out Object_Type;
      Rt_PBA  :     Physical_Block_Address_Type;
      Rt_Gen  :     Generation_Type;
      Rt_Hash :     Hash_Type;
      Max_Lvl :     Tree_Level_Index_Type;
      Degr    :     Tree_Degree_Type;
      Lfs     :     Tree_Number_Of_Leafs_Type)
   is
   begin
      Obj := Initialized_Object (
         Rt_PBA, Rt_Gen, Rt_Hash, Max_Lvl, Degr, Lfs);
   end Initialize_Object;

   function Initialized_Object (
      Rt_PBA  :     Physical_Block_Address_Type;
      Rt_Gen  :     Generation_Type;
      Rt_Hash :     Hash_Type;
      Max_Lvl :     Tree_Level_Index_Type;
      Degr    :     Tree_Degree_Type;
      Lfs     :     Tree_Number_Of_Leafs_Type)
   return Object_Type
   is
      Tr_Helper : constant Tree_Helper.Object_Type :=
         Tree_Helper.Initialized_Object (Degr, Max_Lvl, Lfs);
   begin
      return (
         Trans_Helper       => Tr_Helper,
         Trans              =>
            Translation.Initialized_Object (Tr_Helper, True),
         Execute_Progress   => False,
         Do_WB              => False,
         WB_Done            => False,
         Nr_Of_Blocks       => 0,
         Nr_Of_Found_Blocks => 0,
         Query_Branches     => (others => Query_Branch_Invalid),
         Curr_Query_Branch  => 0,
         Found_PBAs         => (others => PBA_Invalid),
         Free_PBAs          => (others => PBA_Invalid),
         Root_PBA           => Rt_PBA,
         Root_Hash          => Rt_Hash,
         Root_Gen           => Rt_Gen,
         Curr_Query_Prim    => Primitive.Invalid_Object,
         Curr_Type_2        => IO_Entry_Invalid,
         WB_IOs             => (others => IO_Entry_Invalid),
         WB_Data            => Write_Back_Data_Invalid,
         Curr_Query_VBA     => 0,
         Cache_Prim         => Primitive.Invalid_Object,
         Cache_Prim_State   => Invalid,
         Cache_Prim_Data    => (others => 0),
         Cache_Prim_Lvl     => 0,
         Update_State       => Inactive,
         Update_Curr_Branch => 0,
         Update_Curr_Lvl    => 0,
         Update_Data        => (others => 0),
         Update_Pre_Data    => (others => 0));
   end Initialized_Object;

   procedure Retry_Allocation (Obj : in out Object_Type)
   is
   begin
      Reset_Query_Prim (Obj);
      Obj.Update_State     := Inactive;
      Obj.Do_WB            := False;
      Obj.WB_Done          := False;
      Obj.WB_Data.Finished := False;

   end Retry_Allocation;

   procedure Reset_Query_Prim (Obj : in out Object_Type)
   is
   begin
      Obj.Nr_Of_Found_Blocks := 0;

      Obj.Curr_Query_Prim := Primitive.Valid_Object_No_Pool_Idx (
         Op     => Read,
         Succ   => Request.Success_Type (False),
         Tg     => Primitive.Tag_Free_Tree_Query,
         Blk_Nr => 0,
         Idx    => 0);

      Obj.Curr_Query_VBA := 0;

      --
      --  Reset query branches
      --
      Obj.Curr_Query_Branch  := 0;
      For_Query_Branches :
      for Branch_ID in Obj.Query_Branches'Range loop

         --
         --  FIXME may this be replaced by Query_Branch_Invalid
         --        or at least something like Reset_Query_Branch?
         --
         Obj.Query_Branches (Branch_ID).VBA := VBA_Invalid;
         Obj.Query_Branches (Branch_ID).Nr_Of_Free_Blocks := 0;

         For_Query_Branch_PBAs :
         for PBA_ID in Obj.Query_Branches (Branch_ID).PBAs'Range loop
            Obj.Query_Branches (Branch_ID).PBAs (PBA_ID) := PBA_Invalid;
         end loop For_Query_Branch_PBAs;

      end loop For_Query_Branches;
   end Reset_Query_Prim;

   function Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (Obj.Nr_Of_Blocks = 0);

   procedure Submit_Request (
      Obj            : in out Object_Type;
      Curr_Gen       :        Generation_Type;
      Nr_Of_Blks     :        Number_Of_Blocks_Type;
      New_PBAs       :        Write_Back.New_PBAs_Type;
      Old_PBAs       :        Type_1_Node_Walk_Type;
      Tree_Max_Level :        Tree_Level_Index_Type;
      Fr_PBAs        :        Free_PBAs_Type;
      Req_Prim       :        Primitive.Object_Type;
      VBA            :        Virtual_Block_Address_Type)
   is
   begin
      if Obj.Nr_Of_Blocks /= 0 then
         return;
      end if;

      Obj.Nr_Of_Blocks       := Nr_Of_Blks;
      Obj.Nr_Of_Found_Blocks := 0;
      Obj.Curr_Type_2        := IO_Entry_Invalid;

      Obj.Update_State     := Inactive;
      Obj.Do_WB            := False;
      Obj.WB_Done          := False;
      Obj.WB_Data.Finished := False;

      For_WB_IOs :
      for WB_IO_ID in Obj.WB_IOs'Range loop
         Obj.WB_IOs (WB_IO_ID).State := Invalid;
      end loop For_WB_IOs;

      Reset_Query_Prim (Obj);

      --
      --  Prepare the write-back data that is used later on by
      --  the Write_back module.
      --
      Obj.WB_Data.Prim           := Req_Prim;
      Obj.WB_Data.Gen            := Curr_Gen;
      Obj.WB_Data.VBA            := VBA;
      Obj.WB_Data.Tree_Max_Level := Tree_Max_Level;

      --
      --  Store given lists in the module.
      --
      --  (The free and old PBA lists are part of the write-back data
      --  as we have to pass them on to the Write_back module b/c it
      --  needs the addresses for the updating the nodes.
      --
      --  Also putting the lists into a proper structure would allow
      --  for statically size match checking...)
      --
      Obj.WB_Data.New_PBAs := New_PBAs;
      Obj.WB_Data.Old_PBAs := Old_PBAs;
      Obj.Free_PBAs        := Fr_PBAs;

   end Submit_Request;

   function Leaf_Usable (
      Active_Snaps     : Snapshots_Type;
      Last_Secured_Gen : Generation_Type;
      Node             : Type_2_Node_Type)
   return Boolean
   is
      Free   : Boolean := False;
      In_Use : Boolean := False;
   begin
      --  FIXME check could be done outside
      if  not Node.Reserved then
         return True;
      end if;

      Declare_Generations :
      declare
         F_Gen : constant Generation_Type := Node.Free_Gen;
         A_Gen : constant Generation_Type := Node.Alloc_Gen;
         S_Gen : constant Generation_Type := Last_Secured_Gen;
      begin
         --
         --  If the node was freed before the last secured generation,
         --  check if there is a active snapshot that might be using the node,
         --  i.e., its generation is after the allocation generation and
         --  before the free generation.
         --
         if F_Gen <= S_Gen then
            For_Active_Snaps :
            for Snap of Active_Snaps loop
               if Snap.Valid then
                  Declare_B_Generation :
                  declare
                     B_Gen   : constant Generation_Type := Snap.Gen;
                     Is_Free : constant Boolean :=
                        (F_Gen <= B_Gen or else A_Gen >= (B_Gen + 1));
                  begin
                     In_Use := In_Use or else not Is_Free;
                     exit For_Active_Snaps when In_Use;
                  end Declare_B_Generation;
               end if;
            end loop For_Active_Snaps;
            Free := not In_Use;
         end if;
      end Declare_Generations;
      return Free;

   end Leaf_Usable;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      if Obj.Cache_Prim_State = Generated then
         Obj.Cache_Prim
      else
         Primitive.Invalid_Object);

   --
   --  Peek_Generated_Cache_Lvl
   --
   function Peek_Generated_Cache_Lvl (Obj : Object_Type)
   return Tree_Level_Index_Type
   is
   begin
      if Obj.Cache_Prim_State = Generated then
         return Obj.Cache_Prim_Lvl;
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Lvl;

   --
   --  Peek_Generated_Cache_Data
   --
   function Peek_Generated_Cache_Data (Obj : Object_Type)
   return Block_Data_Type
   is
   begin
      if Obj.Cache_Prim_State = Generated then
         return Obj.Cache_Prim_Data;
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Data;

   --
   --  Drop_Generated_Cache_Primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.Cache_Prim_State = Generated and then
         Primitive.Equal (Obj.Cache_Prim, Prim)
      then
         Obj.Cache_Prim_State := Dropped;
      else
         raise Program_Error;
      end if;
   end Drop_Generated_Cache_Primitive;

   --
   --  Mark_Generated_Cache_Primitive_Complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type)
   is
   begin
      if Obj.Cache_Prim_State = Dropped and then
         Primitive.Equal (Obj.Cache_Prim, Prim)
      then
         Obj.Cache_Prim_State := Complete;
         Obj.Cache_Prim_Data := Data;
         Obj.Cache_Prim := Prim;
      else
         raise Program_Error;
      end if;
   end Mark_Generated_Cache_Primitive_Complete;

   procedure Execute_Translation_Generated_Prims (
      Obj        : in out Object_Type;
      Trans_Data : in out Translation_Data_Type)
   is
      Cache_Prim : Primitive.Object_Type;
      Trans_Prim : constant Primitive.Object_Type :=
         Translation.Peek_Generated_Primitive (Obj.Trans);
   begin
      if Primitive.Valid (Trans_Prim) then

         Cache_Prim :=
            Primitive.Valid_Object_No_Pool_Idx (
               Read, False, Primitive.Tag_FT_Cache,
               Primitive.Block_Number (Trans_Prim), 0);

         if Obj.Cache_Prim_State = Invalid then

            Obj.Cache_Prim_State := Generated;
            Obj.Cache_Prim := Cache_Prim;
            Obj.Cache_Prim_Lvl :=
               Translation.Peek_Generated_Level (Obj.Trans);

            Obj.Execute_Progress := True;

         elsif Obj.Cache_Prim_State = Complete and then
               Primitive.Equal (Obj.Cache_Prim, Cache_Prim)
         then

            if not Primitive.Success (Obj.Cache_Prim) then
               raise Program_Error;
            end if;
            Translation.Mark_Generated_Primitive_Complete (
               Obj.Trans, Obj.Cache_Prim_Data, Trans_Data);

            Translation.Discard_Generated_Primitive (Obj.Trans);
            Obj.Cache_Prim_State := Invalid;
            Obj.Execute_Progress := True;

         end if;
      end if;
   end Execute_Translation_Generated_Prims;

   procedure Execute_Translation (
      Obj        : in out Object_Type;
      Trans_Data : in out Translation_Data_Type)
   is
      X : Natural := 1;
   begin
      --
      --  Submit new request for querying a branch in the FT
      --
      Loop_Query_FT_Branch :
      loop
         exit Loop_Query_FT_Branch when
            not Translation.Acceptable (Obj.Trans) or else
            not Primitive.Valid (Obj.Curr_Query_Prim);

         Translation.Submit_Primitive (
            Obj.Trans, Obj.Root_PBA, Obj.Root_Gen, Obj.Root_Hash,
            Obj.Curr_Query_Prim);

         --
         --  Make the query primitive invalid after we successfully submitted
         --  the request to trigger the break above. It will be made valid
         --  again for next query.
         --
         Obj.Curr_Query_Prim := Primitive.Invalid_Object;
         Obj.Execute_Progress := True;

      end loop Loop_Query_FT_Branch;

      Translation.Execute (Obj.Trans, Trans_Data);
      if Translation.Execute_Progress (Obj.Trans) then
         Obj.Execute_Progress := True;
      end if;

      Execute_Translation_Generated_Prims (Obj, Trans_Data);

      Loop_Handle_Trans_Completed_Prim :
      loop
         Declare_Completed_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Translation.Peek_Completed_Primitive (Obj.Trans);
         begin
            exit Loop_Handle_Trans_Completed_Prim when
               not Primitive.Valid (Prim);

            X := X + 1;
            if X = 50 then
               raise Program_Error;
            end if;

            --
            --  Translation has finished, we have the PBA of the type 2
            --  node, request nodes data.
            --
            Obj.Curr_Type_2 := (
               PBA =>
                  Physical_Block_Address_Type (Primitive.Block_Number (Prim)),
               State => Pending);

            Declare_Cache_Prim :
            declare
               Cache_Prim : constant Primitive.Object_Type :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_FT_Cache,
                     Block_Number_Type (Obj.Curr_Type_2.PBA), 0);
            begin
               if Obj.Cache_Prim_State = Invalid then

                  Obj.Cache_Prim_State := Generated;
                  Obj.Cache_Prim := Cache_Prim;
                  Obj.Cache_Prim_Lvl :=
                     Translation.Peek_Generated_Level (Obj.Trans);

                  Obj.Execute_Progress := True;

                  exit Loop_Handle_Trans_Completed_Prim;

               elsif Obj.Cache_Prim_State = Complete and then
                     Primitive.Equal (Obj.Cache_Prim, Cache_Prim)
               then

                  if not Primitive.Success (Obj.Cache_Prim) then
                     raise Program_Error;
                  end if;
                  Obj.Curr_Type_2.State := Complete;

                  --
                  --  To later update the free-tree, store the inner type
                  --  1 nodes
                  --  of the branch.
                  --
                  --  (Currently not implemented.)
                  --
                  if not Translation.Can_Get_Type_1_Node_Walk (
                     Obj.Trans, Prim)
                  then
                     raise Program_Error;
                  end if;
                  Translation.Get_Type_1_Node_Walk (
                     Obj.Trans,
                     Obj.Query_Branches (
                        Obj.Curr_Query_Branch).Trans_Walk);

                  Translation.Drop_Completed_Primitive (Obj.Trans);
                  Obj.Cache_Prim_State := Invalid;
                  Obj.Execute_Progress := True;

               else
                  exit Loop_Handle_Trans_Completed_Prim;
               end if;
            end Declare_Cache_Prim;
         end Declare_Completed_Prim;
      end loop Loop_Handle_Trans_Completed_Prim;
   end Execute_Translation;

   procedure Execute_Query (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type)
   is
   begin
      --
      --  (Invalidating the primitive here should not be necessary b/c it
      --  was already done by the Translation module, not sure why its still
      --  there.)
      --
      Obj.Curr_Query_Prim := Primitive.Invalid_Object;

      --
      --  Check each entry in the type 2 node
      --
      Declare_Nodes_1 :
      declare
         Nodes : Type_2_Node_Block_Type;
         Found_New_Free_Blocks : Boolean := False;
      begin
         Type_2_Node_Block_From_Block_Data (Nodes, Obj.Cache_Prim_Data);

         For_Type_2_Nodes :
         for Node_Index in Nodes'Range loop

            --
            --  Ignore invalid entries.
            --
            --  (It stands to reason if pba == 0 or pba == INVALID_PBA should
            --  be used - unfortunately its done inconsistently throughout the
            --  CBE. The reasons is that the 'cbe_Block' uses a RAM dataspace
            --  as backing store which is zeroed out initiall which made the
            --  this cheap and pba 0 normally contains a superblock anyway.)
            --
            Declare_PBA_1 :
            declare
               PBA : constant Physical_Block_Address_Type :=
                  Nodes (Node_Index).PBA;
            begin
               if PBA /= 0 then
                  Declare_Usable :
                  declare
                     Usable : constant Boolean :=
                        Leaf_Usable (
                           Active_Snaps, Last_Secured_Gen, Nodes (Node_Index));
                  begin
                     if Usable then

                        --
                        --  Set VBA on the first Usable entry, NOP for
                        --  remaining entries
                        --
                        if
                           Obj.Query_Branches (Obj.Curr_Query_Branch).VBA =
                              VBA_Invalid
                        then
                           Obj.Query_Branches (Obj.Curr_Query_Branch).VBA :=
                              Virtual_Block_Address_Type (
                                 Primitive.Block_Number (Obj.Curr_Query_Prim));
                        end if;

                        Declare_Free_Blocks :
                        declare
                           Free_Blocks : constant Number_Of_Blocks_Type :=
                              Obj.Query_Branches (Obj.Curr_Query_Branch).
                                 Nr_Of_Free_Blocks;
                        begin
                           Obj.Query_Branches (Obj.Curr_Query_Branch).
                              PBAs (Natural (Free_Blocks)) := PBA;

                           Obj.Query_Branches (Obj.Curr_Query_Branch).
                              Nr_Of_Free_Blocks := Free_Blocks + 1;

                           Obj.Nr_Of_Found_Blocks :=
                              Obj.Nr_Of_Found_Blocks + 1;

                           Found_New_Free_Blocks := True;

                        end Declare_Free_Blocks;
                     end if;
                  end Declare_Usable;
               end if;
            end Declare_PBA_1;

            --  break off early
            exit For_Type_2_Nodes when
               Obj.Nr_Of_Blocks = Obj.Nr_Of_Found_Blocks;

         end loop For_Type_2_Nodes;

         --
         --  (Rather than always increasing the current query branch,
         --  only do that when we actually found free blocks.
         --  Somehow or other, querying only 8 branches is not enough
         --  for large trees and we have to change the implementation
         --  later.)
         --
         if Found_New_Free_Blocks then
            Obj.Curr_Query_Branch := Obj.Curr_Query_Branch + 1;
         end if;
      end Declare_Nodes_1;

      --
      --  Reset I/O helper to disarm complete check above.
      --
      --  (Again, the module is in desperate need for proper state
      --  handling.)
      --
      Obj.Curr_Type_2.State := Invalid;

      Declare_End_Of_Tree :
      declare
         End_Of_Tree : constant Boolean :=
            Obj.Curr_Query_VBA +
            Block_Number_Type (Tree_Helper.Degree (Obj.Trans_Helper)) >=
            Block_Number_Type (Tree_Helper.Leafs (Obj.Trans_Helper));
      begin

         if Obj.Nr_Of_Found_Blocks < Obj.Nr_Of_Blocks then

            --
            --  In case we did not find enough free blocks, set the write-back
            --  data. The arbiter will call 'peek_Completed_Primitive ()' and
            --  will try to discard snapshots.
            --
            if End_Of_Tree then

               Obj.WB_Data.Finished := True;
               Primitive.Success (Obj.WB_Data.Prim, False);

            else

               Obj.Curr_Query_VBA := Obj.Curr_Query_VBA +
                  Block_Number_Type (Tree_Helper.Degree (Obj.Trans_Helper));

               --  arm query primitive and check next type 2 node
               Obj.Curr_Query_Prim := Primitive.Valid_Object_No_Pool_Idx (
                  Op     => Read,
                  Succ   => False,
                  Tg     => Primitive.Tag_Free_Tree_Query,
                  Blk_Nr => Obj.Curr_Query_VBA,
                  Idx    => 0);
            end if;
         elsif Obj.Nr_Of_Blocks = Obj.Nr_Of_Found_Blocks then

            --
            --  Here we iterate over all query branches and will fill in all
            --  newly allocated blocks consecutively in the new PBA list.
            --
            Declare_Last_New_PBA_Index :
            declare
               Last_New_PBA_Index : Tree_Level_Index_Type := 0;
            begin
               For_Query_Branches_Less_Than_Curr_1 :
               for Branch_Index in 0 .. Obj.Curr_Query_Branch - 1 loop

                  For_PBAs_Of_Free_Blocks :
                  for PBA_Index in 0 .. Obj.Query_Branches (Branch_Index).
                                           Nr_Of_Free_Blocks - 1
                  loop
                     --
                     --  store iterator out-side so we start from the last set
                     --  entry
                     --
                     For_Unhandled_New_PBAs :
                     for New_PBA_Index
                     in Last_New_PBA_Index .. Tree_Level_Index_Type'Last
                     loop
                        --
                        --  Same convention as during the invalid entries
                        --  check, PBA = 0 means we have to fill in a new
                        --  block.
                        --
                        if Obj.WB_Data.New_PBAs (New_PBA_Index) = 0 then

                           Obj.WB_Data.New_PBAs (New_PBA_Index) :=
                              Obj.Query_Branches (Branch_Index).
                                 PBAs (Natural (PBA_Index));

                           exit For_Unhandled_New_PBAs;
                        end if;

                        Last_New_PBA_Index := Last_New_PBA_Index + 1;

                     end loop For_Unhandled_New_PBAs;
                  end loop For_PBAs_Of_Free_Blocks;
               end loop For_Query_Branches_Less_Than_Curr_1;
            end Declare_Last_New_PBA_Index;

            Obj.Update_State := Initialize;
         end if;
      end Declare_End_Of_Tree;
   end Execute_Query;

   procedure Update_Inner_T1_Nodes (Obj : in out Object_Type)
   is
      SHA_Hash : SHA256_4K.Hash_Type;
      Nodes : Type_1_Node_Block_Type;
   begin
      Type_1_Node_Block_From_Block_Data (Nodes, Obj.Update_Data);
      Declare_Pre_Hash_Data :
      declare
         Pre_Hash_Data : SHA256_4K.Data_Type;
      begin
         SHA256_4K_Data_From_CBE_Data (Pre_Hash_Data, Obj.Update_Pre_Data);
         SHA256_4K.Hash (Pre_Hash_Data, SHA_Hash);
      end Declare_Pre_Hash_Data;

      For_Nodes :
      for Node_Index in 0 .. Tree_Helper.Degree (Obj.Trans_Helper) - 1 loop
         if Nodes (Natural (Node_Index)).PBA =
            Obj.Query_Branches (Obj.Update_Curr_Branch).
               Trans_Walk (Obj.Update_Curr_Lvl - 1).PBA
         then
            Declare_CBE_Hash_1 :
            declare
               CBE_Hash : Hash_Type;
            begin
               CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
               Nodes (Natural (Node_Index)).Hash := CBE_Hash;
            end Declare_CBE_Hash_1;
         end if;
      end loop For_Nodes;
      Block_Data_From_Type_1_Node_Block (Obj.Update_Data, Nodes);

      --  for now the root node is a special case
      if Obj.Update_Curr_Lvl = Tree_Helper.Max_Level (Obj.Trans_Helper) then

         Declare_Hash_Data :
         declare
            Hash_Data : SHA256_4K.Data_Type;
            CBE_Hash : Hash_Type;
         begin
            SHA256_4K_Data_From_CBE_Data (Hash_Data, Obj.Update_Data);
            SHA256_4K.Hash (Hash_Data, SHA_Hash);
            CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
            Obj.Root_Hash := CBE_Hash;
         end Declare_Hash_Data;
      end if;
   end Update_Inner_T1_Nodes;

   procedure Exchange_PBA_In_T2_Node_Entries (Obj : in out Object_Type)
   is
      Nodes : Type_2_Node_Block_Type;
   begin
      Type_2_Node_Block_From_Block_Data (Nodes, Obj.Update_Data);

      For_Nodes :
      for Node_Index in 0 .. Tree_Helper.Degree (Obj.Trans_Helper) - 1 loop
         --
         --  The old and new PBA array contains Data and inner node,
         --  therefore we have to check tree height + 1.
         --
         For_Tree_Levels :
         for Tree_Lvl in 0 .. Obj.WB_Data.Tree_Max_Level loop
            if
               Nodes (Natural (Node_Index)).PBA =
               Obj.WB_Data.New_PBAs (Tree_Level_Index_Type (Tree_Lvl))
            then
               Nodes (Natural (Node_Index)).PBA :=
                  Obj.WB_Data.Old_PBAs (Tree_Level_Index_Type (Tree_Lvl)).PBA;

               Nodes (Natural (Node_Index)).Alloc_Gen :=
                  Obj.WB_Data.Old_PBAs (Tree_Level_Index_Type (Tree_Lvl)).Gen;

               Nodes (Natural (Node_Index)).Free_Gen := Obj.WB_Data.Gen;
               Nodes (Natural (Node_Index)).Reserved := True;
            end if;
         end loop For_Tree_Levels;
      end loop For_Nodes;
      Block_Data_From_Type_2_Node_Block (Obj.Update_Data, Nodes);
   end Exchange_PBA_In_T2_Node_Entries;

   procedure Execute_Update (Obj : in out Object_Type)
   is
   begin
      case Obj.Update_State is
      when Inactive =>

         null;

      when Initialize =>

         Obj.Update_Curr_Branch := 0;
         Obj.Update_Curr_Lvl := 1;
         if Obj.Update_Curr_Branch > Obj.Curr_Query_Branch - 1 or else
            Obj.Update_Curr_Lvl > Tree_Helper.Max_Level (Obj.Trans_Helper)
         then
            Obj.Update_State := Finish;
         else
            Obj.Update_State := Data_Read_Required;
         end if;
         Obj.Execute_Progress := True;

      when Data_Read_Required =>

         if Obj.Cache_Prim_State = Invalid then

            Obj.Cache_Prim_State := Generated;
            Obj.Cache_Prim_Lvl := Obj.Update_Curr_Lvl;
            Obj.Cache_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_FT_Cache,
                  Block_Number_Type (
                     Obj.Query_Branches (Obj.Update_Curr_Branch).
                        Trans_Walk (Obj.Update_Curr_Lvl).PBA),
                  0);

            Obj.Update_State := Data_Read_In_Progress;
            Obj.Execute_Progress := True;

         end if;

      when Data_Read_In_Progress =>

         if Obj.Cache_Prim_State = Complete then

            if not Primitive.Success (Obj.Cache_Prim) then
               raise Program_Error;
            end if;

            Obj.Update_Data := Obj.Cache_Prim_Data;
            Obj.Cache_Prim_State := Invalid;

            if Obj.Update_Curr_Lvl = 1 then

               Exchange_PBA_In_T2_Node_Entries (Obj);
               Obj.Update_State := Data_Write_Required;

            else

               Obj.Cache_Prim_State := Generated;
               Obj.Cache_Prim_Lvl := Obj.Update_Curr_Lvl - 1;
               Obj.Cache_Prim :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_FT_Cache,
                     Block_Number_Type (
                        Obj.Query_Branches (Obj.Update_Curr_Branch).
                           Trans_Walk (Obj.Update_Curr_Lvl - 1).PBA),
                     0);

               Obj.Update_State := Pre_Data_Read_In_Progress;

            end if;
            Obj.Execute_Progress := True;

         end if;

      when Pre_Data_Read_In_Progress =>

         if Obj.Cache_Prim_State = Complete then

            if not Primitive.Success (Obj.Cache_Prim) then
               raise Program_Error;
            end if;

            Obj.Update_Pre_Data := Obj.Cache_Prim_Data;
            Obj.Cache_Prim_State := Invalid;
            Update_Inner_T1_Nodes (Obj);
            Obj.Update_State := Data_Write_Required;
            Obj.Execute_Progress := True;

         end if;

      when Data_Write_Required =>

         if Obj.Cache_Prim_State = Invalid then

            Obj.Cache_Prim_State := Generated;
            Obj.Cache_Prim_Data := Obj.Update_Data;
            Obj.Cache_Prim_Lvl := Obj.Update_Curr_Lvl;
            Obj.Cache_Prim :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_FT_Cache,
                  Block_Number_Type (
                     Obj.Query_Branches (Obj.Update_Curr_Branch).
                        Trans_Walk (Obj.Update_Curr_Lvl).PBA),
                  0);

            Obj.Update_State := Data_Write_In_Progress;
            Obj.Execute_Progress := True;

         end if;

      when Data_Write_In_Progress =>

         if Obj.Cache_Prim_State = Complete then

            if not Primitive.Success (Obj.Cache_Prim) then
               raise Program_Error;
            end if;

            Obj.Cache_Prim_State := Invalid;

            --
            --  Seek next node to update if any or end update
            --
            if Obj.Update_Curr_Lvl < Tree_Helper.Max_Level (Obj.Trans_Helper)
            then
               Obj.Update_Curr_Lvl := Obj.Update_Curr_Lvl + 1;
               Obj.Update_State := Data_Read_Required;
            else
               if Obj.Update_Curr_Branch < Obj.Curr_Query_Branch - 1 then
                  Obj.Update_Curr_Branch := Obj.Update_Curr_Branch + 1;
                  Obj.Update_Curr_Lvl := 1;
                  Obj.Update_State := Data_Read_Required;
               else
                  Obj.Update_State := Finish;
               end if;
            end if;
            Obj.Execute_Progress := True;

         end if;

      when Finish =>

         Obj.Do_WB := True;
         Obj.Update_State := Inactive;
         Obj.Execute_Progress := True;

      end case;

   end Execute_Update;

   procedure Execute (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Trans_Data       : in out Translation_Data_Type)
   is
   begin
      Obj.Execute_Progress := False;

      --  nothing to do, return early
      if Obj.Nr_Of_Blocks = 0 then
         return;
      end if;

      if Obj.Curr_Type_2.State /= Complete then

         ----------------------------
         --  Translation handling  --
         ----------------------------

         Execute_Translation (Obj, Trans_Data);

      else

         -----------------------------
         --  Query free leaf nodes  --
         -----------------------------

         --
         --  The type 2 node's data was read successfully, now look
         --  up potentially free blocks.
         --
         Execute_Query (Obj, Active_Snaps, Last_Secured_Gen);
         Obj.Execute_Progress := True;
      end if;

      --------------------------------------------------------------
      --  Poke state machine that updates meta-data in free-tree  --
      --------------------------------------------------------------

      Execute_Update (Obj);

      if Obj.Do_WB and then
         not Obj.WB_Done
      then

         ------------------------------------
         --  Write-back of changed branch  --
         ------------------------------------

         Obj.Do_WB := False;
         Obj.WB_Done := True;
         Obj.WB_Data.Finished := True;
         Primitive.Success (Obj.WB_Data.Prim, True);
         Obj.Execute_Progress := True;

      end if;

   end Execute;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.WB_Data.Finished then
         return Obj.WB_Data.Prim;
      end if;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   function Peek_Completed_Root_Hash (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return CBE.Hash_Type
   is
   begin
      if not Primitive.Equal (Prim, Obj.WB_Data.Prim) then
         raise Program_Error;
      end if;

      return Obj.Root_Hash;
   end Peek_Completed_Root_Hash;

   function Peek_Completed_WB_Data (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Write_Back_Data_Type
   is
   begin
      if not Primitive.Equal (Prim, Obj.WB_Data.Prim) then
         raise Program_Error;
      end if;

      return Obj.WB_Data;
   end Peek_Completed_WB_Data;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if not Primitive.Equal (Prim, Obj.WB_Data.Prim) then
         raise Program_Error;
      end if;

      --  reset state
      Obj.WB_Data.Finished := False;

      --  request finished, ready for a new one
      Obj.Nr_Of_Blocks := 0;

   end Drop_Completed_Primitive;

end CBE.Free_Tree;
