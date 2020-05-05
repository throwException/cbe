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
--  The Free_tree meta-module handles the allocation and freeing, i.e.,
--  reservation, of nodes. It is vital to implement the CoW semantics.
--
package body CBE.New_Free_Tree
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

   procedure Initialized_Object (Obj : out Object_Type)
   is
   begin
      Obj.State       := Invalid;
      Obj.Root_Node   := (
         PBA  => PBA_Invalid,
         Gen  => 0,
         Hash => (others => 0));
      Obj.Tree_Geom   := (
         Max_Level => 0,
         Edges     => Tree_Max_Degree,
         Leafs     => 0);
      Obj.Current_Gen := 0;

      Obj.Needed_Blocks    := 0;
      Obj.Requested_Blocks := 0;
      Obj.Found_Blocks     := 0;
      Obj.Exchanged_Blocks := 0;

      Obj.Meta_Tree_Request := Invalid_Meta_Tree_Request;
      Obj.Cache_Request     := Invalid_Cache_Request;

      Obj.Cache_Block_Data  := (others => 0);

      Initialize_Type_1_Info_Stack_Array (Obj.Level_N_Stacks);
      Initialize_Type_1_Node_Block_Array (Obj.Level_N_Nodes);

      Obj.Level_0_Stack := Type_2_Info_Stack.Initialized_Object;

      Obj.Level_N_Node := (others => Type_1_Node_Invalid);
      Obj.Level_0_Node := (others => Type_2_Node_Invalid);
      Obj.Type_2_Leafs := Node_Queue.Empty_Node_Queue;

      Obj.WB_Data := Write_Back_Data_Invalid;
   end Initialized_Object;

   procedure Reset_Block_State (Obj : in out Object_Type)
   is
   begin
      Obj.Needed_Blocks := Obj.Requested_Blocks;
      Obj.Found_Blocks  := 0;

      Initialize_Type_1_Info_Stack_Array (Obj.Level_N_Stacks);
      Initialize_Type_1_Node_Block_Array (Obj.Level_N_Nodes);

      Obj.Level_0_Stack := Type_2_Info_Stack.Initialized_Object;

      Obj.Level_N_Node := (others => Type_1_Node_Invalid);
      Obj.Level_0_Node := (others => Type_2_Node_Invalid);
   end Reset_Block_State;

   --
   --  Log_2
   --
   function  Log_2 (Value : Unsigned_32)
   return Unsigned_32
   is
      type Bit_Index_Type is range 0 .. 31;
   begin
      if Value = 0 then
         raise Program_Error;
      end if;
      for Bit_Index in reverse Bit_Index_Type'Range loop
         if (
            Value and
            Shift_Left (Unsigned_32 (1), Natural (Bit_Index))) /= 0
         then
            return Unsigned_32 (Bit_Index);
         end if;
      end loop;
      raise Program_Error;
   end Log_2;

   ------------------------
   --  Module interface  --
   ------------------------

   function Request_Acceptable (Obj : Object_Type) return Boolean
   is (Obj.State = Invalid);

   procedure Submit_Request (
      Obj              : in out Object_Type;
      Root_Node        :        Type_1_Node_Type;
      Tree_Geom        :        Tree_Geometry_Type;
      Current_Gen      :        Generation_Type;
      Free_Gen         :        Generation_Type;
      Requested_Blocks :        Number_Of_Blocks_Type;
      New_Blocks       :        Write_Back.New_PBAs_Type;
      Old_Blocks       :        Type_1_Node_Walk_Type;
      Max_Level        :        Tree_Level_Index_Type;
      Req_Prim         :        Primitive.Object_Type;
      VBA              :        Virtual_Block_Address_Type;
      VBD_Degree       :        Tree_Degree_Type;
      Key_ID           :        Key_ID_Type)
   is
   begin
      if Obj.State /= Invalid then
         raise Program_Error;
      end if;

      Obj.Root_Node        := Root_Node;
      Obj.Tree_Geom        := Tree_Geom;
      Obj.Current_Gen      := Current_Gen;
      Obj.Free_Gen         := Free_Gen;
      Obj.Requested_Blocks := Requested_Blocks;
      Obj.Exchanged_Blocks := 0;

      Reset_Block_State (Obj);

      Type_1_Info_Stack.Push (Obj.Level_N_Stacks (
         Type_1_Info_Stack_Array_Index_Type (Obj.Tree_Geom.Max_Level)),
         (
            State => Invalid, Node => Obj.Root_Node,
            Index => 0,
            Volatile => Node_Volatile (Obj.Root_Node, Obj.Current_Gen)
         ));

      Obj.Key_ID := Key_ID;
      Obj.State := Scan;
      Obj.VBD_Degree_Log_2 :=
         Tree_Degree_Log_2_Type (Log_2 (Unsigned_32 (VBD_Degree)));

      Obj.WB_Data := (
         Prim           => Req_Prim,
         Gen            => Current_Gen,
         VBA            => VBA,
         Tree_Max_Level => Max_Level,
         New_PBAs       => New_Blocks,
         Old_PBAs       => Old_Blocks);
   end Submit_Request;

   procedure Retry_Allocation (Obj : in out Object_Type)
   is
   begin
      Reset_Block_State (Obj);

      Type_1_Info_Stack.Push (Obj.Level_N_Stacks (
         Type_1_Info_Stack_Array_Index_Type (Obj.Tree_Geom.Max_Level)),
         (
            State => Invalid, Node => Obj.Root_Node,
            Index => 0,
            Volatile => Node_Volatile (Obj.Root_Node, Obj.Current_Gen)
         ));

      Obj.State := Scan;

   end Retry_Allocation;

   --
   --  Execute module
   --
   procedure Execute (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean)
   is
   begin
      if Obj.Meta_Tree_Request.State = Pending
         or else Obj.Meta_Tree_Request.State = In_Progress
      then
         return;
      end if;

      if Obj.Cache_Request.State = Pending
         or else Obj.Cache_Request.State = In_Progress
      then
         return;
      end if;

      case Obj.State is
         when Invalid =>
            null;
         when Scan =>
            Execute_Scan (Obj, Active_Snaps, Last_Secured_Gen, Progress);
         when Scan_Complete =>
            Obj.State := Update;
            Progress := True;
         when Update =>
            Execute_Update (Obj, Active_Snaps, Last_Secured_Gen, Progress);
         when Update_Complete =>
            Primitive.Success (Obj.WB_Data.Prim, True);
            Obj.State := Complete;
         when Complete =>
            null;
         when Not_Enough_Free_Blocks =>
            Primitive.Success (Obj.WB_Data.Prim, False);
            Obj.State := Complete;
            Progress := True;
         when Tree_Hash_Mismatch =>
            raise Program_Error;
      end case;
   end Execute;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.State = Complete then
         return Obj.WB_Data.Prim;
      end if;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   function Peek_Completed_Root_Node (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Type
   is
   begin
      if not Primitive.Equal (Prim, Obj.WB_Data.Prim) then
         raise Program_Error;
      end if;

      return Obj.Root_Node;
   end Peek_Completed_Root_Node;

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

      Obj.State := Invalid;
   end Drop_Completed_Primitive;

   function Peek_Generated_Meta_Tree_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.Meta_Tree_Request.State = Pending then
         return Obj.Meta_Tree_Request.Prim;
      else
         return Primitive.Invalid_Object;
      end if;
   end Peek_Generated_Meta_Tree_Primitive;

   procedure Drop_Generated_Meta_Tree_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin

      if Obj.Meta_Tree_Request.State /= Pending then
         raise Program_Error;
      end if;

      if not Primitive.Valid (Obj.Meta_Tree_Request.Prim)
         or else not Primitive.Equal (Obj.Meta_Tree_Request.Prim, Prim)
      then
         raise Program_Error;
      end if;

      Obj.Meta_Tree_Request.State := In_Progress;
   end Drop_Generated_Meta_Tree_Primitive;

   procedure Mark_Generated_Meta_Tree_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      PBA  :        Physical_Block_Address_Type)
   is
   begin
      if Obj.Meta_Tree_Request.State /= In_Progress then
         raise Program_Error;
      end if;

      if not Primitive.Valid (Obj.Meta_Tree_Request.Prim)
         or else not Primitive.Equal (Obj.Meta_Tree_Request.Prim, Prim)
      then
         raise Program_Error;
      end if;

      if not Primitive.Success (Prim) then
         raise Program_Error;
      end if;

      Obj.Meta_Tree_Request.PBA   := PBA;
      Obj.Meta_Tree_Request.State := Complete;
   end Mark_Generated_Meta_Tree_Primitive_Complete;

   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      if Obj.Cache_Request.State = Pending then
         return Obj.Cache_Request.Prim;
      else
         return Primitive.Invalid_Object;
      end if;
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
         return Obj.Cache_Block_Data;
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Data;

   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.Cache_Request.State /= Pending then
         raise Program_Error;
      end if;

      if not Primitive.Valid (Obj.Cache_Request.Prim)
         or else not Primitive.Equal (Obj.Cache_Request.Prim, Prim)
      then
         raise Program_Error;
      end if;

      Obj.Cache_Request.State := In_Progress;
   end Drop_Generated_Cache_Primitive;

   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj        : in out Object_Type;
      Prim       :        Primitive.Object_Type;
      Block_Data :        Block_Data_Type)
   is
   begin
      if Obj.Cache_Request.State /= In_Progress then
         raise Program_Error;
      end if;

      if not Primitive.Valid (Obj.Cache_Request.Prim)
         or else not Primitive.Equal (Obj.Cache_Request.Prim, Prim)
      then
         raise Program_Error;
      end if;

      if not Primitive.Success (Prim) then
         raise Program_Error;
      end if;

      declare
         N : Type_1_Info_Type := Type_1_Info_Stack.Peek_Top (
            Obj.Level_N_Stacks (Obj.Cache_Request.Level));
      begin
         Obj.Cache_Request.State := Complete;

         case Primitive.Operation (Obj.Cache_Request.Prim) is
            when Sync =>

               null;

            when Read =>

               if Check_Node_Hash (Block_Data, N.Node.Hash) then
                  Obj.Cache_Block_Data := Block_Data;
                  N.State := Available;
                  Type_1_Info_Stack.Update_Top (
                     Obj.Level_N_Stacks (Obj.Cache_Request.Level), N);
               else
                  Obj.State := Tree_Hash_Mismatch;
               end if;

            when Write =>

               --  Block_Data := Obj.Cache_Block_Data;

               N.State := Complete;
               Type_1_Info_Stack.Update_Top (
                  Obj.Level_N_Stacks (Obj.Cache_Request.Level), N);

         end case;
      end;

   end Mark_Generated_Cache_Primitive_Complete;

   -----------------
   --  Accessors  --
   -----------------

   function To_String (Obj : Object_Type) return String
   is ("New_Free_Tree:"
      & " PBA: " & Debug.To_String (Obj.Root_Node.PBA)
      & " Gen: " & Debug.To_String (Debug.Uint64_Type (Obj.Root_Node.Gen))
      & " state: " & To_String (Obj.State)
      & " Requested_Blocks: "
         & Debug.To_String (Debug.Uint64_Type (Obj.Requested_Blocks))
      & " Needed_Blocks: "
         & Debug.To_String (Debug.Uint64_Type (Obj.Needed_Blocks))
      & " Found_Blocks: "
         & Debug.To_String (Debug.Uint64_Type (Obj.Found_Blocks))
      & " Exchanged_Blocks: "
         & Debug.To_String (Debug.Uint64_Type (Obj.Exchanged_Blocks))
      );

   -----------------------------------
   --  Private functions/procedures --
   -----------------------------------

   function To_String (S : State_Type) return String
   is (
      case S is
      when Invalid                => "Invalid",
      when Scan                   => "Scan",
      when Scan_Complete          => "Scan_Complete",
      when Update                 => "Update",
      when Update_Complete        => "Update_Complete",
      when Complete               => "Complete",
      when Not_Enough_Free_Blocks => "Not_Enough_Free_Blocks",
      when Tree_Hash_Mismatch     => "Tree_Hash_Mismatch");

   function To_String (S : Info_State_Type) return String
   is (
      case S is
      when Invalid   => "Invalid",
      when Available => "Available",
      when Read      => "Read",
      when Write     => "Write",
      when Complete  => "Complete");

   function To_String (T : Type_1_Info_Type) return String
   is ("Type_1_Info: PBA: " & Debug.To_String (T.Node.PBA)
      & " " & To_String (T.State));

   function To_String (T : Type_2_Info_Type) return String
   is ("Type_2_Info: PBA: " & Debug.To_String (T.Node.PBA)
      & " " & To_String (T.State)
      & " " & Debug.To_String (Debug.Uint64_Type (T.Index)));

   --  function To_String (T : Type_1_Node_Type) return String
   --  is ("Type_1_Node: " & Debug.To_String (T.PBA)
   --     & " " & Debug.To_String (T.Gen)
   --     & " " & Debug.To_String (T.Hash));

   --  function To_String (T : Type_2_Node_Type) return String
   --  is ("Type_2_Node: " & Debug.To_String (T.PBA)
   --     & " " & Debug.To_String (T.Last_VBA)
   --     & " " & Debug.To_String (T.Alloc_Gen)
   --     & " " & Debug.To_String (T.Free_Gen)
   --     & " " & Debug.To_String (T.Reserved));

   procedure Initialize_Type_1_Info_Stack_Array (
      A : out Type_1_Info_Stack_Array_Type)
   is
   begin
      for I in Type_1_Info_Stack_Array_Type'Range loop
         A (I) := Type_1_Info_Stack.Initialized_Object;
      end loop;
   end Initialize_Type_1_Info_Stack_Array;

   procedure Initialize_Type_1_Node_Block_Array (
      A : out Type_1_Node_Block_Array_Type)
   is
   begin
      for I in A'Range loop
         A (I) := (others => Type_1_Node_Invalid);
      end loop;
   end Initialize_Type_1_Node_Block_Array;

   function Check_Type_2_Leaf_Usable (
      Active_Snaps     : Snapshots_Type;
      Last_Secured_Gen : Generation_Type;
      Node             : Type_2_Node_Type)
   return Boolean
   is
      Free   : Boolean := False;
      In_Use : Boolean := False;
   begin
      --  there is no valid PBA
      if Node.PBA = 0 or else Node.PBA = PBA_Invalid then
         return False;
      end if;

      Declare_Generations :
      declare
         F_Gen : constant Generation_Type := Node.Free_Gen;
         A_Gen : constant Generation_Type := Node.Alloc_Gen;
         S_Gen : constant Generation_Type := Last_Secured_Gen;
      begin

         if not Node.Reserved then
            if F_Gen <= S_Gen then
               return True;
            else
               return False;
            end if;
         end if;

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
                     --  XXX check reuse condition, f <= b vs. f < b
                     Is_Free : constant Boolean :=
                        (F_Gen < B_Gen or else A_Gen >= (B_Gen + 1));
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
   end Check_Type_2_Leaf_Usable;

   procedure Compute_Node_Hash (
      Block_Data    : in     Block_Data_Type;
      CBE_Hash      :    out Hash_Type);

   procedure Compute_Node_Hash (
      Block_Data    : in     Block_Data_Type;
      CBE_Hash      :    out Hash_Type)
   is
      SHA_Hash_Data : SHA256_4K.Data_Type;
      SHA_Hash      : SHA256_4K.Hash_Type;
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
      Computed_Hash : Hash_Type;
   begin
      Compute_Node_Hash (Block_Data, Computed_Hash);
      return (Computed_Hash = Node_Hash);
   end Check_Node_Hash;

   function Top_Volatile (S : Type_1_Info_Stack.Object_Type) return Boolean
   is
      N : constant Type_1_Info_Type := Type_1_Info_Stack.Peek_Top (S);
   begin
      return N.Volatile;
   end Top_Volatile;

   function Block_From_Level_0_Node (Entries : Type_2_Node_Block_Type)
   return Block_Data_Type
   is
      Block_Data : Block_Data_Type;
   begin
      Block_Data_From_Type_2_Node_Block (Block_Data, Entries);
      return Block_Data;
   end Block_From_Level_0_Node;

   procedure Populate_Level_0_Stack (
      Stack        : in out Type_2_Info_Stack.Object_Type;
      Entries      :    out Type_2_Node_Block_Type;
      Block_Data   :        Block_Data_Type;
      Active_Snaps :        Snapshots_Type;
      Secured_Gen  :        Generation_Type)
   is
   begin
      Type_2_Info_Stack.Reset (Stack);
      Type_2_Node_Block_From_Block_Data (Entries, Block_Data);

      for I in Entries'Range loop
         declare
            Node : constant Type_2_Node_Type := Entries (I);
         begin
            if Check_Type_2_Leaf_Usable (Active_Snaps, Secured_Gen, Node) then
               declare
                  Info : constant Type_2_Info_Type := (
                     State    => Invalid,
                     Node     => Node,
                     Index    => Node_Index_Type (I));
               begin
                  Type_2_Info_Stack.Push (Stack, Info);
               end;
            end if;
         end;
      end loop;
   end Populate_Level_0_Stack;

   function Block_From_Level_N_Node (Entries : Type_1_Node_Block_Type)
   return Block_Data_Type
   is
      Block_Data : Block_Data_Type;
   begin
      Block_Data_From_Type_1_Node_Block (Block_Data, Entries);
      return Block_Data;
   end Block_From_Level_N_Node;

   procedure Populate_Lower_N_Stack (
      Stack       : in out Type_1_Info_Stack.Object_Type;
      Entries     :    out Type_1_Node_Block_Type;
      Block_Data  :        Block_Data_Type;
      Current_Gen :        Generation_Type)
   is
   begin
      Type_1_Info_Stack.Reset (Stack);
      Type_1_Node_Block_From_Block_Data (Entries, Block_Data);

      for I in Entries'Range loop
         if Entries (I).PBA /= 0 then
            declare
               Node : constant Type_1_Node_Type := Entries (I);
               Info : constant Type_1_Info_Type := (
                  State    => Invalid,
                  Node     => Node,
                  Index    => Node_Index_Type (I),
                  Volatile => Node_Volatile (Node, Current_Gen));
            begin
               Type_1_Info_Stack.Push (Stack, Info);
            end;
         end if;
      end loop;
   end Populate_Lower_N_Stack;

   procedure Update_Upper_N_Stack (
      T          :        Type_1_Info_Type;
      Gen        :        Generation_Type;
      Block_Data : in     Block_Data_Type;
      Entries    : in out Type_1_Node_Block_Type)
   is
      Computed_Hash : Hash_Type;
   begin
      Compute_Node_Hash (Block_Data, Computed_Hash);

      Entries (Natural (T.Index)) := (
         PBA  => T.Node.PBA,
         Gen  => Gen,
         Hash => Computed_Hash);
   end Update_Upper_N_Stack;

   procedure Check_Type_2_Stack (
      Stack      : in out Type_2_Info_Stack.Object_Type;
      Stack_Next : in out Type_1_Info_Stack.Object_Type;
      Leafs      : in out Node_Queue.Node_Queue_Type;
      Found      : in out Number_Of_Blocks_Type)
   is
   begin
      if not Type_2_Info_Stack.Empty (Stack) then
         while not Type_2_Info_Stack.Empty (Stack) loop
            declare
               Info : constant Type_2_Info_Type :=
                  Type_2_Info_Stack.Peek_Top (Stack);
            begin
               if not Node_Queue.Full (Leafs) then
                  Node_Queue.Enqueue (Leafs, Info);
               end if;
            end;

            Found := Found + 1;
            Type_2_Info_Stack.Pop (Stack);
         end loop;
      end if;

      if not Type_1_Info_Stack.Empty (Stack_Next) then
         declare
            N : Type_1_Info_Type := Type_1_Info_Stack.Peek_Top (Stack_Next);
         begin
            --
            --  Only when the node is in Read state we actually have to
            --  acknowledge checking the leaf nodes.
            --
            if N.State = Read then
               N.State := Complete;
               Type_1_Info_Stack.Update_Top (Stack_Next, N);
            end if;
         end;
      end if;
   end Check_Type_2_Stack;

   procedure Execute_Scan (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean)
   is
      End_Of_Tree  : Boolean := False;
      Enough_Found : Boolean := False;
   begin
      --  handle level 0
      declare
         Found_Blocks : Number_Of_Blocks_Type := 0;
      begin
         Check_Type_2_Stack (Obj.Level_0_Stack, Obj.Level_N_Stacks (1),
            Obj.Type_2_Leafs, Found_Blocks);
         Obj.Found_Blocks := Obj.Found_Blocks + Found_Blocks;
      end;

      --  handle level 1 - N
      Loop_Level_N_Stacks :
      for L in Type_1_Info_Stack_Array_Type'Range loop
         if not Type_1_Info_Stack.Empty (Obj.Level_N_Stacks (L)) then
            declare
               N : Type_1_Info_Type :=
                  Type_1_Info_Stack.Peek_Top (Obj.Level_N_Stacks (L));
            begin
               case N.State is
               when Invalid =>

                  if Obj.Cache_Request.State /= Invalid then
                     raise Program_Error;
                  end if;

                  Obj.Cache_Request := New_Cache_Request (
                     N.Node.PBA, Read, L);
                  Progress := True;

               when Available =>

                  Obj.Cache_Request.State := Invalid;

                  if L >= 2 then
                     Populate_Lower_N_Stack (Obj.Level_N_Stacks (L - 1),
                        Obj.Level_N_Node, Obj.Cache_Block_Data,
                        Obj.Current_Gen);
                  else
                     Populate_Level_0_Stack (Obj.Level_0_Stack,
                        Obj.Level_0_Node, Obj.Cache_Block_Data,
                        Active_Snaps, Last_Secured_Gen);
                  end if;
                  N.State := Read;
                  Type_1_Info_Stack.Update_Top (Obj.Level_N_Stacks (L), N);
                  Progress := True;

               when Read =>

                  N.State := Complete;
                  Type_1_Info_Stack.Update_Top (Obj.Level_N_Stacks (L), N);
                  Progress := True;

               when Write =>

                  raise Program_Error;

               when Complete =>

                  if L = Type_1_Info_Stack_Array_Index_Type (
                     Obj.Tree_Geom.Max_Level)
                  then
                     End_Of_Tree := True;
                  end if;

                  if Obj.Found_Blocks >= Obj.Needed_Blocks then
                     Enough_Found := True;
                  end if;

                  Type_1_Info_Stack.Pop (Obj.Level_N_Stacks (L));
                  Progress := True;

               end case;
               exit Loop_Level_N_Stacks;
            end;
         end if;
      end loop Loop_Level_N_Stacks;

      if Obj.State /= Scan then
         return;
      end if;

      if Enough_Found then
         Obj.State := Scan_Complete;

         Initialize_Type_1_Info_Stack_Array (Obj.Level_N_Stacks);
         Initialize_Type_1_Node_Block_Array (Obj.Level_N_Nodes);

         Type_1_Info_Stack.Push (Obj.Level_N_Stacks (
            Type_1_Info_Stack_Array_Index_Type (Obj.Tree_Geom.Max_Level)),
            (
               State => Invalid, Node => Obj.Root_Node,
               Index => 0,
               Volatile => Node_Volatile (Obj.Root_Node, Obj.Current_Gen)
            ));
      end if;

      if End_Of_Tree and then not Enough_Found then
         Obj.State := Not_Enough_Free_Blocks;
      end if;
   end Execute_Scan;

   --
   --  VBD_Inner_Node_VBA
   --
   function VBD_Inner_Node_VBA (
      VBD_Degree_Log_2 : Tree_Degree_Log_2_Type;
      VBD_Level        : Tree_Level_Index_Type;
      VBD_Leaf_VBA     : Virtual_Block_Address_Type)
   return Virtual_Block_Address_Type
   is (
      Virtual_Block_Address_Type (
         Unsigned_64 (VBD_Leaf_VBA) and
         Shift_Left (
            Unsigned_64'Last,
            Natural (
               Unsigned_32 (VBD_Degree_Log_2) *
               Unsigned_32 (VBD_Level)))));

   procedure Exchange_Type_2_Leafs (
      Free_Gen         :        Generation_Type;
      Max_Level        :        Tree_Level_Index_Type;
      Old_Blocks       : in     Type_1_Node_Walk_Type;
      New_Blocks       : in out Write_Back.New_PBAs_Type;
      VBA              :        Virtual_Block_Address_Type;
      VBD_Degree_Log_2 :        Tree_Degree_Log_2_Type;
      Stack            : in out Type_2_Info_Stack.Object_Type;
      Entries          : in out Type_2_Node_Block_Type;
      Exchanged        :    out Number_Of_Blocks_Type;
      Handled          :    out Boolean;
      Key_ID           :        Key_ID_Type)
   is
      Local_Exchanged : Number_Of_Blocks_Type := 0;
   begin
      Handled := False;

      Loop_New_Block_Index :
      for I in 0 .. Max_Level loop

         if New_Blocks (I) = 0 then

            if not Type_2_Info_Stack.Empty (Stack) then
               declare
                  Info : constant Type_2_Info_Type :=
                     Type_2_Info_Stack.Peek_Top (Stack);
               begin
                  if Entries (Natural (Info.Index)).PBA /= Info.Node.PBA then
                     raise Program_Error;
                  end if;

                  New_Blocks (I) := Entries (Natural (Info.Index)).PBA;

                  Entries (Natural (Info.Index)).PBA :=
                     Old_Blocks (I).PBA;
                  Entries (Natural (Info.Index)).Alloc_Gen :=
                     Old_Blocks (I).Gen;
                  Entries (Natural (Info.Index)).Free_Gen :=
                     Free_Gen;

                  Entries (Natural (Info.Index)).Last_VBA    :=
                     VBD_Inner_Node_VBA (VBD_Degree_Log_2, I, VBA);

                  Entries (Natural (Info.Index)).Last_Key_ID := Key_ID;
                  Entries (Natural (Info.Index)).Reserved    := True;
               end;
               Local_Exchanged := Local_Exchanged + 1;
               Type_2_Info_Stack.Pop (Stack);
               Handled := True;
            else
               exit Loop_New_Block_Index;
            end if;
         end if;
      end loop Loop_New_Block_Index;
      Exchanged := Local_Exchanged;
   end Exchange_Type_2_Leafs;

   procedure Execute_Update (
      Obj              : in out Object_Type;
      Active_Snaps     :        Snapshots_Type;
      Last_Secured_Gen :        Generation_Type;
      Progress         : in out Boolean)
   is
      Exchange_Finished : Boolean := False;
      Update_Finished   : Boolean := False;
      Exchanged         : Number_Of_Blocks_Type;
   begin
      --  Node_Queue.Dump (Obj.Type_2_Leafs);

      --  handle level 0
      declare
         Handled : Boolean;
      begin
         Exchange_Type_2_Leafs (
            Obj.Free_Gen,
            Obj.WB_Data.Tree_Max_Level,
            Obj.WB_Data.Old_PBAs,
            Obj.WB_Data.New_PBAs,
            Obj.WB_Data.VBA,
            Obj.VBD_Degree_Log_2,
            Obj.Level_0_Stack,
            Obj.Level_0_Node,
            Exchanged,
            Handled,
            Obj.Key_ID);

         if Handled then
            if Exchanged > 0 then
               Obj.Exchanged_Blocks := Obj.Exchanged_Blocks + Exchanged;
            else
               declare
                  L : constant Type_1_Info_Stack_Array_Index_Type :=
                     Obj.Level_N_Stacks'First;
                     N : Type_1_Info_Type :=
                        Type_1_Info_Stack.Peek_Top (Obj.Level_N_Stacks (L));
               begin
                  N.State := Complete;
                  Type_1_Info_Stack.Update_Top (Obj.Level_N_Stacks (L), N);
               end;
            end if;
         end if;
      end;

      if Obj.Exchanged_Blocks = Obj.Needed_Blocks then
         Exchange_Finished := True;
      end if;

      --  handle level 1 - N
      Loop_Level_N_Stacks :
      for L in Obj.Level_N_Stacks'Range loop
         if not Type_1_Info_Stack.Empty (Obj.Level_N_Stacks (L)) then
            declare
               N : Type_1_Info_Type :=
                  Type_1_Info_Stack.Peek_Top (Obj.Level_N_Stacks (L));
            begin
               case N.State is
               when Invalid =>

                  if Obj.Cache_Request.State /= Invalid then
                     raise Program_Error;
                  end if;

                  Obj.Cache_Request := New_Cache_Request (
                     N.Node.PBA, Read, L);
                  Progress := True;

               when Available =>

                  Obj.Cache_Request.State := Invalid;

                  if L >= 2 then
                     Populate_Lower_N_Stack (Obj.Level_N_Stacks (L - 1),
                        Obj.Level_N_Nodes (L - 1), Obj.Cache_Block_Data,
                        Obj.Current_Gen);
                     if not Type_1_Info_Stack.Empty (
                        Obj.Level_N_Stacks (L - 1))
                     then
                        N.State := Write;
                     else
                        N.State := Complete;
                     end if;
                  else
                     Populate_Level_0_Stack (Obj.Level_0_Stack,
                        Obj.Level_0_Node, Obj.Cache_Block_Data,
                        Active_Snaps, Last_Secured_Gen);
                     if not Type_2_Info_Stack.Empty (Obj.Level_0_Stack) then
                        N.State := Write;
                     else
                        N.State := Complete;
                     end if;
                  end if;

                  Type_1_Info_Stack.Update_Top (Obj.Level_N_Stacks (L), N);
                  Progress := True;

               when Read =>

                  raise Program_Error;

               when Write =>

                  if not N.Volatile then

                     if Obj.Meta_Tree_Request.State = Invalid then

                        Obj.Meta_Tree_Request := New_Meta_Tree_Request (
                           N.Node.PBA);
                        Progress := True;
                        exit Loop_Level_N_Stacks;

                     elsif Obj.Meta_Tree_Request.State = Complete then

                        Obj.Meta_Tree_Request.State := Invalid;

                        N.Volatile := True;
                        N.Node.PBA := Obj.Meta_Tree_Request.PBA;
                        Type_1_Info_Stack.Update_Top (
                           Obj.Level_N_Stacks (L), N);
                     else
                        raise Program_Error;
                     end if;
                  end if;

                  if L >= 2 then
                     Obj.Cache_Block_Data :=
                        Block_From_Level_N_Node (Obj.Level_N_Nodes (L - 1));

                     if L < Type_1_Info_Stack_Array_Index_Type (
                        Obj.Tree_Geom.Max_Level)
                     then
                        Update_Upper_N_Stack (N, Obj.Current_Gen,
                           Obj.Cache_Block_Data, Obj.Level_N_Nodes (L));
                     else
                        Compute_Node_Hash (Obj.Cache_Block_Data,
                           Obj.Root_Node.Hash);
                        Obj.Root_Node.Gen := Obj.Current_Gen;
                        Obj.Root_Node.PBA := N.Node.PBA;
                     end if;
                  else
                     Obj.Cache_Block_Data :=
                        Block_From_Level_0_Node (Obj.Level_0_Node);

                     Update_Upper_N_Stack (N, Obj.Current_Gen,
                        Obj.Cache_Block_Data, Obj.Level_N_Nodes (L));
                  end if;

                  Obj.Cache_Request := New_Cache_Request (
                     N.Node.PBA, Write, L);

                  Progress := True;

               when Complete =>

                  Obj.Cache_Request.State := Invalid;

                  Type_1_Info_Stack.Pop (Obj.Level_N_Stacks (L));

                  if Exchange_Finished then
                     while not Type_1_Info_Stack.Empty (
                        Obj.Level_N_Stacks (L))
                     loop
                        Type_1_Info_Stack.Pop (Obj.Level_N_Stacks (L));
                     end loop;
                  end if;

                  if L = Type_1_Info_Stack_Array_Index_Type (
                     Obj.Tree_Geom.Max_Level)
                  then
                     Update_Finished := True;
                  end if;
                  Progress := True;

               end case;
               exit Loop_Level_N_Stacks;
            end;
         end if;
      end loop Loop_Level_N_Stacks;

      if Obj.State /= Update then
         return;
      end if;

      if Exchange_Finished and then Update_Finished then
         Obj.State := Update_Complete;
      end if;
   end Execute_Update;

   --
   --  stack/queue stuff
   --

   package body Type_1_Info_Stack
   is
      function Initialized_Object return Object_Type
      is (
         Container => (others => Type_1_Info_Invalid),
         Top       => Min - 1);

      function Empty (Obj : Object_Type) return Boolean
      is (Obj.Top < Obj.Container'First);

      function Full (Obj : Object_Type) return Boolean
      is (Obj.Top >= Obj.Container'Last);

      procedure Reset (Obj : in out Object_Type)
      is
      begin
         Obj.Top := Min - 1;
      end Reset;

      procedure Pop (Obj : in out Object_Type)
      is
      begin
         --  !Empty
         Obj.Top := Obj.Top - 1;
      end Pop;

      procedure Push (
         Obj : in out Object_Type;
         Val :        Type_1_Info_Type)
      is
      begin
         --  !Full
         Obj.Top                 := Obj.Top + 1;
         Obj.Container (Obj.Top) := Val;
      end Push;

      function Peek_Top (Obj : Object_Type) return Type_1_Info_Type
      is (Obj.Container (Obj.Top));

      procedure Update_Top (
         Obj : in out Object_Type;
         Val :        Type_1_Info_Type)
      is
      begin
         Obj.Container (Obj.Top) := Val;
      end Update_Top;
   end Type_1_Info_Stack;

   package body Type_2_Info_Stack
   is
      function Initialized_Object return Object_Type
      is (
         Container => (others => Type_2_Info_Invalid),
         Top       => Min - 1);

      function Empty (Obj : Object_Type) return Boolean
      is (Obj.Top < Obj.Container'First);

      function Full (Obj : Object_Type) return Boolean
      is (Obj.Top >= Obj.Container'Last);

      procedure Reset (Obj : in out Object_Type)
      is
      begin
         Obj.Top := Min - 1;
      end Reset;

      procedure Pop (Obj : in out Object_Type)
      is
      begin
         --  !Empty
         Obj.Top := Obj.Top - 1;
      end Pop;

      procedure Push (
         Obj : in out Object_Type;
         Val :        Type_2_Info_Type)
      is
      begin
         --  !Full
         Obj.Top                 := Obj.Top + 1;
         Obj.Container (Obj.Top) := Val;
      end Push;

      function Peek_Top (Obj : Object_Type) return Type_2_Info_Type
      is (Obj.Container (Obj.Top));

      procedure Update_Top (
         Obj : in out Object_Type;
         Val :        Type_2_Info_Type)
      is
      begin
         Obj.Container (Obj.Top) := Val;
      end Update_Top;
   end Type_2_Info_Stack;

   package body Node_Queue
   is
      function Empty_Node_Queue return Node_Queue_Type
      is (
         Head   => Node_Queue_Index_Type'First,
         Tail   => Node_Queue_Index_Type'First,
         Used   => Used_Type'First,
         Container => (others => Type_2_Info_Invalid));

      procedure Enqueue (
         Obj  : in out Node_Queue_Type;
         Node :        Type_2_Info_Type)
      is
      begin
         Obj.Container (Obj.Tail) := Node;

         if Obj.Tail < Node_Queue_Index_Type'Last then
            Obj.Tail := Node_Queue_Index_Type'Succ (Obj.Tail);
         else
            Obj.Tail := Node_Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Succ (Obj.Used);
      end Enqueue;

      function Head (Obj : Node_Queue_Type) return Type_2_Info_Type
      is (Obj.Container (Obj.Head));

      procedure Dequeue_Head (Obj : in out Node_Queue_Type)
      is
      begin
         if Obj.Head < Node_Queue_Index_Type'Last then
            Obj.Head := Node_Queue_Index_Type'Succ (Obj.Head);
         else
            Obj.Head := Node_Queue_Index_Type'First;
         end if;
         Obj.Used := Used_Type'Pred (Obj.Used);
      end Dequeue_Head;

      function Empty (Obj : Node_Queue_Type) return Boolean
      is (Obj.Used = Used_Type'First);

      function Full (Obj : Node_Queue_Type) return Boolean
      is (Obj.Used = Used_Type'Last);

      procedure Dump (Obj : Node_Queue_Type)
      is
      begin
         declare
            Next : Node_Queue_Index_Type := Obj.Head;
            Used : Used_Type := Obj.Used;
         begin
            while Used > Used_Type'First loop

               if Next < Node_Queue_Index_Type'Last then
                  Next := Node_Queue_Index_Type'Succ (Next);
               else
                  Next := Node_Queue_Index_Type'First;
               end if;

               Used := Used_Type'Pred (Used);
            end loop;
         end;
      end Dump;
   end Node_Queue;

end CBE.New_Free_Tree;
