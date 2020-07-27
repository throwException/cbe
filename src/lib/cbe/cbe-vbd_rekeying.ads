--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;
with CBE.Write_Back;
with CBE.Request;
with Interfaces;

package CBE.VBD_Rekeying
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Jobs : constant := 1;

   type Rekeying_Type is private;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   --
   --  Initialize_Rekeying
   --
   procedure Initialize_Rekeying (Rkg : out Rekeying_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Rkg : Rekeying_Type)
   return Boolean;

   --
   --  Submit_Primitive_Rekeying
   --
   procedure Submit_Primitive_Rekeying (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type;
      VBA              :        Virtual_Block_Address_Type;
      Snapshots        :        Snapshots_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      Old_Key_ID       :        Key_ID_Type;
      New_Key_ID       :        Key_ID_Type);

   --
   --  Submit_Primitive_Read_VBA
   --
   procedure Submit_Primitive_Read_VBA (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      Req              :        Request.Object_Type;
      Snapshot         :        Snapshot_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      Key_ID           :        Key_ID_Type);

   --
   --  Submit_Primitive_Write_VBA
   --
   procedure Submit_Primitive_Write_VBA (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      Curr_Gen         :        Generation_Type;
      Req              :        Request.Object_Type;
      Snapshot         :        Snapshot_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      Key_ID           :        Key_ID_Type);

   --
   --  Submit_Primitive_Resizing
   --
   procedure Submit_Primitive_Resizing (
      Rkg              : in out Rekeying_Type;
      Prim             :        Primitive.Object_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type;
      Snapshots        :        Snapshots_Type;
      Snapshots_Degree :        Tree_Degree_Type;
      First_PBA        :        Physical_Block_Address_Type;
      Nr_Of_PBAs       :        Number_Of_Blocks_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Snapshots
   --
   function Peek_Completed_Snapshots (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Snapshots_Type;

   --
   --  Peek_Completed_Snap
   --
   function Peek_Completed_Snap (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Snapshot_Type;

   --
   --  Peek_Completed_Nr_Of_Leaves
   --
   function Peek_Completed_Nr_Of_Leaves (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Tree_Number_Of_Leafs_Type;

   --
   --  Peek_Completed_PBA
   --
   function Peek_Completed_PBA (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Physical_Block_Address_Type;

   --
   --  Peek_Completed_Nr_Of_PBAs
   --
   function Peek_Completed_Nr_Of_PBAs (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Rkg      : in out Rekeying_Type;
      Progress : in out Boolean);

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Crypto_Primitive
   --
   function Peek_Generated_Crypto_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Blk_IO_Primitive
   --
   function Peek_Generated_Blk_IO_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_FT_Primitive
   --
   function Peek_Generated_FT_Primitive (Rkg : Rekeying_Type)
   return Primitive.Object_Type;
   --
   --  Peek_Generated_New_PBAs
   --
   function Peek_Generated_New_PBAs (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Write_Back.New_PBAs_Type;

   --
   --  Peek_Generated_Old_Key_ID
   --
   function Peek_Generated_Old_Key_ID (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_New_Key_ID
   --
   function Peek_Generated_New_Key_ID (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Virtual_Block_Address_Type;

   --
   --  Peek_Generated_Max_Level
   --
   function Peek_Generated_Max_Level (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Tree_Level_Index_Type;

   --
   --  Peek_Generated_T1_Node_Walk
   --
   function Peek_Generated_T1_Node_Walk (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Type_1_Node_Walk_Type;

   --
   --  Peek_Generated_Nr_Of_Blks
   --
   function Peek_Generated_Nr_Of_Blks (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Peek_Generated_Cipher_Data
   --
   function Peek_Generated_Cipher_Data (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Peek_Generated_Plain_Data
   --
   function Peek_Generated_Plain_Data (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Peek_Generated_Crypto_Key_ID
   --
   function Peek_Generated_Crypto_Key_ID (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_Req
   --
   function Peek_Generated_Req (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Request.Object_Type;

   --
   --  Peek_Generated_Free_Gen
   --
   function Peek_Generated_Free_Gen (
      Rkg  : Rekeying_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Completed
   --
   procedure Mark_Generated_Prim_Completed (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Completed_Blk_Data
   --
   procedure Mark_Generated_Prim_Completed_Blk_Data (
      Rkg      : in out Rekeying_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type);

   --
   --  Mark_Generated_Prim_Completed_Plain_Data
   --
   procedure Mark_Generated_Prim_Completed_Plain_Data (
      Rkg        : in out Rekeying_Type;
      Prim       :        Primitive.Object_Type;
      Plain_Data :        Block_Data_Type);

   --
   --  Mark_Generated_Prim_Completed_Cipher_Data
   --
   procedure Mark_Generated_Prim_Completed_Cipher_Data (
      Rkg         : in out Rekeying_Type;
      Prim        :        Primitive.Object_Type;
      Cipher_Data :        Block_Data_Type);

   --
   --  Mark_Generated_Prim_Completed
   --
   procedure Mark_Generated_Prim_Completed_New_PBAs (
      Rkg      : in out Rekeying_Type;
      Prim     :        Primitive.Object_Type;
      New_PBAs :        Write_Back.New_PBAs_Type);

   --
   --  Mark_Generated_Prim_Completed_Hash
   --
   procedure Mark_Generated_Prim_Completed_Hash (
      Rkg  : in out Rekeying_Type;
      Prim :        Primitive.Object_Type;
      Hash :        Hash_Type);

private

   type Job_Operation_Type is (
      Invalid,
      Rekey_VBA,
      Read_VBA,
      Write_VBA,
      VBD_Extension_Step
   );

   type Job_State_Type is (
      Submitted,

      Read_Root_Node_Pending,
      Read_Root_Node_In_Progress,
      Read_Root_Node_Completed,

      Read_Inner_Node_Pending,
      Read_Inner_Node_In_Progress,
      Read_Inner_Node_Completed,

      Read_Leaf_Node_Pending,
      Read_Leaf_Node_In_Progress,
      Read_Leaf_Node_Completed,

      Read_Client_Data_From_Leaf_Node_Pending,
      Read_Client_Data_From_Leaf_Node_In_Progress,
      Read_Client_Data_From_Leaf_Node_Completed,

      Write_Client_Data_To_Leaf_Node_Pending,
      Write_Client_Data_To_Leaf_Node_In_Progress,
      Write_Client_Data_To_Leaf_Node_Completed,

      Decrypt_Leaf_Node_Pending,
      Decrypt_Leaf_Node_In_Progress,
      Decrypt_Leaf_Node_Completed,

      Alloc_PBAs_At_Leaf_Lvl_Pending,
      Alloc_PBAs_At_Leaf_Lvl_In_Progress,
      Alloc_PBAs_At_Leaf_Lvl_Completed,

      Alloc_PBAs_At_Lowest_Inner_Lvl_Pending,
      Alloc_PBAs_At_Lowest_Inner_Lvl_In_Progress,
      Alloc_PBAs_At_Lowest_Inner_Lvl_Completed,

      Alloc_PBAs_At_Higher_Inner_Lvl_Pending,
      Alloc_PBAs_At_Higher_Inner_Lvl_In_Progress,
      Alloc_PBAs_At_Higher_Inner_Lvl_Completed,

      Encrypt_Leaf_Node_Pending,
      Encrypt_Leaf_Node_In_Progress,
      Encrypt_Leaf_Node_Completed,

      Write_Leaf_Node_Pending,
      Write_Leaf_Node_In_Progress,
      Write_Leaf_Node_Completed,

      Write_Inner_Node_Pending,
      Write_Inner_Node_In_Progress,
      Write_Inner_Node_Completed,

      Write_Root_Node_Pending,
      Write_Root_Node_In_Progress,
      Write_Root_Node_Completed,

      Completed
   );

   subtype Type_1_Node_Blocks_Index_Type is
      Tree_Level_Index_Type range
         Tree_Level_Index_Type'First + 1 .. Tree_Level_Index_Type'Last;

   type Type_1_Node_Blocks_Type
   is array (Type_1_Node_Blocks_Index_Type) of Type_1_Node_Block_Type;

   type Type_1_Node_Blocks_PBAs_Type
   is array (Type_1_Node_Blocks_Index_Type) of Physical_Block_Address_Type;

   type Job_Type is record
      Operation        : Job_Operation_Type;
      State            : Job_State_Type;
      Submitted_Prim   : Primitive.Object_Type;
      Generated_Prim   : Primitive.Object_Type;
      Snapshots        : Snapshots_Type;
      Snapshots_Degree : Tree_Degree_Type;
      Snapshot_Idx     : Snapshots_Index_Type;
      Old_Key_ID       : Key_ID_Type;
      New_Key_ID       : Key_ID_Type;
      T1_Blks          : Type_1_Node_Blocks_Type;
      T1_Blks_Old_PBAs : Type_1_Node_Blocks_PBAs_Type;
      T1_Blk_Idx       : Type_1_Node_Blocks_Index_Type;
      Data_Blk         : Block_Data_Type;
      Data_Blk_Old_PBA : Physical_Block_Address_Type;
      First_Snapshot   : Boolean;
      VBA              : Virtual_Block_Address_Type;
      T1_Node_Walk     : Type_1_Node_Walk_Type;
      New_PBAs         : Write_Back.New_PBAs_Type;
      PBA              : Physical_Block_Address_Type;
      Req              : Request.Object_Type;
      Hash             : Hash_Type;
      Nr_Of_PBAs       : Number_Of_Blocks_Type;
      Nr_Of_Blks       : Number_Of_Blocks_Type;
      Nr_Of_Leaves     : Tree_Number_Of_Leafs_Type;
      Curr_Gen         : Generation_Type;
      Last_Secured_Gen : Generation_Type;
      Free_Gen         : Generation_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Rekeying_Type is record
      Jobs : Jobs_Type;
   end record;

   --
   --  Execute_Read_VBA
   --
   procedure Execute_Read_VBA (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Write_VBA
   --
   procedure Execute_Write_VBA (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Rekey_VBA
   --
   procedure Execute_Rekey_VBA (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_VBD_Extension_Step
   --
   procedure Execute_VBD_Extension_Step (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean);

   --
   --  Child_Idx_For_VBA
   --
   function Child_Idx_For_VBA (
      VBA  : Virtual_Block_Address_Type;
      Lvl  : Type_1_Node_Blocks_Index_Type;
      Degr : Tree_Degree_Type)
   return Type_1_Node_Block_Index_Type;

   --
   --  Execute_Rekey_VBA_Read_Inner_Node_Completed
   --
   procedure Execute_Rekey_VBA_Read_Inner_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress :    out Boolean);

   --
   --  Execute_VBD_Ext_Step_Read_Inner_Node_Completed
   --
   procedure Execute_VBD_Ext_Step_Read_Inner_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress :    out Boolean);

   --
   --  Execute_Read_VBA_Read_Inner_Node_Completed
   --
   procedure Execute_Read_VBA_Read_Inner_Node_Completed (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress :    out Boolean);

   --
   --  Log_2
   --
   function  Log_2 (Value : Interfaces.Unsigned_32)
   return Interfaces.Unsigned_32;

   --
   --  Set_Args_For_Alloc_Of_New_PBAs_For_Branch_Of_Written_VBA
   --
   procedure Set_Args_For_Alloc_Of_New_PBAs_For_Branch_Of_Written_VBA (
      Curr_Gen         :     Generation_Type;
      Snapshot         :     Snapshot_Type;
      Snapshot_Degree  :     Tree_Degree_Type;
      VBA              :     Virtual_Block_Address_Type;
      T1_Blks          :     Type_1_Node_Blocks_Type;
      Prim_Idx         :     Primitive.Index_Type;
      Free_Gen         : out Generation_Type;
      T1_Walk          : out Type_1_Node_Walk_Type;
      State            : out Job_State_Type;
      Generated_Prim   : out Primitive.Object_Type;
      Progress         : out Boolean);

   --
   --  Set_Args_For_Alloc_Of_New_PBAs_For_Rekeying
   --
   procedure Set_Args_For_Alloc_Of_New_PBAs_For_Rekeying (
      For_Curr_Gen_Blks :        Boolean;
      Curr_Gen          :        Generation_Type;
      Snapshot          :        Snapshot_Type;
      Snapshot_Degree   :        Tree_Degree_Type;
      VBA               :        Virtual_Block_Address_Type;
      Min_Lvl_Idx       :        Tree_Level_Index_Type;
      Prim_Idx          :        Primitive.Index_Type;
      T1_Blks           :        Type_1_Node_Blocks_Type;
      T1_Walk           :    out Type_1_Node_Walk_Type;
      New_PBAs          : in out Write_Back.New_PBAs_Type;
      Nr_Of_Blks        :    out Number_Of_Blocks_Type;
      Free_Gen          :    out Generation_Type;
      Prim              :    out Primitive.Object_Type);

   --
   --  Set_Args_For_Alloc_Of_New_PBAs_For_Resizing
   --
   procedure Set_Args_For_Alloc_Of_New_PBAs_For_Resizing (
      Curr_Gen          :     Generation_Type;
      Snapshot          :     Snapshot_Type;
      Snapshot_Degree   :     Tree_Degree_Type;
      VBA               :     Virtual_Block_Address_Type;
      Min_Lvl_Idx       :     Tree_Level_Index_Type;
      Prim_Idx          :     Primitive.Index_Type;
      T1_Blks           :     Type_1_Node_Blocks_Type;
      T1_Walk           : out Type_1_Node_Walk_Type;
      New_PBAs          : out Write_Back.New_PBAs_Type;
      Nr_Of_Blks        : out Number_Of_Blocks_Type;
      Free_Gen          : out Generation_Type;
      Prim              : out Primitive.Object_Type);

   --
   --  Discard_Disposable_Snapshots
   --
   procedure Discard_Disposable_Snapshots (
      Snapshots        : in out Snapshots_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type);

   --
   --  Alloc_PBA_From_Resizing_Contingent
   --
   procedure Alloc_PBA_From_Resizing_Contingent (
      First_PBA     : in out Physical_Block_Address_Type;
      Nr_Of_PBAs    : in out Number_Of_Blocks_Type;
      Allocated_PBA :    out Physical_Block_Address_Type);

   --
   --  Add_New_Root_Lvl_To_Snap_Using_PBA_Contingent
   --
   procedure Add_New_Root_Lvl_To_Snap_Using_PBA_Contingent (
      Snapshots        : in out Snapshots_Type;
      Snap_Idx         : in out Snapshots_Index_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type;
      T1_Blks          : in out Type_1_Node_Blocks_Type;
      First_PBA        : in out Physical_Block_Address_Type;
      Nr_Of_PBAs       : in out Number_Of_Blocks_Type);

   --
   --  Set_New_PBAs_Identical_To_Current_PBAs
   --
   procedure Set_New_PBAs_Identical_To_Current_PBAs (
      Snapshot          :     Snapshot_Type;
      Snapshot_Degree   :     Tree_Degree_Type;
      VBA               :     Virtual_Block_Address_Type;
      T1_Blks           :     Type_1_Node_Blocks_Type;
      New_PBAs          : out Write_Back.New_PBAs_Type);

   --
   --  Add_New_Branch_To_Snap_Using_PBA_Contingent
   --
   procedure Add_New_Branch_To_Snap_Using_PBA_Contingent (
      Mount_Point_Lvl_Idx   :        Type_1_Node_Blocks_Index_Type;
      Mount_Point_Child_Idx :        Type_1_Node_Block_Index_Type;
      Snapshots_Degree      :        Tree_Degree_Type;
      First_PBA             : in out Physical_Block_Address_Type;
      Nr_Of_PBAs            : in out Number_Of_Blocks_Type;
      T1_Blks               : in out Type_1_Node_Blocks_Type;
      Stopped_At_Lvl_Idx    :    out Type_1_Node_Blocks_Index_Type;
      Nr_Of_Leaves          :    out Tree_Number_Of_Leafs_Type);

   --
   --  Set_Args_For_Write_Back_Of_T1_Lvl
   --
   procedure Set_Args_For_Write_Back_Of_T1_Lvl (
      Max_Lvl_Idx :     Tree_Level_Index_Type;
      T1_Lvl_Idx  :     Type_1_Node_Blocks_Index_Type;
      PBA         :     Physical_Block_Address_Type;
      Prim_Idx    :     Primitive.Index_Type;
      Job_State   : out Job_State_Type;
      Progress    : out Boolean;
      Prim        : out Primitive.Object_Type);

   --
   --  Update_Nodes_Of_Branch_Of_Written_VBA
   --
   procedure Update_Nodes_Of_Branch_Of_Written_VBA (
      Snapshot        : in out Snapshot_Type;
      Snapshot_Degree :        Tree_Degree_Type;
      VBA             :        Virtual_Block_Address_Type;
      New_PBAs        :        Write_Back.New_PBAs_Type;
      Leaf_Hash       :        Hash_Type;
      Curr_Gen        :        Generation_Type;
      T1_Blks         : in out Type_1_Node_Blocks_Type);

   --
   --  Check_Hash_Of_Read_Type_1_Node
   --
   procedure Check_Hash_Of_Read_Type_1_Node (
      Snapshot         : Snapshot_Type;
      Snapshots_Degree : Tree_Degree_Type;
      T1_Blk_Idx       : Type_1_Node_Blocks_Index_Type;
      T1_Blks          : Type_1_Node_Blocks_Type;
      VBA              : Virtual_Block_Address_Type);

   --
   --  Set_Args_In_Order_To_Read_Type_1_Node
   --
   procedure Set_Args_In_Order_To_Read_Type_1_Node (
      Snapshot         :     Snapshot_Type;
      Snapshots_Degree :     Tree_Degree_Type;
      T1_Blks          :     Type_1_Node_Blocks_Type;
      T1_Blk_Idx       :     Type_1_Node_Blocks_Index_Type;
      VBA              :     Virtual_Block_Address_Type;
      Job_Idx          :     Jobs_Index_Type;
      State            : out Job_State_Type;
      Generated_Prim   : out Primitive.Object_Type;
      Progress         : out Boolean);

   --
   --  Set_Args_In_Order_To_Write_Client_Data_To_Leaf_Node
   --
   procedure Set_Args_In_Order_To_Write_Client_Data_To_Leaf_Node (
      New_PBAs         :     Write_Back.New_PBAs_Type;
      Job_Idx          :     Jobs_Index_Type;
      State            : out Job_State_Type;
      Generated_Prim   : out Primitive.Object_Type;
      Progress         : out Boolean);

   --
   --  Initialize_Args_Of_Operation_Write_VBA
   --
   procedure Initialize_Args_Of_Operation_Write_VBA (
      Submitted_Prim :     Primitive.Object_Type;
      Snapshots      :     Snapshots_Type;
      Snapshot_Idx   : out Snapshots_Index_Type;
      VBA            : out Virtual_Block_Address_Type;
      T1_Blk_Idx     : out Type_1_Node_Blocks_Index_Type);

   --
   --  Initialize_New_PBAs_And_Determine_Nr_Of_PBAs_To_Allocate
   --
   procedure Initialize_New_PBAs_And_Determine_Nr_Of_PBAs_To_Allocate (
      Curr_Gen         :     Generation_Type;
      Snapshot         :     Snapshot_Type;
      Snapshot_Degree  :     Tree_Degree_Type;
      VBA              :     Virtual_Block_Address_Type;
      T1_Blks          :     Type_1_Node_Blocks_Type;
      New_PBAs         : out Write_Back.New_PBAs_Type;
      Nr_Of_Blks       : out Number_Of_Blocks_Type);

   --
   --  Mark_Job_Successfully_Completed
   --
   procedure Mark_Job_Successfully_Completed (
      State            :    out Job_State_Type;
      Submitted_Prim   : in out Primitive.Object_Type;
      Progress         :    out Boolean);

   --
   --  Check_That_Primitive_Was_Successful
   --
   procedure Check_That_Primitive_Was_Successful (
      Prim : Primitive.Object_Type);

end CBE.VBD_Rekeying;
