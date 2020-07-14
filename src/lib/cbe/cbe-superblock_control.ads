--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

package CBE.Superblock_Control
with SPARK_Mode
is
   pragma Pure;

   Nr_Of_Jobs : constant := 2;

   type Control_Type is private;

   type Jobs_Index_Type is range 0 .. Nr_Of_Jobs - 1;

   --
   --  Initialize_Control
   --
   procedure Initialize_Control (Ctrl : out Control_Type);

   --
   --  Info
   --
   procedure Info (
      SB   :     Superblock_Type;
      Info : out Info_Type);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Ctrl : Control_Type)
   return Boolean;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Submit_Primitive_Nr_Of_Blks
   --
   procedure Submit_Primitive_Nr_Of_Blks (
      Ctrl       : in out Control_Type;
      Prim       :        Primitive.Object_Type;
      Nr_Of_PBAs :        Number_Of_Blocks_Type);

   --
   --  Submit_Primitive_Gen
   --
   procedure Submit_Primitive_Gen (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type;
      Gen  :        Generation_Type);

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Completed_Request_Finished
   --
   function Peek_Completed_Request_Finished (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Boolean;

   --
   --  Peek_Completed_Previous_Key_Plaintext
   --
   function Peek_Completed_Previous_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type;

   --
   --  Peek_Completed_Current_Key_Plaintext
   --
   function Peek_Completed_Current_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type;

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Generation_Type;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Execute
   --
   procedure Execute (
      Ctrl          : in out Control_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Peek_Generated_VBD_Rkg_Primitive
   --
   function Peek_Generated_VBD_Rkg_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_FT_Rszg_Primitive
   --
   function Peek_Generated_FT_Rszg_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_TA_Primitive
   --
   function Peek_Generated_TA_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Hash
   --
   function Peek_Generated_Hash (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_Plain_Key
   --
   function Peek_Generated_Key_Value_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Value_Plaintext_Type;

   --
   --  Peek_Generated_Key_Value_Ciphertext
   --
   function Peek_Generated_Key_Value_Ciphertext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Value_Ciphertext_Type;

   --
   --  Peek_Generated_Key_Plaintext
   --
   function Peek_Generated_Key_Plaintext (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Key_Plaintext_Type;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Blk_IO_Primitive
   --
   function Peek_Generated_Blk_IO_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Crypto_Primitive
   --
   function Peek_Generated_Crypto_Primitive (Ctrl : Control_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Blk_Data
   --
   function Peek_Generated_Blk_Data (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type)
   return Block_Data_Type;

   --
   --  Peek_Generated_PBA
   --
   function Peek_Generated_PBA (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Physical_Block_Address_Type;

   --
   --  Peek_Generated_Nr_Of_Blks
   --
   function Peek_Generated_Nr_Of_Blks (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Number_Of_Blocks_Type;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Virtual_Block_Address_Type;

   --
   --  Peek_Generated_Last_Secured_Gen
   --
   function Peek_Generated_Last_Secured_Gen (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Generation_Type;

   --
   --  Peek_Generated_Snapshots
   --
   function Peek_Generated_Snapshots (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Snapshots_Type;

   --
   --  Peek_Generated_Snapshots_Degree
   --
   function Peek_Generated_Snapshots_Degree (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Degree_Type;

   --
   --  Peek_Generated_FT_Root
   --
   function Peek_Generated_FT_Root (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Type_1_Node_Type;

   --
   --  Peek_Generated_FT_Degree
   --
   function Peek_Generated_FT_Degree (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Degree_Type;

   --
   --  Peek_Generated_FT_Nr_Of_Leaves
   --
   function Peek_Generated_FT_Nr_Of_Leaves (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Number_Of_Leafs_Type;

   --
   --  Peek_Generated_FT_Max_Lvl_Idx
   --
   function Peek_Generated_FT_Max_Lvl_Idx (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Tree_Level_Index_Type;

   --
   --  Peek_Generated_Old_Key_ID
   --
   function Peek_Generated_Old_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Key_ID_Type;

   --
   --  Peek_Generated_New_Key_ID
   --
   function Peek_Generated_New_Key_ID (
      Ctrl : Control_Type;
      Prim : Primitive.Object_Type;
      SB   : Superblock_Type)
   return Key_ID_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

   --
   --  Mark_Generated_Prim_Complete_VBD_Ext
   --
   procedure Mark_Generated_Prim_Complete_VBD_Ext (
      Ctrl         : in out Control_Type;
      Prim         :        Primitive.Object_Type;
      Snapshots    :        Snapshots_Type;
      First_PBA    :        Physical_Block_Address_Type;
      Nr_Of_PBAs   :        Number_Of_Blocks_Type;
      Nr_Of_Leaves :        Tree_Number_Of_Leafs_Type);

   --
   --  Mark_Generated_Prim_Complete_FT_Ext
   --
   procedure Mark_Generated_Prim_Complete_FT_Ext (
      Ctrl            : in out Control_Type;
      Prim            :        Primitive.Object_Type;
      FT_Root         :        Type_1_Node_Type;
      FT_Max_Lvl_Idx  :        Tree_Level_Index_Type;
      FT_Nr_Of_Leaves :        Tree_Number_Of_Leafs_Type;
      First_PBA       :        Physical_Block_Address_Type;
      Nr_Of_PBAs      :        Number_Of_Blocks_Type;
      Nr_Of_Leaves    :        Tree_Number_Of_Leafs_Type);

   --
   --  Mark_Generated_Prim_Complete_Key_Value_Plaintext
   --
   procedure Mark_Generated_Prim_Complete_Key_Value_Plaintext (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark_Generated_Prim_Complete_Blk_Data
   --
   procedure Mark_Generated_Prim_Complete_Blk_Data (
      Ctrl     : in out Control_Type;
      Prim     :        Primitive.Object_Type;
      Blk_Data :        Block_Data_Type);

   --
   --  Mark_Generated_Prim_Complete_Key_Value_Ciphertext
   --
   procedure Mark_Generated_Prim_Complete_Key_Value_Ciphertext (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type);

   --
   --  Mark_Generated_Prim_Complete_Snapshots
   --
   procedure Mark_Generated_Prim_Complete_Snapshots (
      Ctrl      : in out Control_Type;
      Prim      :        Primitive.Object_Type;
      Snapshots :        Snapshots_Type);

   --
   --  Mark_Generated_Prim_Complete
   --
   procedure Mark_Generated_Prim_Complete (
      Ctrl : in out Control_Type;
      Prim :        Primitive.Object_Type);

private

   type Job_Operation_Type is (
      Invalid,
      Sync,
      Initialize,
      Deinitialize,
      VBD_Extension_Step,
      FT_Extension_Step,
      Create_Snapshot,
      Discard_Snapshot,
      Initialize_Rekeying,
      Rekey_VBA);

   type Job_State_Type is (
      Submitted,
      Read_SB_Pending,
      Read_SB_In_Progress,
      Read_SB_Completed,
      Read_Current_SB_Pending,
      Read_Current_SB_In_Progress,
      Read_Current_SB_Completed,
      Rekey_VBA_In_VBD_Pending,
      Rekey_VBA_In_VBD_In_Progress,
      Rekey_VBA_In_VBD_Completed,
      VBD_Ext_Step_In_VBD_Pending,
      VBD_Ext_Step_In_VBD_In_Progress,
      VBD_Ext_Step_In_VBD_Completed,
      FT_Ext_Step_In_FT_Pending,
      FT_Ext_Step_In_FT_In_Progress,
      FT_Ext_Step_In_FT_Completed,
      Create_Key_Pending,
      Create_Key_In_Progress,
      Create_Key_Completed,
      Encrypt_Current_Key_Pending,
      Encrypt_Current_Key_In_Progress,
      Encrypt_Current_Key_Completed,
      Encrypt_Previous_Key_Pending,
      Encrypt_Previous_Key_In_Progress,
      Encrypt_Previous_Key_Completed,
      Decrypt_Current_Key_Pending,
      Decrypt_Current_Key_In_Progress,
      Decrypt_Current_Key_Completed,
      Decrypt_Previous_Key_Pending,
      Decrypt_Previous_Key_In_Progress,
      Decrypt_Previous_Key_Completed,
      Sync_Cache_Pending,
      Sync_Cache_In_Progress,
      Sync_Cache_Completed,
      Add_Key_At_Crypto_Module_Pending,
      Add_Key_At_Crypto_Module_In_Progress,
      Add_Key_At_Crypto_Module_Completed,
      Add_Previous_Key_At_Crypto_Module_Pending,
      Add_Previous_Key_At_Crypto_Module_In_Progress,
      Add_Previous_Key_At_Crypto_Module_Completed,
      Add_Current_Key_At_Crypto_Module_Pending,
      Add_Current_Key_At_Crypto_Module_In_Progress,
      Add_Current_Key_At_Crypto_Module_Completed,
      Remove_Previous_Key_At_Crypto_Module_Pending,
      Remove_Previous_Key_At_Crypto_Module_In_Progress,
      Remove_Previous_Key_At_Crypto_Module_Completed,
      Remove_Current_Key_At_Crypto_Module_Pending,
      Remove_Current_Key_At_Crypto_Module_In_Progress,
      Remove_Current_Key_At_Crypto_Module_Completed,
      Write_SB_Pending,
      Write_SB_In_Progress,
      Write_SB_Completed,
      Sync_Blk_IO_Pending,
      Sync_Blk_IO_In_Progress,
      Sync_Blk_IO_Completed,
      Secure_SB_Pending,
      Secure_SB_In_Progress,
      Secure_SB_Completed,
      Completed);

   type Job_Type is record
      Operation : Job_Operation_Type;
      State : Job_State_Type;
      Submitted_Prim : Primitive.Object_Type;
      Generated_Prim : Primitive.Object_Type;
      Key_Plaintext : Key_Plaintext_Type;
      SB_Ciphertext : Superblock_Ciphertext_Type;
      SB_Idx : Superblocks_Index_Type;
      SB_Found : Boolean;
      Read_SB_Idx : Superblocks_Index_Type;
      Nr_Of_Leaves : Tree_Number_Of_Leafs_Type;
      Generation : Generation_Type;
      Hash : Hash_Type;
      Request_Finished : Boolean;
      Snapshots : Snapshots_Type;
      PBA : Physical_Block_Address_Type;
      Nr_Of_Blks : Number_Of_Blocks_Type;
      FT_Root : Type_1_Node_Type;
      FT_Max_Lvl_Idx : Tree_Level_Index_Type;
      FT_Nr_Of_Leaves : Tree_Number_Of_Leafs_Type;
      Prev_Key_Plaintext : Key_Plaintext_Type;
      Curr_Key_Plaintext : Key_Plaintext_Type;
   end record;

   type Jobs_Type is array (Jobs_Index_Type) of Job_Type;

   type Control_Type is record
      Jobs : Jobs_Type;
   end record;

   --
   --  Execute_Initialize_Rekeying
   --
   procedure Execute_Initialize_Rekeying (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_Create_Snapshot
   --
   procedure Execute_Create_Snapshot (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_Discard_Snapshot
   --
   procedure Execute_Discard_Snapshot (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_Rekey_VBA
   --
   procedure Execute_Rekey_VBA (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_VBD_Extension_Step
   --
   procedure Execute_VBD_Extension_Step (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_FT_Extension_Step
   --
   procedure Execute_FT_Extension_Step (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Execute_Initialize
   --
   procedure Execute_Initialize (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      SB       : in out Superblock_Type;
      SB_Idx   : in out Superblocks_Index_Type;
      Curr_Gen : in out Generation_Type;
      Progress : in out Boolean);

   --
   --  Execute_Deinitialize
   --
   procedure Execute_Deinitialize (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      Progress      : in out Boolean);

   --
   --  Execute_Sync
   --
   procedure Execute_Sync (
      Job           : in out Job_Type;
      Job_Idx       :        Jobs_Index_Type;
      SB            : in out Superblock_Type;
      SB_Idx        : in out Superblocks_Index_Type;
      Curr_Gen      : in out Generation_Type;
      Progress      : in out Boolean);

   --
   --  Superblock_Enter_Rekeying_State
   --
   procedure Superblock_Enter_Rekeying_State (
      SB        : in out Superblock_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Init_SB_Ciphertext_Without_Key_Values
   --
   procedure Init_SB_Ciphertext_Without_Key_Values (
      SB_Plain  :     Superblock_Type;
      SB_Cipher : out Superblock_Ciphertext_Type);

   --
   --  Init_SB_Plaintext_Without_Key_Values
   --
   procedure Init_SB_Plaintext_Without_Key_Values (
      SB_Cipher :     Superblock_Ciphertext_Type;
      SB_Plain  : out Superblock_Type);

   --
   --  Discard_Disposable_Snapshots
   --
   procedure Discard_Disposable_Snapshots (
      Snapshots        : in out Snapshots_Type;
      Curr_Gen         :        Generation_Type;
      Last_Secured_Gen :        Generation_Type);

end CBE.Superblock_Control;
