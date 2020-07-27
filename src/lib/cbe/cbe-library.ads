--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request_Pool;
with CBE.Crypto;
with CBE.Virtual_Block_Device;
with CBE.Write_Back;
with CBE.Block_IO;
with CBE.Request;
with CBE.Primitive;
with CBE.Cache;
with CBE.New_Free_Tree;
with CBE.Meta_Tree;
with CBE.Superblock_Control;
with CBE.Trust_Anchor;
with CBE.VBD_Rekeying;
with CBE.FT_Resizing;
with CBE.MT_Resizing;
with CBE.TA_Request;

package CBE.Library
with SPARK_Mode
is
   --  FIXME cannot be pure yet because of CBE.Crypto
   --  pragma Pure;

   type Object_Type is private;

   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type);

   --
   --  Active_Snapshot_IDs
   --
   procedure Active_Snapshot_IDs (
      Obj  :     Object_Type;
      List : out Active_Snapshot_IDs_Type);

   --
   --  Info
   --
   procedure Info (
      Obj  :     Object_Type;
      Info : out Info_Type);

   --
   --  Check if the CBE can accept a new requeust
   --
   --  \return true if a request can be accepted, otherwise false
   --
   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit a new request
   --
   --  This method must only be called after executing 'Request_Acceptable'
   --  returned true.
   --
   --  \param Req  block request
   --
   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      ID  :        Snapshot_ID_Type);

   --
   --  Check for any completed request
   --
   --  \return a valid block request will be returned if there is an
   --         completed request, otherwise an invalid one
   --
   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type;

   --
   --  Drops the completed request
   --
   --  This method must only be called after executing
   --  'Peek_Completed_Request' returned a valid request.
   --
   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Return write request for the backend block session
   --
   --  \param Req  return valid request in case the is one pending that
   --             needs data, otherwise an invalid one is returned
   --
   procedure Has_IO_Request (
      Obj      :     Object_Type;
      Req      : out Request.Object_Type;
      Data_Idx : out Block_IO.Data_Index_Type);

   --
   --  Obtain data for write request for the backend block session
   --
   --  The CBE will transfer the payload to the given data.
   --
   --  \param Req       reference to the request processed by the CBE
   --  \param Data      reference to the data associated with the request
   --  \param Progress  return true if the CBE could process the request
   --
   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type);

   --
   --  Submit read request data from the backend block session to the CBE
   --
   --  The given data will be transfered to the CBE.
   --
   --  \param Req       reference to the request from the CBE
   --  \param Data      reference to the data associated with the request
   --  \param Progress  return true if the CBE acknowledged the request
   --
   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean);

   --
   --  Client_Transfer_Read_Data_Required
   --
   procedure Client_Transfer_Read_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Read_Data_In_Progress
   --
   procedure Client_Transfer_Read_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Read_Data_Completed
   --
   procedure Client_Transfer_Read_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean);

   --
   --  Client_Transfer_Write_Data_Required
   --
   procedure Client_Transfer_Write_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Write_Data_In_Progress
   --
   procedure Client_Transfer_Write_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Client_Transfer_Write_Data_Completed
   --
   procedure Client_Transfer_Write_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean);

   --
   --  Crypto_Add_Key_Required
   --
   procedure Crypto_Add_Key_Required (
      Obj :     Object_Type;
      Req : out Request.Object_Type;
      Key : out Key_Plaintext_Type);

   --
   --  Crypto_Add_Key_Requested
   --
   procedure Crypto_Add_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Add_Key_Completed
   --
   procedure Crypto_Add_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Remove_Key_Required
   --
   procedure Crypto_Remove_Key_Required (
      Obj    :     Object_Type;
      Req    : out Request.Object_Type;
      Key_ID : out Key_ID_Type);

   --
   --  Crypto_Remove_Key_Requested
   --
   procedure Crypto_Remove_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Remove_Key_Completed
   --
   procedure Crypto_Remove_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Crypto_Cipher_Data_Required
   --
   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type);

   --
   --  Crypto_Cipher_Data_Requested
   --
   procedure Crypto_Cipher_Data_Requested (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type);

   --
   --  Supply_Crypto_Cipher_Data
   --
   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Crypto_Plain_Data_Required
   --
   procedure Crypto_Plain_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Cipher_Buffer_Index_Type);

   --
   --  Crypto_Plain_Data_Requested
   --
   procedure Crypto_Plain_Data_Requested (
      Obj            : in out Library.Object_Type;
      Cipher_Buf_Idx :        Crypto.Cipher_Buffer_Index_Type);

   --
   --  Supply_Crypto_Plain_Data
   --
   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean);

   --
   --  Peek generated trust-anchor request
   --
   procedure Peek_Generated_TA_Request (
      Obj :     Object_Type;
      Req : out TA_Request.Object_Type);

   --
   --  Drop generated trust-anchor request
   --
   procedure Drop_Generated_TA_Request (
      Obj : in out Object_Type;
      Req :        TA_Request.Object_Type);

   --
   --  Peek generated trust-anchor superblock hash
   --
   procedure Peek_Generated_TA_SB_Hash (
      Obj  :     Object_Type;
      Req  :     TA_Request.Object_Type;
      Hash : out Hash_Type);

   --
   --  Peek generated trust-anchor key value ciphertext
   --
   procedure Peek_Generated_TA_Key_Cipher (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Ciphertext_Type);

   --
   --  Peek generated trust-anchor key value plaintext
   --
   procedure Peek_Generated_TA_Key_Plain (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Plaintext_Type);

   --
   --  Mark generated TA create key request complete
   --
   procedure Mark_Generated_TA_Create_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark generated TA secure superblock request complete
   --
   procedure Mark_Generated_TA_Secure_SB_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type);

   --
   --  Mark generated TA decrypt key request complete
   --
   procedure Mark_Generated_TA_Decrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type);

   --
   --  Mark generated TA encrypt key request complete
   --
   procedure Mark_Generated_TA_Encrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type);

   --
   --  Max_VBA
   --
   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type;

   function Execute_Progress (Obj : Object_Type)
   return Boolean;

   function To_String (Obj : Object_Type) return String;

private

   function Advance_Superblocks_Index is new
      Advance_Index (Superblocks_Index_Type);

   Free_Tree_Retry_Limit : constant := 3;

   type Free_Tree_Retry_Count_Type is range 0 .. Free_Tree_Retry_Limit;

   --
   --  Defining the structure here is just an interims solution
   --  and should be properly managed, especially handling more
   --  than one request is "missing".
   --
   type Event_Type is (
      Event_Invalid,
      Event_Supply_Client_Data_After_VBD,
      Event_Supply_Client_Data_After_FT,
      Event_IO_Request_Completed,
      Event_Obtain_Client_Data);

   type Wait_For_Event_Type is record
      Req         : Request.Object_Type;
      Prim        : Primitive.Object_Type;
      Event       : Event_Type;
      In_Progress : Boolean;
   end record;

   type Cache_Prim_State_Type is (Invalid, Submitted, Complete);
   type SCD_State_Type is (Inactive, Active);

   function Wait_For_Event_Invalid
   return Wait_For_Event_Type
   is (
      Req         => Request.Invalid_Object,
      Prim        => Primitive.Invalid_Object,
      Event       => Event_Invalid,
      In_Progress => False);

   type Cache_Sync_State_Type is (Inactive, Active);

   type Arbiter_State_Type is
      (Invalid, Read_Request, Write_Request, Sync_Request);

   function To_String (S : Arbiter_State_Type) return String
   is (
      case S is
      when Invalid => "Invalid",
      when Read_Request => "Read_Request",
      when Write_Request => "Write_Request",
      when Sync_Request => "Sync_Request");

   type Read_Request_State_Type is
      (Invalid, Translate, Decrypt, Complete);

   type Write_Request_State_Type is
      (Invalid, Translate, Encrypt, Complete);

   type Sync_Request_State_Type is
      (Invalid, Cache_Writeback, Superblock_Update, Superblock_Writeback);

   type Object_Type is record

      Execute_Progress        : Boolean;
      Cache_Obj               : Cache.Cache_Type;
      Cache_Jobs_Data         : Cache.Jobs_Data_Type;
      Cache_Slots_Data        : Cache.Slots_Data_Type;
      Cache_Sync_State        : Cache_Sync_State_Type;
      Request_Pool_Obj        : Request_Pool.Object_Type;
      Crypto_Obj              : Crypto.Object_Type;
      IO_Obj                  : Block_IO.Object_Type;
      Trans_Data              : Translation_Data_Type;
      VBD                     : Virtual_Block_Device.Object_Type;
      Write_Back_Obj          : Write_Back.Object_Type;
      Write_Back_Data         : Write_Back.Data_Type;
      New_Free_Tree_Obj       : New_Free_Tree.Object_Type;
      Free_Tree_Retry_Count   : Free_Tree_Retry_Count_Type;
      New_Free_Tree_Prim      : Primitive.Object_Type;
      Meta_Tree_Obj           : Meta_Tree.Object_Type;
      Cur_SB                  : Superblocks_Index_Type;
      Cur_Gen                 : Generation_Type;
      Last_Secured_Generation : Generation_Type;
      Secure_Superblock       : Boolean;
      Wait_For_Front_End      : Wait_For_Event_Type;
      Superblock              : Superblock_Type;

      SCD_State    : SCD_State_Type;
      SCD_Req      : Request.Object_Type;
      SCD_Data     : Block_Data_Type;
      SCD_Curr_Lvl : Tree_Level_Index_Type;

      SCD_Cache_Prim        : Primitive.Object_Type;
      SCD_Cache_Prim_State  : Cache_Prim_State_Type;
      SCD_Cache_Prim_Data   : Block_Data_Type;

      --
      --  The array of new_PBA will either get populated from the Old_PBA
      --  content or from newly allocated blocks.
      --  The order of the array items corresponds to the level within
      --  the tree.
      --
      SCD_New_PBAs   : Write_Back.New_PBAs_Type := (others => 0);
      SCD_New_Blocks : Number_Of_Blocks_Type := 0;

      --
      --  This array contains all blocks that will get freed or rather
      --  marked as reserved in the FT as they are still referenced by
      --  an snapshot.
      --
      SCD_Free_Blocks : Tree_Level_Index_Type := 0;

      WB_Update_PBA : Physical_Block_Address_Type;

      WB_Cache_Prim_1       : Primitive.Object_Type;
      WB_Cache_Prim_1_State : Cache_Prim_State_Type;
      WB_Cache_Prim_1_Data  : Block_Data_Type;

      WB_Cache_Prim_2       : Primitive.Object_Type;
      WB_Cache_Prim_2_State : Cache_Prim_State_Type;
      WB_Cache_Prim_2_Data  : Block_Data_Type;

      WB_Cache_Prim_3       : Primitive.Object_Type;
      WB_Cache_Prim_3_State : Cache_Prim_State_Type;
      WB_Cache_Prim_3_Data  : Block_Data_Type;
      WB_Prim               : Primitive.Object_Type;

      SB_Ctrl : Superblock_Control.Control_Type;
      TA      : Trust_Anchor.Anchor_Type;
      VBD_Rkg : VBD_Rekeying.Rekeying_Type;
      FT_Rszg : FT_Resizing.Resizing_Type;
      MT_Rszg : MT_Resizing.Resizing_Type;

   end record;

   function To_String (WFE : Wait_For_Event_Type) return String;

   function Curr_Snap (Obj : Object_Type)
   return Snapshots_Index_Type;

   function Snap_Slot_For_ID (
      Obj : Object_Type;
      ID  : Generation_Type)
   return Snapshots_Index_Type;

   --
   --  Idx_Of_Any_Invalid_Snap
   --
   function Idx_Of_Any_Invalid_Snap (Snapshots : Snapshots_Type)
   return Snapshots_Index_Type;

   function Front_End_Busy_With_Other_Request (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Boolean
   is (not Request.Equal (Obj.Wait_For_Front_End.Req, Req));

   procedure Start_Waiting_For_Front_End (
      Obj   : in out Object_Type;
      Prim  :        Primitive.Object_Type;
      Event :        Event_Type);

   function To_String (Event : Event_Type)
   return String
   is (
      case Event is
      when Event_Invalid                      => "Invalid",
      when Event_Supply_Client_Data_After_VBD =>
         "Supply_Client_Data_After_VBD",
      when Event_Supply_Client_Data_After_FT  => "Supply_Client_Data_After_FT",
      when Event_IO_Request_Completed         => "IO_Request_Completed",
      when Event_Obtain_Client_Data           => "Obtain_Client_Data");

   procedure Execute_VBD (
      Obj              : in out Object_Type;
      Crypto_Plain_Buf : in out Crypto.Plain_Buffer_Type;
      Progress         : in out Boolean);

   procedure Execute_Free_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   procedure Execute_Meta_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   procedure Execute_SCD (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_Request_Pool
   --
   procedure Execute_Request_Pool (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_SB_Ctrl
   --
   procedure Execute_SB_Ctrl (
      Obj        : in out Object_Type;
      Blk_IO_Buf : in out Block_IO.Data_Type;
      Progress   : in out Boolean);

   --
   --  Execute_TA
   --
   procedure Execute_TA (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_VBD_Rkg
   --
   procedure Execute_VBD_Rkg (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   --
   --  Execute_FT_Rszg
   --
   procedure Execute_FT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Execute_MT_Rszg
   --
   procedure Execute_MT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   procedure Execute_Writeback (
      Obj              : in out Object_Type;
      IO_Buf           : in out Block_IO.Data_Type;
      Crypto_Plain_Buf : in out Crypto.Plain_Buffer_Type;
      Progress         : in out Boolean);

   --
   --  Execute_Crypto
   --
   procedure Execute_Crypto (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  :        Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf :        Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   procedure Execute_IO (
      Obj               : in out Object_Type;
      IO_Buf            :        Block_IO.Data_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean);

   --
   --  Execute_Cache
   --
   procedure Execute_Cache (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean);

   --
   --  Execute_Cache_Generated_Prims
   --
   procedure Execute_Cache_Generated_Prims (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean);

   --
   --  Execute_Cache_Completed_Prims
   --
   procedure Execute_Cache_Completed_Prims (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

end CBE.Library;
