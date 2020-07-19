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

package body CBE.Library
with SPARK_Mode
is
   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin

      Obj.Execute_Progress := False;
      Obj.Request_Pool_Obj := Request_Pool.Initialized_Object;
      Obj.Crypto_Obj       := Crypto.Initialized_Object;

      Obj.IO_Obj := Block_IO.Initialized_Object;

      Cache.Initialize (Obj.Cache_Obj);

      Obj.Cache_Jobs_Data  := (others => (others => 0));
      Obj.Cache_Slots_Data := (others => (others => 0));
      Obj.Cache_Sync_State := Inactive;
      Obj.Trans_Data       := (others => (others => 0));
      Obj.VBD              := Virtual_Block_Device.Initialized_Object;
      Obj.Write_Back_Obj   := Write_Back.Initialized_Object;
      Obj.Write_Back_Data  := (others => (others => 0));

      New_Free_Tree.Initialized_Object (Obj.New_Free_Tree_Obj);
      Obj.New_Free_Tree_Prim := Primitive.Invalid_Object;
      Meta_Tree.Initialized_Object (Obj.Meta_Tree_Obj);

      Obj.Free_Tree_Retry_Count   := 0;

      Obj.Secure_Superblock := False;
      Obj.Wait_For_Front_End := Wait_For_Event_Invalid;

      Obj.Superblock := Superblock_Invalid;
      Obj.Cur_Gen := Generation_Type'First;
      Obj.Cur_SB := Superblocks_Index_Type'First;
      Obj.Last_Secured_Generation := Generation_Type'First;

      Obj.SCD_State       := Inactive;
      Obj.SCD_Req         := Request.Invalid_Object;
      Obj.SCD_Data        := (others => 0);
      Obj.SCD_Curr_Lvl    := 1;
      Obj.SCD_New_PBAs    := (others => 0);
      Obj.SCD_New_Blocks  := 0;
      Obj.SCD_Free_Blocks := 0;

      Obj.SCD_Cache_Prim := Primitive.Invalid_Object;
      Obj.SCD_Cache_Prim_State := Invalid;
      Obj.SCD_Cache_Prim_Data := (others => 0);

      Obj.WB_Update_PBA := 0;

      Obj.WB_Cache_Prim_1 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_1_State := Invalid;
      Obj.WB_Cache_Prim_1_Data := (others => 0);

      Obj.WB_Cache_Prim_2 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_2_State := Invalid;
      Obj.WB_Cache_Prim_2_Data := (others => 0);

      Obj.WB_Cache_Prim_3 := Primitive.Invalid_Object;
      Obj.WB_Cache_Prim_3_State := Invalid;
      Obj.WB_Cache_Prim_3_Data := (others => 0);

      Obj.WB_Prim := Primitive.Invalid_Object;

      FT_Resizing.Initialize_Resizing (Obj.FT_Rszg);
      MT_Resizing.Initialize_Resizing (Obj.MT_Rszg);
      Superblock_Control.Initialize_Control (Obj.SB_Ctrl);
      Trust_Anchor.Initialize_Anchor (Obj.TA);
      VBD_Rekeying.Initialize_Rekeying (Obj.VBD_Rkg);

   end Initialize_Object;

   --
   --  Info
   --
   procedure Info (
      Obj  :     Object_Type;
      Info : out Info_Type)
   is
   begin
      Superblock_Control.Info (Obj.Superblock, Info);
   end Info;

   --
   --  Active_Snapshot_IDs
   --
   procedure Active_Snapshot_IDs (
      Obj  :     Object_Type;
      List : out Active_Snapshot_IDs_Type)
   is
   begin
      Superblock_Control.Active_Snapshot_IDs (Obj.Superblock, List);
   end Active_Snapshot_IDs;

   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean
   is (Request_Pool.Request_Acceptable (Obj.Request_Pool_Obj));

   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      ID  :        Snapshot_ID_Type)
   is
   begin

      case Request.Operation (Req) is
      when
         Read |
         Write |
         Sync |
         Rekey |
         Extend_VBD |
         Extend_FT |
         Discard_Snapshot |
         Create_Snapshot |
         Deinitialize
      =>

         Request_Pool.Submit_Request (Obj.Request_Pool_Obj, Req, ID);

      when Initialize | Resume_Rekeying =>

         raise Program_Error;

      end case;

   end Submit_Client_Request;

   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type
   is
      Req : constant Request.Object_Type :=
         Request_Pool.Peek_Completed_Request (Obj.Request_Pool_Obj);
   begin

      case Request.Operation (Req) is
      when
         Read |
         Write |
         Sync |
         Rekey |
         Extend_VBD |
         Extend_FT |
         Deinitialize |
         Create_Snapshot |
         Discard_Snapshot
      =>

         return Req;

      when Initialize | Resume_Rekeying =>

         return Request.Invalid_Object;

      end case;

   end Peek_Completed_Client_Request;

   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Request_Pool.Drop_Completed_Request (Obj.Request_Pool_Obj, Req);
   end Drop_Completed_Client_Request;

   --
   --  Has_IO_Request
   --
   procedure Has_IO_Request (
      Obj      :     Object_Type;
      Req      : out Request.Object_Type;
      Data_Idx : out Block_IO.Data_Index_Type)
   is
      Prim : constant Primitive.Object_Type :=
         Block_IO.Peek_Generated_Blk_Dev_Primitive (Obj.IO_Obj);
   begin

      if Primitive.Valid (Prim) then

         Data_Idx := Block_IO.Peek_Generated_Data_Index (Obj.IO_Obj, Prim);
         Req      := Request.Valid_Object (
            Op     => Prim_Op_To_Req_Op (Primitive.Operation (Prim)),
            Succ   => False,
            Blk_Nr => Primitive.Block_Number (Prim),
            Off    => 0,
            Cnt    => 1,
            Key    => 0,
            Tg     => 0);

      else

         Req := Request.Invalid_Object;
         Data_Idx := Block_IO.Data_Index_Type'First;

      end if;

   end Has_IO_Request;

   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type)
   is
   begin
      Block_IO.Drop_Generated_Primitive_2 (Obj.IO_Obj, Data_Idx);
   end IO_Request_In_Progress;

   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean)
   is
   begin
      Block_IO.Mark_Generated_Primitive_Complete (
         Obj.IO_Obj, Data_Index, Success);
   end IO_Request_Completed;

   --
   --  Client_Transfer_Read_Data_Required
   --
   procedure Client_Transfer_Read_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type)
   is
   begin

      Declare_Prim :
      declare
         Prim : constant Primitive.Object_Type :=
            Crypto.Peek_Generated_Client_Primitive (Obj.Crypto_Obj);
      begin

         if Primitive.Valid (Prim) and then
            Primitive.Has_Tag_Crypto_IO_Client_Supply_Data (Prim)
         then

            Req := Crypto.Peek_Generated_Req (Obj.Crypto_Obj, Prim);
            VBA := Crypto.Peek_Generated_VBA (Obj.Crypto_Obj, Prim);
            Plain_Buf_Idx :=
               Crypto.Peek_Generated_Plain_Buf_Idx (Obj.Crypto_Obj, Prim);

         else

            Req := Request.Invalid_Object;
            VBA := Virtual_Block_Address_Type'First;
            Plain_Buf_Idx := Crypto.Plain_Buffer_Index_Type'First;

         end if;

      end Declare_Prim;

   end Client_Transfer_Read_Data_Required;

   --
   --  Client_Transfer_Read_Data_In_Progress
   --
   procedure Client_Transfer_Read_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive_New (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Plain_Buf_Idx));

   end Client_Transfer_Read_Data_In_Progress;

   --
   --  Client_Transfer_Read_Data_Completed
   --
   procedure Client_Transfer_Read_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean)
   is
   begin
      Crypto.Mark_Generated_Primitive_Complete (
         Obj.Crypto_Obj, Plain_Buf_Idx, Success);

   end Client_Transfer_Read_Data_Completed;

   --
   --  For now there can be only one Request pending.
   --
   function Client_Data_Index (
      Obj : Object_Type;
      Req : Request.Object_Type)
   return Primitive.Index_Type
   is
   begin
      if Front_End_Busy_With_Other_Request (Obj, Req) then
         return Primitive.Invalid_Index;
      end if;

      return Primitive.Index (Obj.Wait_For_Front_End.Prim);
   end Client_Data_Index;

   --
   --  Client_Data_Required
   --
   procedure Client_Data_Required (
      Obj : in out Object_Type;
      Req :    out Request.Object_Type)
   is
   begin
      Req := Request.Invalid_Object;

      if Primitive.Valid (Obj.Wait_For_Front_End.Prim) then
         return;
      end if;

      --
      --  A write Request, we need the location from where to read the new
      --  leaf data.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
      begin
         if Primitive.Valid (Prim) and then Primitive.Operation (Prim) = Write
         then
            Start_Waiting_For_Front_End (
               Obj, Prim, Event_Supply_Client_Data_After_VBD);
            Req := Obj.Wait_For_Front_End.Req;
            return;
         end if;
      end;

      --
      --  The free-tree needs the data to give to the Write_Back module.
      --
      declare
         Prim : constant Primitive.Object_Type :=
            New_Free_Tree.Peek_Completed_Primitive (Obj.New_Free_Tree_Obj);
      begin

         case Primitive.Tag (Prim) is
         when Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks |
              Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks |
              Primitive.Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg
         =>

            null;

         when others =>

            if Primitive.Valid (Prim) and then Primitive.Success (Prim) then
               Start_Waiting_For_Front_End (
                  Obj, Prim, Event_Supply_Client_Data_After_FT);
               Req := Obj.Wait_For_Front_End.Req;
               return;
            end if;

         end case;

      end;

   end Client_Data_Required;

   --
   --  Supply_Client_Data
   --
   procedure Supply_Client_Data (
      Obj      : in out Object_Type;
      Req      :        Request.Object_Type;
      Data     :        Block_Data_Type;
      Progress :    out Boolean)
   is
   begin
      case Obj.SCD_State is
      when Inactive =>
         Obj.SCD_State := Active;
         Obj.SCD_Data := Data;
         Obj.SCD_Req := Req;
         Obj.SCD_Curr_Lvl := 1;
         Obj.SCD_New_PBAs := (others => 0);
         Obj.SCD_New_Blocks := 0;
         Obj.SCD_Free_Blocks := 0;
         Progress := True;
      when others =>
         raise Program_Error;
      end case;
   end Supply_Client_Data;

   --
   --  Crypto_Remove_Key_Required
   --
   procedure Crypto_Remove_Key_Required (
      Obj    :     Object_Type;
      Req    : out Request.Object_Type;
      Key_ID : out Key_ID_Type)
   is
      Idx  : Crypto.Jobs_Index_Type;
      Prim : Primitive.Object_Type;
   begin

      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj, Idx, Prim);
      if not Primitive.Valid (Prim) or else
         not Primitive.Has_Tag_SB_Ctrl_Crypto_Remove_Key (Prim)
      then
         Key_ID := Key_ID_Invalid;
         Req := Request.Invalid_Object;
         return;
      end if;

      Key_ID := Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx);
      Req := Request.Valid_Object (
         Op     => Read,
         Succ   => False,
         Blk_Nr => 0,
         Off    => 0,
         Cnt    => 1,
         Key    => 0,
         Tg     => Request.Tag_Type (Idx));

   end Crypto_Remove_Key_Required;

   --
   --  Crypto_Remove_Key_Requested
   --
   procedure Crypto_Remove_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Request.Tag (Req)));
   end Crypto_Remove_Key_Requested;

   --
   --  Crypto_Remove_Key_Completed
   --
   procedure Crypto_Remove_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj,
         Crypto.Jobs_Index_Type (Request.Tag (Req)),
         Request.Success (Req));
   end Crypto_Remove_Key_Completed;

   --
   --  Crypto_Add_Key_Required
   --
   procedure Crypto_Add_Key_Required (
      Obj :     Object_Type;
      Req : out Request.Object_Type;
      Key : out Key_Plaintext_Type)
   is
      Idx  : Crypto.Jobs_Index_Type;
      Prim : Primitive.Object_Type;
   begin

      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj, Idx, Prim);
      if not Primitive.Valid (Prim) or else
         not Primitive.Has_Tag_SB_Ctrl_Crypto_Add_Key (Prim)
      then
         Key := Key_Plaintext_Invalid;
         Req := Request.Invalid_Object;
         return;
      end if;

      Key := Crypto.Peek_Generated_Key (Obj.Crypto_Obj, Idx);
      Req := Request.Valid_Object (
         Op     => Read,
         Succ   => False,
         Blk_Nr => 0,
         Off    => 0,
         Cnt    => 1,
         Key    => 0,
         Tg     => Request.Tag_Type (Idx));

   end Crypto_Add_Key_Required;

   --
   --  Crypto_Add_Key_Requested
   --
   procedure Crypto_Add_Key_Requested (
      Obj : in out Library.Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Request.Tag (Req)));
   end Crypto_Add_Key_Requested;

   --
   --  Crypto_Add_Key_Completed
   --
   procedure Crypto_Add_Key_Completed (
      Obj : in out Object_Type;
      Req :        Request.Object_Type)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj,
         Crypto.Jobs_Index_Type (Request.Tag (Req)),
         Request.Success (Req));
   end Crypto_Add_Key_Completed;

   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type)
   is
      Idx  : Crypto.Jobs_Index_Type;
      Prim : Primitive.Object_Type;
   begin
      Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Idx, Prim);
      Data_Index := Crypto.Plain_Buffer_Index_Type (Idx);
      if not Primitive.Valid (Prim) or else
         Primitive.Has_Tag_SB_Ctrl_Crypto_Add_Key (Prim) or else
         Primitive.Operation (Prim) /= Write
      then
         Req := Request.Invalid_Object;
         return;
      end if;
      Req := Request.Valid_Object (
         Op     => CBE.Write,
         Succ   => False,
         Blk_Nr => Primitive.Block_Number (Prim),
         Off    => 0,
         Cnt    => 1,
         Key    => Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx),
         Tg     => 0);
   end Crypto_Cipher_Data_Required;

   procedure Crypto_Cipher_Data_Requested (
      Obj        : in out Library.Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Data_Index));
   end Crypto_Cipher_Data_Requested;

   procedure Supply_Crypto_Cipher_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Cipher_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Cipher_Data;

   --
   --  Crypto_Plain_Data_Required
   --
   procedure Crypto_Plain_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Cipher_Buffer_Index_Type)
   is
      Prim : constant Primitive.Object_Type :=
         Crypto.Peek_Generated_Crypto_Dev_Primitive (Obj.Crypto_Obj);
   begin

      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Crypto_IO_Crypto_Dev_Decrypt =>

            Data_Index :=
               Crypto.Peek_Generated_Cipher_Buf_Idx (Obj.Crypto_Obj, Prim);

            Req := Request.Valid_Object (
               Op     => CBE.Read,
               Succ   => False,
               Blk_Nr => Primitive.Block_Number (Prim),
               Off    => 0,
               Cnt    => 1,
               Key    =>
                  Crypto.Peek_Generated_Key_ID_New (Obj.Crypto_Obj, Prim),
               Tg     => 0);

         when others =>

            Declare_Idx :
            declare
               Idx : Crypto.Jobs_Index_Type;
               Prim_1 : Primitive.Object_Type;
            begin

               Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj, Idx, Prim_1);

               Data_Index := Crypto.Cipher_Buffer_Index_Type (Idx);
               if not Primitive.Valid (Prim_1) or else
                  Primitive.Has_Tag_SB_Ctrl_Crypto_Add_Key (Prim_1) or else
                  Primitive.Operation (Prim_1) /= Read
               then
                  Req := Request.Invalid_Object;
                  return;
               end if;
               Req := Request.Valid_Object (
                  Op     => CBE.Read,
                  Succ   => False,
                  Blk_Nr => Primitive.Block_Number (Prim_1),
                  Off    => 0,
                  Cnt    => 1,
                  Key    => Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx),
                  Tg     => 0);

            end Declare_Idx;

         end case;

      else

         Data_Index := Crypto.Cipher_Buffer_Index_Type'First;
         Req := Request.Invalid_Object;

      end if;

   end Crypto_Plain_Data_Required;

   --
   --  Crypto_Plain_Data_Requested
   --
   procedure Crypto_Plain_Data_Requested (
      Obj            : in out Library.Object_Type;
      Cipher_Buf_Idx :        Crypto.Cipher_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive_New (Obj.Crypto_Obj,
         Crypto.Jobs_Index_Type (Cipher_Buf_Idx));

   end Crypto_Plain_Data_Requested;

   --
   --  Supply_Crypto_Plain_Data
   --
   procedure Supply_Crypto_Plain_Data (
      Obj        : in out Object_Type;
      Data_Index :        Crypto.Plain_Buffer_Index_Type;
      Data_Valid :        Boolean)
   is
   begin
      Crypto.Mark_Completed_Primitive (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Data_Index), Data_Valid);
   end Supply_Crypto_Plain_Data;

   --------------
   --  private --
   --------------

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

   function Curr_Snap (Obj : Object_Type)
   return Snapshots_Index_Type
   is (Obj.Superblock.Curr_Snap);

   function Snap_Slot_For_ID (
      Obj : Object_Type;
      ID  : Generation_Type)
   return Snapshots_Index_Type
   is
   begin
      for I in Snapshots_Index_Type loop
         if Obj.Superblock.Snapshots (I).Valid and then
            Obj.Superblock.Snapshots (I).Gen = ID
         then
            return I;
         end if;
      end loop;
      raise Program_Error;
   end Snap_Slot_For_ID;

   --
   --  Idx_Of_Any_Invalid_Snap
   --
   function Idx_Of_Any_Invalid_Snap (Snapshots : Snapshots_Type)
   return Snapshots_Index_Type
   is
   begin
      Find_Invalid_Snap_Idx :
      for Idx in Snapshots'Range loop
         if not Snapshots (Idx).Valid then
            return Idx;
         end if;
      end loop Find_Invalid_Snap_Idx;
      raise Program_Error;
   end Idx_Of_Any_Invalid_Snap;

   --
   --  Max_VBA
   --
   function Max_VBA (Obj : Object_Type)
   return Virtual_Block_Address_Type
   is (
      Superblock_Control.Max_VBA (Obj.Superblock));

   procedure Execute_VBD (
      Obj              : in out Object_Type;
      Crypto_Plain_Buf : in out Crypto.Plain_Buffer_Type;
      Progress         : in out Boolean)
   is
      Prim    : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin
      Virtual_Block_Device.Execute (Obj.VBD, Obj.Trans_Data);
      if Virtual_Block_Device.Execute_Progress (Obj.VBD) then
         Progress := True;
      end if;

      Loop_Generated_Cache_Prims :
      loop
         Prim := Virtual_Block_Device.Peek_Generated_Cache_Primitive (Obj.VBD);

         exit Loop_Generated_Cache_Prims when
            not Primitive.Valid (Prim) or else
            not Cache.Primitive_Acceptable (Obj.Cache_Obj);

         Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

         if Primitive.Operation (Prim) = Write then
            Obj.Cache_Jobs_Data (Job_Idx) :=
               Virtual_Block_Device.Peek_Generated_Cache_Data (Obj.VBD);
         end if;
         Virtual_Block_Device.Drop_Generated_Cache_Primitive (
            Obj.VBD, Prim);

         Progress := True;

      end loop Loop_Generated_Cache_Prims;

      --
      --  Submit Block-IO read-primitives for completed primitives of the VBD
      --
      Loop_VBD_Completed_Prims :
      loop
         declare
            Prim : Primitive.Object_Type :=
               Virtual_Block_Device.Peek_Completed_Primitive (Obj.VBD);
         begin
            exit Loop_VBD_Completed_Prims when
               not Primitive.Valid (Prim) or else
               Primitive.Operation (Prim) /= Read;

            if Virtual_Block_Device.Peek_Completed_Generation (Obj.VBD) =
               Initial_Generation
            then
               --  write all 0 to cache entry
               --  write all 0 to result buffer
               --  mark primitive complete at request pool

               exit Loop_VBD_Completed_Prims when
                  not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

               Declare_Data_Idx :
               declare
                  Data_Idx : Crypto.Jobs_Index_Type;
               begin
                  Primitive.Success (Prim, True);
                  Crypto.Submit_Completed_Primitive (
                     Obj.Crypto_Obj,
                     Prim,
                     Virtual_Block_Device.Peek_Completed_Key_ID (Obj.VBD),
                     Data_Idx);

                  Crypto_Plain_Buf (Data_Idx) := (others => 0);
               end Declare_Data_Idx;

               Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);
               Progress := True;

            else

               exit Loop_VBD_Completed_Prims when
                  not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

               Block_IO.Submit_Primitive_Decrypt (
                  Obj.IO_Obj, Prim,
                  Virtual_Block_Device.Peek_Completed_Hash (Obj.VBD),
                  Virtual_Block_Device.Peek_Completed_Key_ID (Obj.VBD));

               Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);
               Progress := True;

            end if;
         end;
      end loop Loop_VBD_Completed_Prims;
   end Execute_VBD;

   procedure Execute_Free_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin

      --------------------------
      --  Free-tree handling  --
      --------------------------

      New_Free_Tree.Execute (
         Obj.New_Free_Tree_Obj,
         Obj.Superblock.Snapshots,
         Obj.Superblock.Last_Secured_Generation,
         Progress);

      Loop_Generated_Cache_Prims :
      loop
         Prim := New_Free_Tree.Peek_Generated_Cache_Primitive (
            Obj.New_Free_Tree_Obj);

         exit Loop_Generated_Cache_Prims when
            not Primitive.Valid (Prim) or else
            not Cache.Primitive_Acceptable (Obj.Cache_Obj);

         Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

         if Primitive.Operation (Prim) = Write then
            Obj.Cache_Jobs_Data (Job_Idx) :=
               New_Free_Tree.Peek_Generated_Cache_Data (Obj.New_Free_Tree_Obj,
                  Prim);
         end if;
         New_Free_Tree.Drop_Generated_Cache_Primitive (
            Obj.New_Free_Tree_Obj, Prim);

         Progress := True;

      end loop Loop_Generated_Cache_Prims;

      Loop_Free_Tree_Generated_Meta_Tree_Prims :
      loop
         Declare_Free_Tree_Generated_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               New_Free_Tree.Peek_Generated_Meta_Tree_Primitive (
                  Obj.New_Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Generated_Meta_Tree_Prims when
               not Primitive.Valid (Prim);
            exit Loop_Free_Tree_Generated_Meta_Tree_Prims when
               not Meta_Tree.Request_Acceptable (Obj.Meta_Tree_Obj);

            Meta_Tree.Submit_Primitive (
               Obj.Meta_Tree_Obj,
               Prim,
               (PBA => Obj.Superblock.Meta_Number,
                Gen => Obj.Superblock.Meta_Gen,
                Hash => Obj.Superblock.Meta_Hash),
               (Max_Level => Obj.Superblock.Meta_Max_Level,
                Edges     => Obj.Superblock.Meta_Degree,
                Leafs     => Obj.Superblock.Meta_Leafs),
               Obj.Cur_Gen,
               Physical_Block_Address_Type (Primitive.Block_Number (Prim)));

            New_Free_Tree.Drop_Generated_Meta_Tree_Primitive (
               Obj.New_Free_Tree_Obj, Prim);
            Obj.New_Free_Tree_Prim := Prim;
            Progress := True;
         end Declare_Free_Tree_Generated_Prim;
      end loop Loop_Free_Tree_Generated_Meta_Tree_Prims;

      Loop_Free_Tree_Completed_Prims :
      loop
         Declare_Prim_1 :
         declare
            Prim : constant Primitive.Object_Type :=
               New_Free_Tree.Peek_Completed_Primitive (Obj.New_Free_Tree_Obj);
         begin
            exit Loop_Free_Tree_Completed_Prims when
               not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks |
                 Primitive.Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks |
                 Primitive.Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg
            =>

               VBD_Rekeying.Mark_Generated_Prim_Completed_New_PBAs (
                  Obj.VBD_Rkg,
                  Prim,
                  New_Free_Tree.Peek_Completed_WB_Data (
                     Obj.New_Free_Tree_Obj,
                     Prim).New_PBAs);

               --
               --  FIXME
               --
               --  The manual update of the Free Tree root is done because
               --  the Free Tree module was designed this way before the
               --  VBD Rekeying was implemented. Normally, the Free
               --  Tree root should either be handed out by the Superblock
               --  Control to the VBD Rekeying and then by the VBD Rekeying to
               --  the Free Tree module and the resulting root would be
               --  propagated backwards as result of the Primitives. Or the
               --  Free Tree root is updated by the Free Tree module itself
               --  while processing the Rekey VBA primitive through a
               --  primitive generated for the Superblock Control.
               --
               declare
                  FT_Root_Node : constant Type_1_Node_Type :=
                     New_Free_Tree.Peek_Completed_Root_Node (
                        Obj.New_Free_Tree_Obj,
                        Prim);
               begin
                  Obj.Superblock.Free_Hash   := FT_Root_Node.Hash;
                  Obj.Superblock.Free_Number := FT_Root_Node.PBA;
                  Obj.Superblock.Free_Gen    := FT_Root_Node.Gen;
               end;

               New_Free_Tree.Drop_Completed_Primitive (
                  Obj.New_Free_Tree_Obj, Prim);

               Progress := True;

            when others =>

               exit Loop_Free_Tree_Completed_Prims when
                  Primitive.Success (Prim);

               raise Program_Error;

            end case;

         end Declare_Prim_1;

      end loop Loop_Free_Tree_Completed_Prims;
   end Execute_Free_Tree;

   procedure Execute_Meta_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Job_Idx : Cache.Jobs_Index_Type;
   begin
      Meta_Tree.Execute (Obj.Meta_Tree_Obj, Progress);

      Loop_Generated_Meta_Tree_Primitives :
      loop
         declare
            Prim : constant Primitive.Object_Type :=
               Meta_Tree.Peek_Generated_Cache_Primitive (Obj.Meta_Tree_Obj);
         begin
            exit Loop_Generated_Meta_Tree_Primitives when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Job_Idx);

            if Primitive.Operation (Prim) = Write then
               Obj.Cache_Jobs_Data (Job_Idx) :=
                  Meta_Tree.Peek_Generated_Cache_Data (Obj.Meta_Tree_Obj,
                  Prim);
            end if;

            Meta_Tree.Drop_Generated_Cache_Primitive (
               Obj.Meta_Tree_Obj, Prim);

            exit Loop_Generated_Meta_Tree_Primitives;
         end;
      end loop Loop_Generated_Meta_Tree_Primitives;

      Loop_Completed_Meta_Tree_Primitives :
      loop
         declare
            Prim : constant Primitive.Object_Type :=
               Meta_Tree.Peek_Completed_Primitive (Obj.Meta_Tree_Obj);
         begin
            exit Loop_Completed_Meta_Tree_Primitives when
               not Primitive.Valid (Prim);

            pragma Debug (Debug.Print_String (
               "Loop_Completed_Meta_Tree_Primitives: "
               & Primitive.To_String (Prim)));

            case Primitive.Tag (Prim) is
            when Primitive.Tag_FT_MT =>

               declare
                  Node : constant Type_1_Node_Type :=
                     Meta_Tree.Peek_Completed_Root_Node (Obj.Meta_Tree_Obj,
                        Prim);
               begin
                  Obj.Superblock.Meta_Gen    := Node.Gen;
                  Obj.Superblock.Meta_Number := Node.PBA;
                  Obj.Superblock.Meta_Hash   := Node.Hash;

                  pragma Debug (Debug.Print_String (
                     "Loop_Completed_Meta_Tree_Primitives: "
                     & " Node: " & Debug.To_String (Node.PBA)
                     & " Prim: " & Primitive.To_String (Prim)));
               end;

               Primitive.Success (Obj.New_Free_Tree_Prim,
                  Primitive.Success (Prim));

               New_Free_Tree.Mark_Generated_Meta_Tree_Primitive_Complete (
                  Obj.New_Free_Tree_Obj, Obj.New_Free_Tree_Prim,
                  Meta_Tree.Peek_Completed_New_PBA (Obj.Meta_Tree_Obj, Prim));

               Meta_Tree.Drop_Completed_Primitive (
                  Obj.Meta_Tree_Obj, Prim);
               Progress := True;

            when Primitive.Tag_FT_Rszg_MT_Alloc =>

               declare
                  Node : constant Type_1_Node_Type :=
                     Meta_Tree.Peek_Completed_Root_Node (Obj.Meta_Tree_Obj,
                        Prim);
               begin
                  Obj.Superblock.Meta_Gen    := Node.Gen;
                  Obj.Superblock.Meta_Number := Node.PBA;
                  Obj.Superblock.Meta_Hash   := Node.Hash;
               end;

               FT_Resizing.Mark_Generated_Prim_Completed_New_PBA (
                  Obj.FT_Rszg, Prim,
                  Meta_Tree.Peek_Completed_New_PBA (Obj.Meta_Tree_Obj, Prim));

               Meta_Tree.Drop_Completed_Primitive (
                  Obj.Meta_Tree_Obj, Prim);

               Progress := True;

            when Primitive.Tag_MT_Rszg_MT_Alloc =>

               MT_Resizing.Mark_Generated_Prim_Completed_PBA_Alloc (
                  Obj.MT_Rszg, Prim,
                  Meta_Tree.Peek_Completed_Root_Node (Obj.Meta_Tree_Obj, Prim),
                  Meta_Tree.Peek_Completed_New_PBA (Obj.Meta_Tree_Obj, Prim));

               Meta_Tree.Drop_Completed_Primitive (Obj.Meta_Tree_Obj, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;
         end;
      end loop Loop_Completed_Meta_Tree_Primitives;
   end Execute_Meta_Tree;

   procedure Execute_SCD (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : constant Primitive.Object_Type := Obj.Wait_For_Front_End.Prim;
      Curr_Lvl : constant Tree_Level_Index_Type := Obj.SCD_Curr_Lvl;
      Req  : constant Request.Object_Type := Obj.SCD_Req;
      Data : constant Block_Data_Type := Obj.SCD_Data;
   begin
      if Obj.SCD_State = Inactive then
         return;
      end if;

      --
      --  For now there is only one Request pending.
      --
      if not Request.Equal (Obj.Wait_For_Front_End.Req, Req) then
         Obj.SCD_State := Inactive;
         return;
      end if;

      if Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_FT then

         if not Write_Back.Primitive_Acceptable (Obj.Write_Back_Obj) then
            return;
         end if;

         Obj.Free_Tree_Retry_Count := 0;

         declare
            WB : constant New_Free_Tree.Write_Back_Data_Type :=
               New_Free_Tree.Peek_Completed_WB_Data (Obj.New_Free_Tree_Obj,
                  Prim);

            FT_Root_Node : constant Type_1_Node_Type :=
               New_Free_Tree.Peek_Completed_Root_Node (Obj.New_Free_Tree_Obj,
                  Prim);
         begin

            Write_Back.Submit_Primitive (
               Obj.Write_Back_Obj, WB.Prim, WB.Gen, WB.VBA, WB.New_PBAs,
               WB.Old_PBAs, WB.Tree_Max_Level, Data, Obj.Write_Back_Data);

            Obj.Superblock.Free_Hash   := FT_Root_Node.Hash;
            Obj.Superblock.Free_Number := FT_Root_Node.PBA;
            Obj.Superblock.Free_Gen    := FT_Root_Node.Gen;
         end;

         New_Free_Tree.Drop_Completed_Primitive (Obj.New_Free_Tree_Obj, Prim);

         Obj.Wait_For_Front_End := Wait_For_Event_Invalid;
         Progress := True;
         Obj.SCD_State := Inactive;
         return;

      --
      --  The VBD module translated a write Request, writing the data
      --  now to disk involves multiple steps:
      --
      --  1. Gathering of all nodes in the branch and looking up the
      --     volatile ones (those, which belong to theCurr generation
      --     and will be updated in place).
      --  2. Allocate new blocks if needed by consulting the FT
      --  3. Updating all entries in the nodes
      --  4. Writing the branch back to the block device.
      --
      --  Those steps are handled by different modules, depending on
      --  the allocation of new blocks.
      --
      elsif
         Obj.Wait_For_Front_End.Event = Event_Supply_Client_Data_After_VBD
      then

         --
         --  As usual check first we can submit new requests.
         --
         if not New_Free_Tree.Request_Acceptable (Obj.New_Free_Tree_Obj)
            or else not Virtual_Block_Device.Trans_Can_Get_Type_1_Node_Walk (
               Obj.VBD, Prim)
         then
            return;
         end if;

         --
         --  Then (ab-)use the Translation module and its still pending
         --  Request to get all old PBAs, whose generation we then check.
         --  The order of the array items corresponds to the level within
         --  the tree.
         --
         Declare_Old_PBAs : declare

            Old_PBAs : Type_1_Node_Walk_Type := (
               others => Type_1_Node_Invalid);

            Trans_Max_Level : constant Tree_Level_Index_Type :=
               Virtual_Block_Device.Tree_Max_Level (Obj.VBD);

            Snap : constant Snapshot_Type :=
               Obj.Superblock.Snapshots (Curr_Snap (Obj));

            --
            --  Get the corresponding VBA that we use to calculate the index
            --  for the edge in the node for a given level within the tree.
            --
            VBA : constant Virtual_Block_Address_Type :=
               Virtual_Block_Address_Type (
                  Virtual_Block_Device.Trans_Get_Virtual_Block_Address (
                     Obj.VBD, Prim));
         begin

            Virtual_Block_Device.Trans_Get_Type_1_Node_Walk (
               Obj.VBD, Old_PBAs);

            --
            --  Make sure we work with the proper snapshot.
            --
            --  (This check may be removed at some point.)
            --
            if Old_PBAs (Trans_Max_Level).PBA /= Snap.PBA then
               raise Program_Error;
            end if;

            --
            --  Here only the inner nodes, i.E. all nodes excluding root and
            --  leaf, are considered. The root node is checked afterwards as
            --  we need the information of theCurr snapshot for that.
            --
            for Level in Curr_Lvl .. Trans_Max_Level loop

               --
               --  Use the old PBA to get the node's data from the cache and
               --  use it check how we have to handle the node.
               --
               Declare_Nodes :
               declare
                  PBA : constant Physical_Block_Address_Type :=
                     Old_PBAs (Tree_Level_Index_Type (Level)).PBA;

                  Nodes : Type_1_Node_Block_Type;

                  Cache_Prim : constant Primitive.Object_Type :=
                     Primitive.Valid_Object_No_Pool_Idx (
                        Read, False, Primitive.Tag_SCD_Cache,
                        Block_Number_Type (PBA), 0);

                  Job_Idx : Cache.Jobs_Index_Type;
               begin

                  if Obj.SCD_Cache_Prim_State = Invalid then

                     if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                        Obj.SCD_Cache_Prim := Cache_Prim;
                        Obj.SCD_Cache_Prim_State := Submitted;
                        Cache.Submit_Primitive (
                           Obj.Cache_Obj, Obj.SCD_Cache_Prim, Job_Idx);

                        if Primitive.Operation (Obj.SCD_Cache_Prim) = Write
                        then
                           Obj.Cache_Jobs_Data (Job_Idx) :=
                              Obj.SCD_Cache_Prim_Data;
                        end if;

                        Progress := True;
                     end if;
                     return;

                  elsif Obj.SCD_Cache_Prim_State /= Complete or else
                        not Primitive.Equal (Obj.SCD_Cache_Prim, Cache_Prim)
                  then
                     return;
                  end if;

                  if not Primitive.Success (Obj.SCD_Cache_Prim) then
                     raise Program_Error;
                  end if;

                  Type_1_Node_Block_From_Block_Data (
                     Nodes, Obj.SCD_Cache_Prim_Data);

                  Declare_Generation :
                  declare
                     Child_Idx : constant Tree_Child_Index_Type :=
                        Virtual_Block_Device.Index_For_Level (
                           Obj.VBD, VBA, Tree_Level_Index_Type (Level));

                     Gen : constant Generation_Type :=
                        Nodes (Natural (Child_Idx)).Gen;
                  begin
                     --
                     --  In case the generation of the entry is the same as the
                     --  Curr generation OR if the generation is 0 (which means
                     --  it was never used before) the block is volatile and we
                     --  change it in place and store it directly in the
                     --  new_PBA array.
                     --
                     pragma Debug (Debug.Print_String ("PBA: "
                        & Debug.To_String (Debug.Uint64_Type (PBA)) & " "
                        & "Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Gen)) & " "
                        & "Cur_Gen: "
                        & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
                        & " Npba: "
                        & Debug.To_String (Debug.Uint64_Type (
                           Nodes (Natural (Child_Idx)).PBA))));

                     if Gen = Obj.Cur_Gen or else
                        (Curr_Lvl = 1 and then Gen = 0)
                     then

                        Obj.SCD_New_PBAs (Tree_Level_Index_Type (Level - 1)) :=
                           Old_PBAs (Tree_Level_Index_Type (Level - 1)).PBA;

                     --
                     --  Otherwise add the block to the free_PBA array so that
                     --  the FT will reserved it and note that we need another
                     --  new block.
                     --
                     else
                        Obj.SCD_Free_Blocks := Obj.SCD_Free_Blocks + 1;
                        Obj.SCD_New_Blocks  := Obj.SCD_New_Blocks  + 1;

                        pragma Debug (Debug.Print_String ("New_Blocks: "
                           & Debug.To_String (Debug.Uint64_Type (
                                Obj.SCD_New_Blocks))
                           & " " & Debug.To_String (Debug.Uint64_Type (
                              Old_PBAs (
                                 Tree_Level_Index_Type (Level - 1)).PBA))));
                     end if;
                  end Declare_Generation;
               end Declare_Nodes;

               Obj.SCD_Cache_Prim_State := Invalid;
               Obj.SCD_Curr_Lvl := Obj.SCD_Curr_Lvl + 1;
            end loop;

            pragma Debug (Debug.Print_String ("Snap.Gen: "
               & Debug.To_String (Debug.Uint64_Type (Snap.Gen)) & " "
               & "Cur_Gen: "
               & Debug.To_String (Debug.Uint64_Type (Obj.Cur_Gen))
               & " root PBA: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Trans_Max_Level - 1).PBA))
               & " Gen: "
               & Debug.To_String (Debug.Uint64_Type (
                  Old_PBAs (Trans_Max_Level - 1).Gen))));

            --  check root node
            if Old_PBAs (Trans_Max_Level).Gen = 0
               or else Old_PBAs (Trans_Max_Level).Gen = Obj.Cur_Gen
            then
               pragma Debug (Debug.Print_String ("Change root PBA in place"));
               Obj.SCD_New_PBAs (Trans_Max_Level) :=
                  Old_PBAs (Trans_Max_Level).PBA;
            else
               pragma Debug (Debug.Print_String ("New root PBA"));
               Obj.SCD_New_Blocks := Obj.SCD_New_Blocks  + 1;
            end if;

            --
            --  Since there are blocks we cannot change in place, use the
            --  FT module to allocate the blocks. As we have to reserve
            --  the blocks we implicitly will free (free_PBA items), pass
            --  on theCurr generation.
            --
            if Obj.SCD_New_Blocks > 0 then
               New_Free_Tree.Submit_Request (
                  Obj              => Obj.New_Free_Tree_Obj,
                  Root_Node        => (
                     PBA  => Obj.Superblock.Free_Number,
                     Gen  => Obj.Superblock.Free_Gen,
                     Hash => Obj.Superblock.Free_Hash),
                  Tree_Geom        => (
                     Max_Level => Obj.Superblock.Free_Max_Level,
                     Edges     => Obj.Superblock.Free_Degree,
                     Leafs     => Obj.Superblock.Free_Leafs),
                  Current_Gen    => Obj.Cur_Gen,
                  Free_Gen         => Obj.Cur_Gen,
                  Requested_Blocks => Obj.SCD_New_Blocks,
                  New_Blocks       => Obj.SCD_New_PBAs,
                  Old_Blocks       => Old_PBAs,
                  Max_Level        => Trans_Max_Level,
                  Req_Prim         => Prim,
                  VBA              => VBA,
                  VBD_Degree       => Obj.Superblock.Degree,
                  VBD_Highest_VBA  => Max_VBA (Obj),
                  Rekeying         => Obj.Superblock.State = Rekeying,
                  Previous_Key_ID  => Obj.Superblock.Previous_Key.ID,
                  Current_Key_ID   => Obj.Superblock.Current_Key.ID,
                  Rekeying_VBA     => Obj.Superblock.Rekeying_VBA);
            else
               --
               --  The complete branch is still part of theCurr generation,
               --  call the Write_Back module directly.
               --
               --  (We would have to check if the module can acutally accept
               --  the Request...)
               --
               Write_Back.Submit_Primitive (
                  Obj      => Obj.Write_Back_Obj,
                  Prim     => Prim,
                  Gen      => Obj.Cur_Gen,
                  VBA      => VBA,
                  New_PBAs => Obj.SCD_New_PBAs,
                  Old_PBAs => Old_PBAs,
                  N        => Trans_Max_Level,
                  Data     => Data,
                  WB_Data  => Obj.Write_Back_Data);
            end if;

            Virtual_Block_Device.Drop_Completed_Primitive (Obj.VBD);

            Obj.Wait_For_Front_End := Wait_For_Event_Invalid;

            --
            --  Inhibit translation which effectively will suspend the
            --  Translation modules operation and will stall all other
            --  pending requests to make sure all following Request will
            --  use the newest tree.
            --
            --  (It stands to reasons whether we can remove this check
            --  if we make sure that only the requests belonging to
            --  the same branch are serialized.)
            --
            Virtual_Block_Device.Trans_Inhibit_Translation (Obj.VBD);
            Progress := True;
            Obj.SCD_State := Inactive;
            return;
         end Declare_Old_PBAs;
      end if;
      Obj.SCD_State := Inactive;
   end Execute_SCD;

   --
   --  Execute_Cache_Generated_Prims
   --
   procedure Execute_Cache_Generated_Prims (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Slot_Idx : Cache.Slots_Index_Type;
      Data_Idx : Block_IO.Data_Index_Type;
   begin

      Handle_Generated_Prims :
      loop

         Cache.Peek_Generated_Primitive (Obj.Cache_Obj, Prim, Slot_Idx);
         exit Handle_Generated_Prims when
            not Primitive.Valid (Prim);

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Cache_Blk_IO =>

            exit Handle_Generated_Prims when
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Cache_Blk_IO, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) := Obj.Cache_Slots_Data (Slot_Idx);
            end if;

            Cache.Drop_Generated_Primitive (Obj.Cache_Obj, Slot_Idx);
            Progress := True;

         when others => raise Program_Error;
         end case;

      end loop Handle_Generated_Prims;

   end Execute_Cache_Generated_Prims;

   --
   --  Execute_Cache_Completed_Prims
   --
   procedure Execute_Cache_Completed_Prims (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin

      Handle_Completed_Prims :
      loop

         Cache.Peek_Completed_Primitive (Obj.Cache_Obj, Prim, Job_Idx);
         exit Handle_Completed_Prims when
            not Primitive.Valid (Prim);

         if not Primitive.Success (Prim) then
            raise Program_Error;
         end if;

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Lib_Cache_Sync =>

            case Obj.Cache_Sync_State is
            when Active =>

               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Obj.Cache_Sync_State := Inactive;
               Progress := True;

            when Inactive => raise Program_Error;
            end case;

         when Primitive.Tag_WB_Cache =>

            if Obj.WB_Cache_Prim_1_State = Submitted and then
               Primitive.Equal (Obj.WB_Cache_Prim_1, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_1_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_1_State := Complete;
               Obj.WB_Cache_Prim_1 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            elsif Obj.WB_Cache_Prim_2_State = Submitted and then
                  Primitive.Equal (Obj.WB_Cache_Prim_2, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_2_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_2_State := Complete;
               Obj.WB_Cache_Prim_2 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            elsif Obj.WB_Cache_Prim_3_State = Submitted and then
                  Primitive.Equal (Obj.WB_Cache_Prim_3, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.WB_Cache_Prim_3_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.WB_Cache_Prim_3_State := Complete;
               Obj.WB_Cache_Prim_3 := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            else
               raise Program_Error;
            end if;

         when Primitive.Tag_SCD_Cache =>

            if Obj.SCD_Cache_Prim_State = Submitted and then
               Primitive.Equal (Obj.SCD_Cache_Prim, Prim)
            then
               if Primitive.Operation (Prim) = Read then
                  Obj.SCD_Cache_Prim_Data := Obj.Cache_Jobs_Data (Job_Idx);
               end if;
               Obj.SCD_Cache_Prim_State := Complete;
               Obj.SCD_Cache_Prim := Prim;
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            else
               raise Program_Error;
            end if;

         when Primitive.Tag_VBD_Cache =>

            Virtual_Block_Device.Mark_Generated_Cache_Primitive_Complete (
                  Obj.VBD, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
            Progress := True;

         when Primitive.Tag_FT_Cache =>

            New_Free_Tree.Mark_Generated_Cache_Primitive_Complete (
               Obj.New_Free_Tree_Obj, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
            Progress := True;

         when Primitive.Tag_MT_Cache =>

            Meta_Tree.Mark_Generated_Cache_Primitive_Complete (
               Obj.Meta_Tree_Obj, Prim, Obj.Cache_Jobs_Data (Job_Idx));

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
            Progress := True;

         when Primitive.Tag_SB_Ctrl_Cache =>

            Superblock_Control.Mark_Generated_Prim_Complete (
               Obj.SB_Ctrl, Prim);

            Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);

         when Primitive.Tag_VBD_Rkg_Cache =>

            case Primitive.Operation (Prim) is
            when Read =>

               VBD_Rekeying.Mark_Generated_Prim_Completed_Blk_Data (
                  Obj.VBD_Rkg, Prim, Obj.Cache_Jobs_Data (Job_Idx));

               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            when Write | Sync =>

               VBD_Rekeying.Mark_Generated_Prim_Completed (Obj.VBD_Rkg, Prim);
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            end case;

         when Primitive.Tag_FT_Rszg_Cache =>

            case Primitive.Operation (Prim) is
            when Read =>

               FT_Resizing.Mark_Generated_Prim_Completed_Blk_Data (
                  Obj.FT_Rszg, Prim, Obj.Cache_Jobs_Data (Job_Idx));

               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            when Write | Sync =>

               FT_Resizing.Mark_Generated_Prim_Completed (Obj.FT_Rszg, Prim);
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            end case;

         when Primitive.Tag_MT_Rszg_Cache =>

            case Primitive.Operation (Prim) is
            when Read =>

               MT_Resizing.Mark_Generated_Prim_Completed_Blk_Data (
                  Obj.MT_Rszg, Prim, Obj.Cache_Jobs_Data (Job_Idx));

               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            when Write | Sync =>

               MT_Resizing.Mark_Generated_Prim_Completed (Obj.MT_Rszg, Prim);
               Cache.Drop_Completed_Primitive (Obj.Cache_Obj, Job_Idx);
               Progress := True;

            end case;

         when others => raise Program_Error;
         end case;

      end loop Handle_Completed_Prims;
   end Execute_Cache_Completed_Prims;

   --
   --  Execute_Cache
   --
   procedure Execute_Cache (
      Obj      : in out Object_Type;
      IO_Buf   : in out Block_IO.Data_Type;
      Progress : in out Boolean)
   is
   begin
      Cache.Execute (
         Obj.Cache_Obj, Obj.Cache_Slots_Data, Obj.Cache_Jobs_Data, Progress);

      Execute_Cache_Generated_Prims (Obj, IO_Buf, Progress);
      Execute_Cache_Completed_Prims (Obj, Progress);

   end Execute_Cache;

   --
   --  Execute_Request_Pool
   --
   procedure Execute_Request_Pool (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      Request_Pool.Execute (Obj.Request_Pool_Obj, Progress);

      Loop_Pool_Generated_VBD_Prims :
      loop
         Declare_VBD_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Request_Pool.Peek_Generated_VBD_Primitive (
                  Obj.Request_Pool_Obj);
         begin

            exit Loop_Pool_Generated_VBD_Prims when
               not Primitive.Valid (Prim) or else
               not Virtual_Block_Device.Primitive_Acceptable (Obj.VBD);

            Declare_Snap_Slot_Idx :
            declare
               Snap_ID : constant Snapshot_ID_Type :=
                  Request_Pool.Peek_Generated_VBD_Primitive_ID (
                     Obj.Request_Pool_Obj,
                     Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

               Snap_Slot_Idx : constant Snapshots_Index_Type := (
                  if Snap_ID = 0 then Curr_Snap (Obj)
                  else Snap_Slot_For_ID (Obj, Generation_Type (Snap_ID)));
            begin

               if Primitive.Block_Number (Prim) >
                     Block_Number_Type (
                        Obj.Superblock.Snapshots (Snap_Slot_Idx)
                           .Nr_Of_Leafs - 1)
               then

                  Request_Pool.Drop_Generated_Primitive (
                     Obj.Request_Pool_Obj,
                     Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

                  Request_Pool.Mark_Generated_Primitive_Complete (
                     Obj.Request_Pool_Obj,
                     Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
                     False);

                  Progress := True;

               else

                  Virtual_Block_Device.Submit_Primitive (
                     Obj.VBD,
                     Obj.Superblock.Snapshots (Snap_Slot_Idx).PBA,
                     Obj.Superblock.Snapshots (Snap_Slot_Idx).Gen,
                     Obj.Superblock.Snapshots (Snap_Slot_Idx).Hash,
                     Obj.Superblock.Snapshots (Snap_Slot_Idx).Max_Level,
                     Obj.Superblock.Degree,
                     Obj.Superblock.Snapshots (Snap_Slot_Idx).Nr_Of_Leafs,
                     Prim,
                     Obj.Superblock.State = Rekeying,
                     Obj.Superblock.Rekeying_VBA,
                     Obj.Superblock.Previous_Key.ID,
                     Obj.Superblock.Current_Key.ID);

                  Request_Pool.Drop_Generated_Primitive (
                     Obj.Request_Pool_Obj,
                     Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

                  Progress := True;

               end if;

            end Declare_Snap_Slot_Idx;

         end Declare_VBD_Prim;

      end loop Loop_Pool_Generated_VBD_Prims;

      Loop_Pool_Generated_SB_Ctrl_Prims :
      loop
         Declare_SB_Ctrl_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Request_Pool.Peek_Generated_SB_Ctrl_Primitive (
                  Obj.Request_Pool_Obj);
         begin

            exit Loop_Pool_Generated_SB_Ctrl_Prims when
               not Primitive.Valid (Prim) or else
               not Superblock_Control.Primitive_Acceptable (Obj.SB_Ctrl);

            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step |
               Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step
            =>

               Superblock_Control.Submit_Primitive_Nr_Of_Blks (
                  Obj.SB_Ctrl, Prim,
                  Request_Pool.Peek_Generated_Nr_Of_Blks (
                     Obj.Request_Pool_Obj, Prim));

               Request_Pool.Drop_Generated_Primitive (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

               Progress := True;

            when Primitive.Tag_Pool_SB_Ctrl_Discard_Snap =>

               Superblock_Control.Submit_Primitive_Gen (
                  Obj.SB_Ctrl, Prim,
                  Request_Pool.Peek_Generated_Gen (
                     Obj.Request_Pool_Obj, Prim));

               Request_Pool.Drop_Generated_Primitive (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

            when Primitive.Tag_Pool_SB_Ctrl_Read_VBA =>

               Superblock_Control.Submit_Primitive_Req (
                  Obj.SB_Ctrl, Prim,
                  Request_Pool.Peek_Generated_Req (
                     Obj.Request_Pool_Obj, Prim));

               Request_Pool.Drop_Generated_Primitive (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

            when
               Primitive.Tag_Pool_SB_Ctrl_Sync |
               Primitive.Tag_Pool_SB_Ctrl_Rekey_VBA |
               Primitive.Tag_Pool_SB_Ctrl_Init_Rekey |
               Primitive.Tag_Pool_SB_Ctrl_Create_Snap |
               Primitive.Tag_Pool_SB_Ctrl_Initialize |
               Primitive.Tag_Pool_SB_Ctrl_Deinitialize
            =>

               Superblock_Control.Submit_Primitive (Obj.SB_Ctrl, Prim);
               Request_Pool.Drop_Generated_Primitive (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)));

               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_SB_Ctrl_Prim;

      end loop Loop_Pool_Generated_SB_Ctrl_Prims;

   end Execute_Request_Pool;

   --
   --  Execute_FT_Rszg
   --
   procedure Execute_FT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      FT_Resizing.Execute (Obj.FT_Rszg, Progress);

      Loop_Generated_MT_Prims :
      loop

         Declare_MT_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               FT_Resizing.Peek_Generated_MT_Primitive (Obj.FT_Rszg);
         begin
            exit Loop_Generated_MT_Prims when
               not Primitive.Valid (Prim) or else
               not Meta_Tree.Request_Acceptable (Obj.Meta_Tree_Obj);

            Meta_Tree.Submit_Primitive (
               Obj.Meta_Tree_Obj,
               Prim,
               (PBA => Obj.Superblock.Meta_Number,
                Gen => Obj.Superblock.Meta_Gen,
                Hash => Obj.Superblock.Meta_Hash),
               (Max_Level => Obj.Superblock.Meta_Max_Level,
                Edges => Obj.Superblock.Meta_Degree,
                Leafs => Obj.Superblock.Meta_Leafs),
               FT_Resizing.Peek_Generated_Curr_Gen (Obj.FT_Rszg, Prim),
               FT_Resizing.Peek_Generated_Old_PBA (Obj.FT_Rszg, Prim));

            FT_Resizing.Drop_Generated_Primitive (Obj.FT_Rszg, Prim);
            Progress := True;

         end Declare_MT_Prim;

      end loop Loop_Generated_MT_Prims;

      Loop_Generated_MT_Rszg_Prims :
      loop
         Declare_MT_Rszg_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               FT_Resizing.Peek_Generated_MT_Rszg_Primitive (Obj.FT_Rszg);
         begin
            exit Loop_Generated_MT_Rszg_Prims when
               not Primitive.Valid (Prim) or else
               not MT_Resizing.Primitive_Acceptable (Obj.MT_Rszg);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_FT_Rszg_MT_Rszg_Extend_By_One_Leaf =>

               MT_Resizing.Submit_Primitive (
                  Obj.MT_Rszg, Prim, Obj.Cur_Gen,
                  (Obj.Superblock.Meta_Number,
                   Obj.Superblock.Meta_Gen,
                   Obj.Superblock.Meta_Hash),
                  Obj.Superblock.Meta_Max_Level,
                  Obj.Superblock.Meta_Leafs,
                  Obj.Superblock.Meta_Degree,
                  FT_Resizing.Peek_Generated_PBA (Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Generated_Nr_Of_PBAs (Obj.FT_Rszg, Prim));

               FT_Resizing.Drop_Generated_Primitive (Obj.FT_Rszg, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_MT_Rszg_Prim;
      end loop Loop_Generated_MT_Rszg_Prims;

      Loop_Generated_Cache_Prims :
      loop
         Declare_Cache_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               FT_Resizing.Peek_Generated_Cache_Primitive (Obj.FT_Rszg);
         begin
            exit Loop_Generated_Cache_Prims when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            case Primitive.Operation (Prim) is
            when Write =>

               Declare_Cache_Job_Idx :
               declare
                  Idx : Cache.Jobs_Index_Type;
               begin

                  Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Idx);
                  Obj.Cache_Jobs_Data (Idx) :=
                     FT_Resizing.Peek_Generated_Blk_Data (Obj.FT_Rszg, Prim);

               end Declare_Cache_Job_Idx;

               FT_Resizing.Drop_Generated_Primitive (Obj.FT_Rszg, Prim);
               Progress := True;

            when Read | Sync =>

               Cache.Submit_Primitive_Without_Data (Obj.Cache_Obj, Prim);
               FT_Resizing.Drop_Generated_Primitive (Obj.FT_Rszg, Prim);
               Progress := True;

            end case;

         end Declare_Cache_Prim;
      end loop Loop_Generated_Cache_Prims;

      Loop_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               FT_Resizing.Peek_Completed_Primitive (Obj.FT_Rszg);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step =>

               Superblock_Control.Mark_Generated_Prim_Complete_FT_Ext (
                  Obj.SB_Ctrl,
                  Prim,
                  FT_Resizing.Peek_Completed_FT_Root (Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Completed_FT_Max_Lvl_Idx (
                     Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Completed_FT_Nr_Of_Leaves (
                     Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Completed_PBA (Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Completed_Nr_Of_PBAs (Obj.FT_Rszg, Prim),
                  FT_Resizing.Peek_Completed_Nr_Of_Leaves (
                     Obj.FT_Rszg, Prim));

               FT_Resizing.Drop_Completed_Primitive (Obj.FT_Rszg, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Prim;
      end loop Loop_Completed_Prims;

   end Execute_FT_Rszg;

   --
   --  Execute_MT_Rszg
   --
   procedure Execute_MT_Rszg (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      MT_Resizing.Execute (Obj.MT_Rszg, Progress);

      Loop_Generated_MT_Prims :
      loop

         Declare_MT_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               MT_Resizing.Peek_Generated_MT_Primitive (Obj.MT_Rszg);
         begin
            exit Loop_Generated_MT_Prims when
               not Primitive.Valid (Prim) or else
               not Meta_Tree.Request_Acceptable (Obj.Meta_Tree_Obj);

            Meta_Tree.Submit_Primitive (
               Obj.Meta_Tree_Obj, Prim,
               MT_Resizing.Peek_Generated_MT_Root (Obj.MT_Rszg, Prim),
               MT_Resizing.Peek_Generated_MT_Geom (Obj.MT_Rszg, Prim),
               MT_Resizing.Peek_Generated_Curr_Gen (Obj.MT_Rszg, Prim),
               MT_Resizing.Peek_Generated_Old_PBA (Obj.MT_Rszg, Prim));

            MT_Resizing.Drop_Generated_Primitive (Obj.MT_Rszg, Prim);
            Progress := True;

         end Declare_MT_Prim;

      end loop Loop_Generated_MT_Prims;

      Loop_Generated_Cache_Prims :
      loop
         Declare_Cache_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               MT_Resizing.Peek_Generated_Cache_Primitive (Obj.MT_Rszg);
         begin
            exit Loop_Generated_Cache_Prims when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            case Primitive.Operation (Prim) is
            when Write =>

               Declare_Cache_Job_Idx :
               declare
                  Idx : Cache.Jobs_Index_Type;
               begin

                  Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Idx);
                  Obj.Cache_Jobs_Data (Idx) :=
                     MT_Resizing.Peek_Generated_Blk_Data (Obj.MT_Rszg, Prim);

               end Declare_Cache_Job_Idx;

               MT_Resizing.Drop_Generated_Primitive (Obj.MT_Rszg, Prim);
               Progress := True;

            when Read | Sync =>

               Cache.Submit_Primitive_Without_Data (Obj.Cache_Obj, Prim);
               MT_Resizing.Drop_Generated_Primitive (Obj.MT_Rszg, Prim);
               Progress := True;

            end case;

         end Declare_Cache_Prim;
      end loop Loop_Generated_Cache_Prims;

      Loop_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               MT_Resizing.Peek_Completed_Primitive (Obj.MT_Rszg);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_FT_Rszg_MT_Rszg_Extend_By_One_Leaf =>

               declare
                  MT_Root : constant Type_1_Node_Type :=
                     MT_Resizing.Peek_Completed_MT_Root (Obj.MT_Rszg, Prim);
               begin
                  Obj.Superblock.Meta_Gen    := MT_Root.Gen;
                  Obj.Superblock.Meta_Number := MT_Root.PBA;
                  Obj.Superblock.Meta_Hash   := MT_Root.Hash;

                  Obj.Superblock.Meta_Max_Level :=
                     MT_Resizing.Peek_Completed_MT_Max_Lvl_Idx (
                        Obj.MT_Rszg, Prim);

                  Obj.Superblock.Meta_Leafs :=
                     MT_Resizing.Peek_Completed_MT_Nr_Of_Leaves (
                        Obj.MT_Rszg, Prim);
               end;

               FT_Resizing.Mark_Generated_Prim_Completed_MT_Ext (
                  Obj.FT_Rszg, Prim,
                  MT_Resizing.Peek_Completed_PBA (Obj.MT_Rszg, Prim),
                  MT_Resizing.Peek_Completed_Nr_Of_PBAs (Obj.MT_Rszg, Prim),
                  MT_Resizing.Peek_Completed_Nr_Of_Leaves (Obj.MT_Rszg, Prim));

               MT_Resizing.Drop_Completed_Primitive (Obj.MT_Rszg, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Prim;
      end loop Loop_Completed_Prims;

   end Execute_MT_Rszg;

   --
   --  Execute_VBD_Rkg
   --
   procedure Execute_VBD_Rkg (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean)
   is
   begin
      VBD_Rekeying.Execute (Obj.VBD_Rkg, Progress);

      Loop_Generated_FT_Prims :
      loop

         Declare_FT_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               VBD_Rekeying.Peek_Generated_FT_Primitive (Obj.VBD_Rkg);
         begin
            exit Loop_Generated_FT_Prims when
               not Primitive.Valid (Prim) or else
               not New_Free_Tree.Request_Acceptable (Obj.New_Free_Tree_Obj);

            New_Free_Tree.Submit_Request (
               Obj.New_Free_Tree_Obj,
               (Obj.Superblock.Free_Number,
                Obj.Superblock.Free_Gen,
                Obj.Superblock.Free_Hash),
               (Obj.Superblock.Free_Max_Level,
                Obj.Superblock.Free_Degree,
                Obj.Superblock.Free_Leafs),
               Obj.Cur_Gen,
               VBD_Rekeying.Peek_Generated_Free_Gen (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_Nr_Of_Blks (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_New_PBAs (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_T1_Node_Walk (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_Max_Level (Obj.VBD_Rkg, Prim),
               Prim,
               VBD_Rekeying.Peek_Generated_VBA (Obj.VBD_Rkg, Prim),
               Obj.Superblock.Degree,
               Max_VBA (Obj),
               Obj.Superblock.State = Rekeying,
               VBD_Rekeying.Peek_Generated_Old_Key_ID (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_New_Key_ID (Obj.VBD_Rkg, Prim),
               VBD_Rekeying.Peek_Generated_VBA (Obj.VBD_Rkg, Prim));

            VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
            Progress := True;

         end Declare_FT_Prim;

      end loop Loop_Generated_FT_Prims;

      Loop_Generated_Crypto_Prims :
      loop
         Declare_Crypto_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               VBD_Rekeying.Peek_Generated_Crypto_Primitive (Obj.VBD_Rkg);
         begin
            exit Loop_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_VBD_Rkg_Crypto_Decrypt =>

               Declare_Cipher_Buf_Idx :
               declare
                  Idx : Crypto.Jobs_Index_Type;
               begin

                  Crypto.Submit_Primitive (
                     Obj.Crypto_Obj,
                     Prim,
                     VBD_Rekeying.Peek_Generated_Crypto_Key_ID (
                        Obj.VBD_Rkg, Prim),
                     Idx);

                  Crypto_Cipher_Buf (Idx) :=
                     VBD_Rekeying.Peek_Generated_Cipher_Data (
                        Obj.VBD_Rkg, Prim);

               end Declare_Cipher_Buf_Idx;

               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Primitive.Tag_VBD_Rkg_Crypto_Encrypt =>

               Declare_Plain_Buf_Idx :
               declare
                  Idx : Crypto.Jobs_Index_Type;
               begin

                  Crypto.Submit_Primitive (
                     Obj.Crypto_Obj,
                     Prim,
                     VBD_Rekeying.Peek_Generated_Crypto_Key_ID (
                        Obj.VBD_Rkg, Prim),
                     Idx);

                  Crypto_Plain_Buf (Idx) :=
                     VBD_Rekeying.Peek_Generated_Plain_Data (
                        Obj.VBD_Rkg, Prim);

               end Declare_Plain_Buf_Idx;

               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Crypto_Prim;
      end loop Loop_Generated_Crypto_Prims;

      Loop_Generated_Cache_Prims :
      loop
         Declare_Cache_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               VBD_Rekeying.Peek_Generated_Cache_Primitive (Obj.VBD_Rkg);
         begin
            exit Loop_Generated_Cache_Prims when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            case Primitive.Operation (Prim) is
            when Write =>

               Declare_Cache_Job_Idx :
               declare
                  Idx : Cache.Jobs_Index_Type;
               begin

                  Cache.Submit_Primitive (Obj.Cache_Obj, Prim, Idx);
                  Obj.Cache_Jobs_Data (Idx) :=
                     VBD_Rekeying.Peek_Generated_Blk_Data (Obj.VBD_Rkg, Prim);

               end Declare_Cache_Job_Idx;

               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Read | Sync =>

               Cache.Submit_Primitive_Without_Data (Obj.Cache_Obj, Prim);
               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            end case;

         end Declare_Cache_Prim;
      end loop Loop_Generated_Cache_Prims;

      Loop_Generated_Blk_IO_Prims :
      loop
         Declare_Blk_IO_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               VBD_Rekeying.Peek_Generated_Blk_IO_Primitive (Obj.VBD_Rkg);
         begin
            exit Loop_Generated_Blk_IO_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            case Primitive.Operation (Prim) is
            when Write =>

               Declare_Blk_IO_Buf_Idx :
               declare
                  Idx : Block_IO.Data_Index_Type;
               begin

                  Block_IO.Submit_Primitive (
                     Obj.IO_Obj, Primitive.Tag_VBD_Rkg_Blk_IO, Prim, Idx);

                  Blk_IO_Buf (Idx) :=
                     VBD_Rekeying.Peek_Generated_Blk_Data (Obj.VBD_Rkg, Prim);

               end Declare_Blk_IO_Buf_Idx;

               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Sync =>

               Block_IO.Submit_Primitive (
                  Obj.IO_Obj, Primitive.Tag_VBD_Rkg_Blk_IO, Prim);

               VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Read =>

               case Primitive.Tag (Prim) is
               when Primitive.Tag_VBD_Rkg_Blk_IO =>

                  Block_IO.Submit_Primitive (
                     Obj.IO_Obj, Primitive.Tag_VBD_Rkg_Blk_IO, Prim);

                  VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
                  Progress := True;

               when Primitive.Tag_VBD_Rkg_Blk_IO_Read_Client_Data =>

                  Block_IO.Submit_Primitive_Client_Data (
                     Obj.IO_Obj,
                     Prim,
                     VBD_Rekeying.Peek_Generated_Req (Obj.VBD_Rkg, Prim),
                     VBD_Rekeying.Peek_Generated_VBA (Obj.VBD_Rkg, Prim),
                     VBD_Rekeying.Peek_Generated_Crypto_Key_ID (
                        Obj.VBD_Rkg, Prim));

                  VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
                  Progress := True;

               when others =>

                  raise Program_Error;

               end case;

            end case;

         end Declare_Blk_IO_Prim;
      end loop Loop_Generated_Blk_IO_Prims;

      Loop_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               VBD_Rekeying.Peek_Completed_Primitive (Obj.VBD_Rkg);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA =>

               Superblock_Control.Mark_Generated_Prim_Complete_Snapshots (
                  Obj.SB_Ctrl,
                  Prim,
                  VBD_Rekeying.Peek_Completed_Snapshots (Obj.VBD_Rkg, Prim));

               VBD_Rekeying.Drop_Completed_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Read_VBA =>

               Superblock_Control.Mark_Generated_Prim_Complete (
                  Obj.SB_Ctrl, Prim);

               VBD_Rekeying.Drop_Completed_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step =>

               Superblock_Control.Mark_Generated_Prim_Complete_VBD_Ext (
                  Obj.SB_Ctrl,
                  Prim,
                  VBD_Rekeying.Peek_Completed_Snapshots (Obj.VBD_Rkg, Prim),
                  VBD_Rekeying.Peek_Completed_PBA (Obj.VBD_Rkg, Prim),
                  VBD_Rekeying.Peek_Completed_Nr_Of_PBAs (Obj.VBD_Rkg, Prim),
                  VBD_Rekeying.Peek_Completed_Nr_Of_Leaves (
                     Obj.VBD_Rkg, Prim));

               VBD_Rekeying.Drop_Completed_Primitive (Obj.VBD_Rkg, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Prim;
      end loop Loop_Completed_Prims;

   end Execute_VBD_Rkg;

   --
   --  Execute_SB_Ctrl
   --
   procedure Execute_SB_Ctrl (
      Obj        : in out Object_Type;
      Blk_IO_Buf : in out Block_IO.Data_Type;
      Progress   : in out Boolean)
   is
   begin
      Superblock_Control.Execute (
         Obj.SB_Ctrl, Obj.Superblock, Obj.Cur_SB, Obj.Cur_Gen, Progress);

      Loop_Generated_VBD_Rkg_Prims :
      loop
         Declare_VBD_Rkg_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_VBD_Rkg_Primitive (
                  Obj.SB_Ctrl);
         begin
            exit Loop_Generated_VBD_Rkg_Prims when
               not Primitive.Valid (Prim) or else
               not VBD_Rekeying.Primitive_Acceptable (Obj.VBD_Rkg);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Read_VBA =>

               VBD_Rekeying.Submit_Primitive_Read_VBA (
                  Obj.VBD_Rkg, Prim,
                  Superblock_Control.Peek_Generated_Req (Obj.SB_Ctrl, Prim),
                  Superblock_Control.Peek_Generated_Snapshot (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Snapshots_Degree (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Key_ID (
                     Obj.SB_Ctrl, Prim));

               Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA =>

               VBD_Rekeying.Submit_Primitive_Rekeying (
                  Obj.VBD_Rkg, Prim, Obj.Cur_Gen,
                  Superblock_Control.Peek_Generated_Last_Secured_Gen (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_VBA (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Snapshots (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Snapshots_Degree (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Old_Key_ID (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_New_Key_ID (
                     Obj.SB_Ctrl, Prim, Obj.Superblock));

               Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step =>

               VBD_Rekeying.Submit_Primitive_Resizing (
                  Obj.VBD_Rkg, Prim, Obj.Cur_Gen,
                  Superblock_Control.Peek_Generated_Last_Secured_Gen (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Snapshots (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Snapshots_Degree (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_PBA (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Nr_Of_Blks (
                     Obj.SB_Ctrl, Prim, Obj.Superblock));

               Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_VBD_Rkg_Prim;
      end loop Loop_Generated_VBD_Rkg_Prims;

      Loop_Generated_FT_Rszg_Prims :
      loop
         Declare_FT_Rszg_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_FT_Rszg_Primitive (
                  Obj.SB_Ctrl);
         begin
            exit Loop_Generated_FT_Rszg_Prims when
               not Primitive.Valid (Prim) or else
               not FT_Resizing.Primitive_Acceptable (Obj.FT_Rszg);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step =>

               FT_Resizing.Submit_Primitive (
                  Obj.FT_Rszg, Prim, Obj.Cur_Gen,
                  Superblock_Control.Peek_Generated_FT_Root (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_FT_Max_Lvl_Idx (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_FT_Nr_Of_Leaves (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_FT_Degree (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_PBA (
                     Obj.SB_Ctrl, Prim, Obj.Superblock),
                  Superblock_Control.Peek_Generated_Nr_Of_Blks (
                     Obj.SB_Ctrl, Prim, Obj.Superblock));

               Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_FT_Rszg_Prim;
      end loop Loop_Generated_FT_Rszg_Prims;

      Loop_Generated_TA_Prims :
      loop
         Declare_TA_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_TA_Primitive (Obj.SB_Ctrl);
         begin
            exit Loop_Generated_TA_Prims when
               not Primitive.Valid (Prim) or else
               not Trust_Anchor.Primitive_Acceptable (Obj.TA);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_TA_Create_Key =>

               Trust_Anchor.Submit_Primitive (Obj.TA, Prim);

            when Primitive.Tag_SB_Ctrl_TA_Encrypt_Key =>

               Trust_Anchor.Submit_Primitive_Key_Value_Plaintext (
                  Obj.TA, Prim,
                  Superblock_Control.Peek_Generated_Key_Value_Plaintext (
                     Obj.SB_Ctrl, Prim));

            when Primitive.Tag_SB_Ctrl_TA_Decrypt_Key =>

               Trust_Anchor.Submit_Primitive_Key_Value_Ciphertext (
                  Obj.TA, Prim,
                  Superblock_Control.Peek_Generated_Key_Value_Ciphertext (
                     Obj.SB_Ctrl, Prim));

            when Primitive.Tag_SB_Ctrl_TA_Secure_SB =>

               Trust_Anchor.Submit_Primitive_Hash (
                  Obj.TA, Prim,
                  Superblock_Control.Peek_Generated_Hash (
                     Obj.SB_Ctrl, Prim));

            when others =>

               raise Program_Error;

            end case;

            Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
            Progress := True;

         end Declare_TA_Prim;
      end loop Loop_Generated_TA_Prims;

      Loop_Generated_Crypto_Prims :
      loop
         Declare_Crypto_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_Crypto_Primitive (
                  Obj.SB_Ctrl);
         begin
            exit Loop_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_Crypto_Remove_Key =>

               Crypto.Submit_Primitive_Key_ID (
                  Obj.Crypto_Obj, Prim,
                  Superblock_Control.Peek_Generated_Key_ID (
                     Obj.SB_Ctrl, Prim));

            when Primitive.Tag_SB_Ctrl_Crypto_Add_Key =>

               Crypto.Submit_Primitive_Key (
                  Obj.Crypto_Obj, Prim,
                  Superblock_Control.Peek_Generated_Key_Plaintext (
                     Obj.SB_Ctrl, Prim));

            when others =>

               raise Program_Error;

            end case;

            Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
            Progress := True;

         end Declare_Crypto_Prim;
      end loop Loop_Generated_Crypto_Prims;

      Loop_Generated_Cache_Prims :
      loop
         Declare_Cache_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_Cache_Primitive (Obj.SB_Ctrl);
         begin
            exit Loop_Generated_Cache_Prims when
               not Primitive.Valid (Prim) or else
               not Cache.Primitive_Acceptable (Obj.Cache_Obj);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_Cache =>

               Cache.Submit_Primitive_Without_Data (Obj.Cache_Obj, Prim);

            when others =>

               raise Program_Error;

            end case;

            Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
            Progress := True;

         end Declare_Cache_Prim;
      end loop Loop_Generated_Cache_Prims;

      Loop_Generated_Blk_IO_Prims :
      loop
         Declare_Blk_IO_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Generated_Blk_IO_Primitive (
                  Obj.SB_Ctrl);
         begin
            exit Loop_Generated_Blk_IO_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB =>

               Declare_Data_Idx :
               declare
                  Data_Idx : Block_IO.Data_Index_Type;
               begin

                  Block_IO.Submit_Primitive (
                     Obj.IO_Obj, Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB, Prim,
                     Data_Idx);

                  Blk_IO_Buf (Data_Idx) :=
                     Superblock_Control.Peek_Generated_Blk_Data (
                        Obj.SB_Ctrl, Prim);

               end Declare_Data_Idx;

            when Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB =>

               Block_IO.Submit_Primitive (
                  Obj.IO_Obj, Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB, Prim);

            when Primitive.Tag_SB_Ctrl_Blk_IO_Sync =>

               Block_IO.Submit_Primitive (
                  Obj.IO_Obj, Primitive.Tag_SB_Ctrl_Blk_IO_Sync, Prim);

            when others =>

               raise Program_Error;

            end case;

            Superblock_Control.Drop_Generated_Primitive (Obj.SB_Ctrl, Prim);
            Progress := True;

         end Declare_Blk_IO_Prim;
      end loop Loop_Generated_Blk_IO_Prims;

      Loop_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Superblock_Control.Peek_Completed_Primitive (Obj.SB_Ctrl);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_Pool_SB_Ctrl_Read_VBA |
               Primitive.Tag_Pool_SB_Ctrl_Sync |
               Primitive.Tag_Pool_SB_Ctrl_Init_Rekey |
               Primitive.Tag_Pool_SB_Ctrl_Discard_Snap |
               Primitive.Tag_Pool_SB_Ctrl_Deinitialize
            =>

               Request_Pool.Mark_Generated_Primitive_Complete (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
                  Primitive.Success (Prim));

               Superblock_Control.Drop_Completed_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when
               Primitive.Tag_Pool_SB_Ctrl_Rekey_VBA |
               Primitive.Tag_Pool_SB_Ctrl_VBD_Ext_Step |
               Primitive.Tag_Pool_SB_Ctrl_FT_Ext_Step
            =>

               Request_Pool.Mark_Generated_Primitive_Complete_Req_Fin (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
                  Primitive.Success (Prim),
                  Superblock_Control.Peek_Completed_Request_Finished (
                     Obj.SB_Ctrl, Prim));

               Superblock_Control.Drop_Completed_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when Primitive.Tag_Pool_SB_Ctrl_Create_Snap =>

               Request_Pool.Mark_Generated_Primitive_Complete_Gen (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
                  Primitive.Success (Prim),
                  Superblock_Control.Peek_Completed_Generation (
                     Obj.SB_Ctrl, Prim));

               Superblock_Control.Drop_Completed_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when Primitive.Tag_Pool_SB_Ctrl_Initialize =>

               Request_Pool.Mark_Generated_Primitive_Complete_SB_State (
                  Obj.Request_Pool_Obj,
                  Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
                  Primitive.Success (Prim),
                  Obj.Superblock.State);

               Superblock_Control.Drop_Completed_Primitive (Obj.SB_Ctrl, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Prim;
      end loop Loop_Completed_Prims;

   end Execute_SB_Ctrl;

   --
   --  Execute_TA
   --
   procedure Execute_TA (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin
      Trust_Anchor.Execute (Obj.TA, Progress);

      Loop_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Trust_Anchor.Peek_Completed_Primitive (Obj.TA);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_TA_Create_Key =>

               Superblock_Control.
                  Mark_Generated_Prim_Complete_Key_Value_Plaintext (
                     Obj.SB_Ctrl, Prim,
                     Trust_Anchor.Peek_Completed_Key_Value_Plaintext (
                        Obj.TA, Prim));

               Trust_Anchor.Drop_Completed_Primitive (Obj.TA, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_TA_Encrypt_Key =>

               Superblock_Control.
                  Mark_Generated_Prim_Complete_Key_Value_Ciphertext (
                     Obj.SB_Ctrl, Prim,
                     Trust_Anchor.Peek_Completed_Key_Value_Ciphertext (
                        Obj.TA, Prim));

               Trust_Anchor.Drop_Completed_Primitive (Obj.TA, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_TA_Decrypt_Key =>

               Superblock_Control.
                  Mark_Generated_Prim_Complete_Key_Value_Plaintext (
                     Obj.SB_Ctrl, Prim,
                     Trust_Anchor.Peek_Completed_Key_Value_Plaintext (
                        Obj.TA, Prim));

               Trust_Anchor.Drop_Completed_Primitive (Obj.TA, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_TA_Secure_SB =>

               Superblock_Control.Mark_Generated_Prim_Complete (
                  Obj.SB_Ctrl, Prim);

               Trust_Anchor.Drop_Completed_Primitive (Obj.TA, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Prim;
      end loop Loop_Completed_Prims;

   end Execute_TA;

   procedure Execute_Writeback (
      Obj              : in out Object_Type;
      IO_Buf           : in out Block_IO.Data_Type;
      Crypto_Plain_Buf : in out Crypto.Plain_Buffer_Type;
      Progress         : in out Boolean)
   is
   begin
      --
      --  The Write_Back module will store a changed branch including its leaf
      --  node on the block device.
      --
      --  The way it currently operates is as follows:
      --    1. (CRYPTO)   it hands the leaf Data to the Crypto module for
      --                  encryption
      --    2. (IO)       it hands the encrypted leaf Data to I/O module to
      --                  write it to the block device
      --    3. (CACHE)    starting by the lowest inner node it will update the
      --                  node entry (PBA and Hash)
      --    4. (COMPLETE) update root PBA and root Hash
      --

      Loop_WB_Completed_Prims :
      loop
         Declare_Prim_6 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Completed_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Completed_Prims when
               not Primitive.Valid (Prim);

            if not Primitive.Success (Prim) then
               raise Program_Error;
            end if;

            if Obj.Superblock.Snapshots (Curr_Snap (Obj)).Gen < Obj.Cur_Gen
            then
               declare
                  Snap_Idx : constant Snapshots_Index_Type :=
                     Idx_Of_Any_Invalid_Snap (Obj.Superblock.Snapshots);
               begin
                  Obj.Superblock.Snapshots (Snap_Idx) :=
                     Obj.Superblock.Snapshots (Obj.Superblock.Curr_Snap);

                  Obj.Superblock.Snapshots (Snap_Idx).Gen := Obj.Cur_Gen;
                  Obj.Superblock.Snapshots (Snap_Idx).Keep := False;
                  Obj.Superblock.Curr_Snap := Snap_Idx;
               end;
            end if;

            declare
               PBA : constant Physical_Block_Address_Type :=
                  Write_Back.Peek_Completed_Root (Obj.Write_Back_Obj, Prim);

               Snap_Idx : constant Snapshots_Index_Type :=
                  Obj.Superblock.Curr_Snap;
            begin
               Obj.Superblock.Snapshots (Snap_Idx).Gen := Obj.Cur_Gen;
               Obj.Superblock.Snapshots (Snap_Idx).PBA := PBA;
               Write_Back.Peek_Completed_Root_Hash (
                  Obj.Write_Back_Obj, Prim,
                  Obj.Superblock.Snapshots (Snap_Idx).Hash);
            end;

            --
            --  We touched the super-block, either by updating a snapshot or by
            --  creating a new one - make sure it gets secured within the next
            --  interval.
            --
            Write_Back.Drop_Completed_Primitive (Obj.Write_Back_Obj, Prim);

            --
            --  Since the write Request is finally finished, all nodes stored
            --  at some place "save" (leafs on the block device, inner nodes
            --  within the Cache, acknowledge the primitive.
            --
            Request_Pool.Mark_Generated_Primitive_Complete (
               Obj.Request_Pool_Obj,
               Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim)),
               Primitive.Success (Prim));

            pragma Debug (Debug.Print_String (
               "========> Request_Pool.Mark_Completed_Primitive: "
               & Primitive.To_String (Prim)));

         end Declare_Prim_6;
         Progress := True;

         --
         --  FIXME stalling translation as long as the write-back takes places
         --        is not a good idea
         --
         Virtual_Block_Device.Trans_Resume_Translation (Obj.VBD);

      end loop Loop_WB_Completed_Prims;

      --
      --  Give the leaf Data to the Crypto module.
      --
      Loop_WB_Generated_Crypto_Prims :
      loop

         Declare_Prim_7 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_Crypto_Primitive (Obj.Write_Back_Obj);
         begin
            exit Loop_WB_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            --
            --  The Data will be copied into the Crypto module's internal
            --  buffer
            --
            Declare_Crypto_Data :
            declare
               Plain_Data_Index : constant Write_Back.Data_Index_Type :=
                  Write_Back.Peek_Generated_Crypto_Data (
                     Obj.Write_Back_Obj, Prim);

               Data_Idx : Crypto.Jobs_Index_Type;
            begin
               Crypto.Submit_Primitive (
                  Obj.Crypto_Obj,
                  Prim,
                  Obj.Superblock.Current_Key.ID,
                  Data_Idx);

               Crypto_Plain_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Plain_Data_Index);

            end Declare_Crypto_Data;
            Write_Back.Drop_Generated_Crypto_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_7;
         Progress := True;

      end loop Loop_WB_Generated_Crypto_Prims;

      --
      --  Pass the encrypted leaf Data to the I/O module.
      --
      Loop_WB_Generated_IO_Prims :
      loop
         Declare_Prim_8 :
         declare
            Prim : constant Primitive.Object_Type :=
               Write_Back.Peek_Generated_IO_Primitive (Obj.Write_Back_Obj);

            Data_Idx : Block_IO.Data_Index_Type;
         begin
            exit Loop_WB_Generated_IO_Prims when
               not Primitive.Valid (Prim) or else
               not Block_IO.Primitive_Acceptable (Obj.IO_Obj);

            Block_IO.Submit_Primitive (
               Obj.IO_Obj, Primitive.Tag_Write_Back, Prim, Data_Idx);

            if Primitive.Operation (Prim) = Write then
               IO_Buf (Data_Idx) :=
                  Obj.Write_Back_Data (Write_Back.Peek_Generated_IO_Data (
                     Obj.Write_Back_Obj, Prim));
            end if;

            Write_Back.Drop_Generated_IO_Primitive (
               Obj.Write_Back_Obj, Prim);

         end Declare_Prim_8;
         Progress := True;

      end loop Loop_WB_Generated_IO_Prims;

      --
      --  Update the inner nodes of the tree. This is always done after the
      --  encrypted leaf node was stored by the I/O module.
      --
      Loop_WB_Generated_Cache_Prims :
      loop
         if not Primitive.Valid (Obj.WB_Prim) then
            Obj.WB_Prim :=
               Write_Back.Peek_Generated_Cache_Primitive (
                  Obj.Write_Back_Obj);
         end if;

         exit Loop_WB_Generated_Cache_Prims when
            not Primitive.Valid (Obj.WB_Prim);

         if Obj.WB_Cache_Prim_1_State = Invalid then
            Obj.WB_Update_PBA :=
               Write_Back.Peek_Generated_Cache_Update_PBA (
                  Obj.Write_Back_Obj, Obj.WB_Prim);
         end if;

         Declare_PBAs :
         declare
            PBA : constant Physical_Block_Address_Type :=
               Physical_Block_Address_Type (
                  Primitive.Block_Number (Obj.WB_Prim));

            Cache_Prim_1 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (PBA), 0);

            Cache_Prim_2 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Read, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (Obj.WB_Update_PBA), 0);

            Cache_Prim_3 : constant Primitive.Object_Type :=
               Primitive.Valid_Object_No_Pool_Idx (
                  Write, False, Primitive.Tag_WB_Cache,
                  Block_Number_Type (Obj.WB_Update_PBA), 0);

            Job_Idx : Cache.Jobs_Index_Type;
         begin

            if Obj.WB_Cache_Prim_1_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Obj.WB_Cache_Prim_1 := Cache_Prim_1;
                  Obj.WB_Cache_Prim_1_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_1, Job_Idx);

                  if Primitive.Operation (Cache_Prim_1) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_1_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_1_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_1, Cache_Prim_1)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            if not Primitive.Success (Obj.WB_Cache_Prim_1) then
               raise Program_Error;
            end if;

            if Obj.WB_Cache_Prim_2_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Obj.WB_Cache_Prim_2 := Cache_Prim_2;
                  Obj.WB_Cache_Prim_2_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_2, Job_Idx);

                  if Primitive.Operation (Cache_Prim_2) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_1_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_2_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_2, Cache_Prim_2)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            if not Primitive.Success (Obj.WB_Cache_Prim_2) then
               raise Program_Error;
            end if;

            if Obj.WB_Cache_Prim_3_State = Invalid then

               if Cache.Primitive_Acceptable (Obj.Cache_Obj) then

                  Write_Back.Drop_Generated_Cache_Primitive (
                     Obj.Write_Back_Obj, Obj.WB_Prim);

                  Write_Back.Update (
                     Obj.Write_Back_Obj,
                     PBA, Virtual_Block_Device.Get_Tree_Helper (Obj.VBD),
                     Obj.WB_Cache_Prim_1_Data, Obj.WB_Cache_Prim_2_Data);

                  Obj.WB_Cache_Prim_3 := Cache_Prim_3;
                  Obj.WB_Cache_Prim_3_State := Submitted;
                  Cache.Submit_Primitive (
                     Obj.Cache_Obj, Obj.WB_Cache_Prim_3, Job_Idx);

                  if Primitive.Operation (Cache_Prim_3) = Write then
                     Obj.Cache_Jobs_Data (Job_Idx) := Obj.WB_Cache_Prim_2_Data;
                  end if;

                  Progress := True;
               end if;
               exit Loop_WB_Generated_Cache_Prims;

            elsif Obj.WB_Cache_Prim_3_State /= Complete or else
                  not Primitive.Equal (Obj.WB_Cache_Prim_3, Cache_Prim_3)
            then
               exit Loop_WB_Generated_Cache_Prims;
            end if;

            Obj.WB_Cache_Prim_1_State := Invalid;
            Obj.WB_Cache_Prim_2_State := Invalid;
            Obj.WB_Cache_Prim_3_State := Invalid;
            Obj.WB_Prim := Primitive.Invalid_Object;

         end Declare_PBAs;

      end loop Loop_WB_Generated_Cache_Prims;

   end Execute_Writeback;

   procedure Execute_Crypto (
      Obj               : in out Object_Type;
      Crypto_Plain_Buf  :        Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf :        Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean)
   is
   begin
      Crypto.Execute (Obj.Crypto_Obj, Progress);

      --
      --  Only writes primitives (encrypted Data) are handled here,
      --  read primitives (decrypred Data) are handled in 'give_Read_Data'.
      --
      Loop_Crypto_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
         begin

            --
            --  FIXME
            --  By default, primitives of the crypto module are treated in a
            --  special way as the initial integration of the module was done
            --  breaking several principles of the modular design of the CBE.
            --  However, newer modules (like VBD Rekeying) use the crypto
            --  module in a simple server-client fashion for
            --  encryption/decryption requests, as originally intended. We
            --  filter those out through their tags.
            --
            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_Crypto_Add_Key |
                 Primitive.Tag_SB_Ctrl_Crypto_Remove_Key
            =>

               Superblock_Control.Mark_Generated_Prim_Complete (
                  Obj.SB_Ctrl, Prim);

               Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
               Progress := True;

            when Primitive.Tag_Blk_IO_Crypto_Decrypt_And_Supply_Client_Data =>

               Block_IO.Mark_Generated_Primitive_Complete_New (
                  Obj.IO_Obj, Prim);
               Crypto.Drop_Completed_Primitive_New (Obj.Crypto_Obj, Prim);
               Progress := True;

            when Primitive.Tag_VBD_Rkg_Crypto_Decrypt =>

               VBD_Rekeying.Mark_Generated_Prim_Completed_Plain_Data (
                  Obj.VBD_Rkg, Prim,
                  Crypto_Plain_Buf (Crypto.Data_Index (Obj.Crypto_Obj, Prim)));

               Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
               Progress := True;

            when Primitive.Tag_VBD_Rkg_Crypto_Encrypt =>

               VBD_Rekeying.Mark_Generated_Prim_Completed_Cipher_Data (
                  Obj.VBD_Rkg, Prim,
                  Crypto_Cipher_Buf (
                     Crypto.Data_Index (Obj.Crypto_Obj, Prim)));

               Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
               Progress := True;

            when others =>

               exit Loop_Crypto_Completed_Prims when
                  not Primitive.Valid (Prim) or else
                  Primitive.Operation (Prim) = Read;

               if not Primitive.Success (Prim) then
                  raise Program_Error;
               end if;

               Declare_Index_2 :
               declare
                  Index : constant Write_Back.Data_Index_Type :=
                     Write_Back.Peek_Generated_Crypto_Data (
                        Obj.Write_Back_Obj, Prim);
               begin
                  --
                  --  FIXME instead of copying the Data just ask the crypto
                  --        module for the resulting Hash and omit further
                  --        processing in case the operation failed
                  --
                  Obj.Write_Back_Data (Index) := Crypto_Cipher_Buf (
                     Crypto.Data_Index (Obj.Crypto_Obj, Prim));

                  Write_Back.Mark_Completed_Crypto_Primitive (
                     Obj.Write_Back_Obj, Prim, Obj.Write_Back_Data (Index));

               end Declare_Index_2;
               Crypto.Drop_Completed_Primitive (Obj.Crypto_Obj);
                  null;

               Progress := True;

            end case;

         end Declare_Prim;

      end loop Loop_Crypto_Completed_Prims;

   end Execute_Crypto;

   procedure Execute_IO (
      Obj               : in out Object_Type;
      IO_Buf            : in     Block_IO.Data_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean)
   is
   begin
      --
      --  This module handles all the block backend I/O and has to
      --  work with all most all modules. IT uses the 'Tag' field
      --  to differentiate the modules.
      --
      Block_IO.Execute (Obj.IO_Obj, Progress);

      Loop_Generated_Crypto_Prims :
      loop
         Declare_Crypto_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Block_IO.Peek_Generated_Crypto_Primitive (Obj.IO_Obj);
         begin
            exit Loop_Generated_Crypto_Prims when
               not Primitive.Valid (Prim) or else
               not Crypto.Primitive_Acceptable (Obj.Crypto_Obj);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_Blk_IO_Crypto_Decrypt_And_Supply_Client_Data =>

               Declare_Cipher_Buf_Idx :
               declare
                  Idx : Crypto.Cipher_Buffer_Index_Type;
               begin

                  Crypto.Submit_Primitive_Client_Data (
                     Obj.Crypto_Obj,
                     Prim,
                     Block_IO.Peek_Generated_Req (Obj.IO_Obj, Prim),
                     Block_IO.Peek_Generated_VBA (Obj.IO_Obj, Prim),
                     Block_IO.Peek_Generated_Key_ID (Obj.IO_Obj, Prim),
                     Idx);

                  Crypto_Cipher_Buf (Crypto.Jobs_Index_Type (Idx)) :=
                     IO_Buf (
                        Block_IO.Peek_Generated_Data_Index (Obj.IO_Obj, Prim));

               end Declare_Cipher_Buf_Idx;

               Block_IO.Drop_Generated_Primitive_New (Obj.IO_Obj, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Crypto_Prim;
      end loop Loop_Generated_Crypto_Prims;

      Loop_IO_Completed_Prims :
      loop
         Declare_Prim_15 :
         declare
            Prim : constant Primitive.Object_Type :=
               Block_IO.Peek_Completed_Primitive (Obj.IO_Obj);
         begin
            exit Loop_IO_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_VBD_Rkg_Blk_IO_Read_Client_Data =>

               VBD_Rekeying.Mark_Generated_Prim_Completed (
                  Obj.VBD_Rkg, Prim);

               Block_IO.Drop_Completed_Primitive_New (Obj.IO_Obj, Prim);
               Progress := True;

            when others =>

               if not Primitive.Success (Prim) then
                  raise Program_Error;
               end if;

               Declare_Index_3 :
               declare
                  Index : constant Block_IO.Data_Index_Type :=
                     Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj);

                  --
                  --  Whenever we cannot hand a successful primitive over
                  --  to the corresponding module, leave the loop but keep
                  --  the completed primitive so that it might be processed
                  --  next time.
                  --
                  Mod_Progress : Boolean := True;
               begin
                  if Primitive.Has_Tag_Decrypt (Prim) then

                     if not Crypto.Primitive_Acceptable (Obj.Crypto_Obj) then
                        Mod_Progress := False;
                     else
                        Declare_Data :
                        declare
                           Data_Idx : Crypto.Jobs_Index_Type;
                           SHA_Data : SHA256_4K.Data_Type;
                           SHA_Hash : SHA256_4K.Hash_Type;
                           CBE_Hash : Hash_Type;
                        begin
                           SHA256_4K_Data_From_CBE_Data (
                              SHA_Data, IO_Buf (Index));
                           SHA256_4K.Hash (SHA_Data, SHA_Hash);
                           CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
                           if CBE_Hash /=
                              Block_IO.Peek_Completed_Hash (Obj.IO_Obj, Prim)
                           then
                              raise Program_Error;
                           end if;

                           --
                           --  Having to override the Tag is needed because of
                           --  the way the Crypto module is hooked up in the
                           --  overall Data flow. Since it is the one that
                           --  acknowledges the primitive to the pool in the
                           --  read
                           --  case, we have to use the Tag the pool module
                           --  uses.
                           --
                           Crypto.Submit_Primitive (
                              Obj.Crypto_Obj,
                              Primitive.Copy_Valid_Object_New_Tag (
                                 Prim,
                                 Block_IO.Peek_Completed_Tag (
                                    Obj.IO_Obj, Prim)),
                              Block_IO.Peek_Completed_Key_ID (
                                 Obj.IO_Obj, Prim),
                              Data_Idx);

                           Crypto_Cipher_Buf (Data_Idx) := IO_Buf (Index);

                        end Declare_Data;
                     end if;

                  elsif Primitive.Has_Tag_Cache_Blk_IO (Prim) then

                     Declare_Slot_Idx :
                     declare
                        Slot_Idx : constant Cache.Slots_Index_Type :=
                           Cache.Slots_Index_Type (Primitive.Index (Prim));
                     begin
                        if Primitive.Operation (Prim) = Read then
                           Obj.Cache_Slots_Data (Slot_Idx) := IO_Buf (Index);
                        end if;
                        Cache.Mark_Generated_Primitive_Complete (
                           Obj.Cache_Obj, Slot_Idx, Primitive.Success (Prim));

                     end Declare_Slot_Idx;

                  elsif Primitive.Has_Tag_Write_Back (Prim) then
                     Write_Back.Mark_Completed_IO_Primitive (
                        Obj.Write_Back_Obj, Prim);

                  elsif Primitive.Has_Tag_SB_Ctrl_Blk_IO_Write_SB (Prim) then
                     Superblock_Control.Mark_Generated_Prim_Complete (
                        Obj.SB_Ctrl, Prim);

                  elsif Primitive.Has_Tag_SB_Ctrl_Blk_IO_Read_SB (Prim) then
                     Superblock_Control.Mark_Generated_Prim_Complete_Blk_Data (
                        Obj.SB_Ctrl, Prim, IO_Buf (Index));

                  elsif Primitive.Has_Tag_SB_Ctrl_Blk_IO_Sync (Prim) then
                     Superblock_Control.Mark_Generated_Prim_Complete (
                        Obj.SB_Ctrl, Prim);

                  elsif Primitive.Has_Tag_VBD_Rkg_Blk_IO (Prim) then

                     case Primitive.Operation (Prim) is
                     when Read =>

                        VBD_Rekeying.Mark_Generated_Prim_Completed_Blk_Data (
                           Obj.VBD_Rkg, Prim, IO_Buf (Index));

                     when Write | Sync =>

                        VBD_Rekeying.Mark_Generated_Prim_Completed (
                           Obj.VBD_Rkg, Prim);

                     end case;

                  else
                     raise Program_Error;
                  end if;
                  exit Loop_IO_Completed_Prims when not Mod_Progress;

               end Declare_Index_3;
               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            end case;

         end Declare_Prim_15;

      end loop Loop_IO_Completed_Prims;
   end Execute_IO;

   --
   --  Execute
   --
   procedure Execute (
      Obj               : in out Object_Type;
      IO_Buf            : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  : in out Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type)
   is
      Progress : Boolean := False;
   begin

      Execute_SCD (Obj, Progress);
      Execute_Request_Pool (Obj, Progress);
      Execute_SB_Ctrl (Obj, IO_Buf, Progress);
      Execute_TA (Obj, Progress);
      Execute_VBD_Rkg (
         Obj, IO_Buf, Crypto_Plain_Buf, Crypto_Cipher_Buf, Progress);
      Execute_FT_Rszg (Obj, Progress);
      Execute_MT_Rszg (Obj, Progress);
      Execute_VBD (Obj, Crypto_Plain_Buf, Progress);
      Execute_Cache  (Obj, IO_Buf, Progress);
      Execute_IO     (Obj, IO_Buf, Crypto_Cipher_Buf, Progress);
      Execute_Crypto (Obj, Crypto_Plain_Buf, Crypto_Cipher_Buf, Progress);
      Execute_Meta_Tree (Obj, Progress);
      Execute_Writeback (Obj, IO_Buf, Crypto_Plain_Buf, Progress);
      Execute_Free_Tree (Obj, Progress);

      Obj.Execute_Progress := Progress;
   end Execute;

   procedure Start_Waiting_For_Front_End (
      Obj   : in out Object_Type;
      Prim  :        Primitive.Object_Type;
      Event :        Event_Type)
   is
   begin
      Obj.Wait_For_Front_End := (
         Req         =>
            Request_Pool.Request_For_Index (
               Obj.Request_Pool_Obj,
               Pool_Idx_Slot_Content (Primitive.Pool_Idx_Slot (Prim))),
         Prim        => Prim,
         Event       => Event,
         In_Progress => False);
   end Start_Waiting_For_Front_End;

   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

   function To_String (WFE : Wait_For_Event_Type)
   return String
   is (
      "WFE (Req=" & Request.To_String (WFE.Req) &
      ", Prim="        & Primitive.To_String (WFE.Prim) &
      ", Event="       & To_String (WFE.Event) &
      ", In_Progress=" & Debug.To_String (WFE.In_Progress) & ")");

   function To_String (Obj : Object_Type)
   return String
   is (
      "CBE=(" &
      ", Wait_For_Front_End=" & To_String (Obj.Wait_For_Front_End) &
      ", VBD="                & Virtual_Block_Device.To_String (Obj.VBD) &
      ", Secure_Superblock="  & Debug.To_String (Obj.Secure_Superblock) &
      ")");

end CBE.Library;
