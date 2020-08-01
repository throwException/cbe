--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Primitive;

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
      Obj.Trans_Data       := (others => (others => 0));

      New_Free_Tree.Initialized_Object (Obj.New_Free_Tree_Obj);
      Meta_Tree.Initialized_Object (Obj.Meta_Tree_Obj);

      Obj.Superblock := Superblock_Invalid;
      Obj.Cur_Gen := Generation_Type'First;
      Obj.Cur_SB := Superblocks_Index_Type'First;

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

   --
   --  Client_Request_Acceptable
   --
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

   --
   --  Peek_Completed_Client_Request
   --
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

   --
   --  Drop_Completed_Client_Request
   --
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

   --
   --  IO_Request_In_Progress
   --
   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type)
   is
   begin
      Block_IO.Drop_Generated_Primitive_2 (Obj.IO_Obj, Data_Idx);
   end IO_Request_In_Progress;

   --
   --  IO_Request_Completed
   --
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
      Prim : constant Primitive.Object_Type :=
         Crypto.Peek_Generated_Client_Primitive (Obj.Crypto_Obj);
   begin
      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Crypto_IO_Client_Supply_Data =>

            Req := Crypto.Peek_Generated_Req (Obj.Crypto_Obj, Prim);
            VBA := Crypto.Peek_Generated_VBA (Obj.Crypto_Obj, Prim);
            Plain_Buf_Idx :=
               Crypto.Peek_Generated_Plain_Buf_Idx (Obj.Crypto_Obj, Prim);
            return;

         when others => null;
         end case;

      end if;
      Req := Request.Invalid_Object;
      VBA := Virtual_Block_Address_Type'First;
      Plain_Buf_Idx := Crypto.Plain_Buffer_Index_Type'First;

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
   --  Client_Transfer_Write_Data_Required
   --
   procedure Client_Transfer_Write_Data_Required (
      Obj           :     Object_Type;
      Req           : out Request.Object_Type;
      VBA           : out Virtual_Block_Address_Type;
      Plain_Buf_Idx : out Crypto.Plain_Buffer_Index_Type)
   is
      Prim : constant Primitive.Object_Type :=
         Crypto.Peek_Generated_Client_Primitive (Obj.Crypto_Obj);
   begin
      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Crypto_IO_Client_Obtain_Data =>

            Req := Crypto.Peek_Generated_Req (Obj.Crypto_Obj, Prim);
            VBA := Crypto.Peek_Generated_VBA (Obj.Crypto_Obj, Prim);
            Plain_Buf_Idx :=
               Crypto.Peek_Generated_Plain_Buf_Idx (Obj.Crypto_Obj, Prim);
            return;

         when others => null;
         end case;

      end if;
      Req := Request.Invalid_Object;
      VBA := Virtual_Block_Address_Type'First;
      Plain_Buf_Idx := Crypto.Plain_Buffer_Index_Type'First;

   end Client_Transfer_Write_Data_Required;

   --
   --  Client_Transfer_Write_Data_In_Progress
   --
   procedure Client_Transfer_Write_Data_In_Progress (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive_New (
         Obj.Crypto_Obj, Crypto.Jobs_Index_Type (Plain_Buf_Idx));

   end Client_Transfer_Write_Data_In_Progress;

   --
   --  Client_Transfer_Write_Data_Completed
   --
   procedure Client_Transfer_Write_Data_Completed (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type;
      Success       :        Boolean)
   is
   begin
      Crypto.Mark_Generated_Primitive_Complete (
         Obj.Crypto_Obj, Plain_Buf_Idx, Success);

   end Client_Transfer_Write_Data_Completed;

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
      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_SB_Ctrl_Crypto_Remove_Key =>

            Key_ID := Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx);
            Req := Request.Valid_Object (
               Op     => Read,
               Succ   => False,
               Blk_Nr => 0,
               Off    => 0,
               Cnt    => 1,
               Key    => 0,
               Tg     => Request.Tag_Type (Idx));
            return;

         when others => null;
         end case;

      end if;
      Key_ID := Key_ID_Invalid;
      Req := Request.Invalid_Object;

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
      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_SB_Ctrl_Crypto_Add_Key =>

            Key := Crypto.Peek_Generated_Key (Obj.Crypto_Obj, Idx);
            Req := Request.Valid_Object (
               Op     => Read,
               Succ   => False,
               Blk_Nr => 0,
               Off    => 0,
               Cnt    => 1,
               Key    => 0,
               Tg     => Request.Tag_Type (Idx));
            return;

         when others => null;
         end case;

      end if;
      Key := Key_Plaintext_Invalid;
      Req := Request.Invalid_Object;

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

   --
   --  Crypto_Cipher_Data_Required
   --
   procedure Crypto_Cipher_Data_Required (
      Obj        :     Object_Type;
      Req        : out Request.Object_Type;
      Data_Index : out Crypto.Plain_Buffer_Index_Type)
   is
      Prim : constant Primitive.Object_Type :=
         Crypto.Peek_Generated_Crypto_Dev_Primitive (Obj.Crypto_Obj);
   begin

      if Primitive.Valid (Prim) then

         case Primitive.Tag (Prim) is
         when Primitive.Tag_Crypto_IO_Crypto_Dev_Encrypt =>

            Data_Index :=
               Crypto.Peek_Generated_Plain_Buf_Idx (Obj.Crypto_Obj, Prim);

            Req := Request.Valid_Object (
               Op     => Write,
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
               Idx    : Crypto.Jobs_Index_Type;
               Prim_1 : Primitive.Object_Type;
            begin

               Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Idx, Prim_1);
               Data_Index := Crypto.Plain_Buffer_Index_Type (Idx);

               if Primitive.Valid (Prim_1) and then
                  Primitive.Operation (Prim_1) = Write
               then

                  case Primitive.Tag (Prim_1) is
                  when
                     Primitive.Tag_SB_Ctrl_Crypto_Add_Key |
                     Primitive.Tag_SB_Ctrl_Crypto_Remove_Key |
                     Primitive.Tag_Crypto_IO_Crypto_Dev_Decrypt |
                     Primitive.Tag_Crypto_IO_Crypto_Dev_Encrypt
                  =>

                     Req := Request.Invalid_Object;
                     return;

                  when others =>

                     Req := Request.Valid_Object (
                        Op     => Write,
                        Succ   => False,
                        Blk_Nr => Primitive.Block_Number (Prim_1),
                        Off    => 0,
                        Cnt    => 1,
                        Key    =>
                           Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx),
                        Tg     => 0);

                     return;

                  end case;

               else

                  Req := Request.Invalid_Object;
                  return;

               end if;

            end Declare_Idx;

         end case;

      else

         Data_Index := Crypto.Plain_Buffer_Index_Type'First;
         Req := Request.Invalid_Object;

      end if;

   end Crypto_Cipher_Data_Required;

   --
   --  Crypto_Cipher_Data_Requested
   --
   procedure Crypto_Cipher_Data_Requested (
      Obj           : in out Library.Object_Type;
      Plain_Buf_Idx :        Crypto.Plain_Buffer_Index_Type)
   is
   begin
      Crypto.Drop_Generated_Primitive_New (Obj.Crypto_Obj,
         Crypto.Jobs_Index_Type (Plain_Buf_Idx));

   end Crypto_Cipher_Data_Requested;

   --
   --  Supply_Crypto_Cipher_Data
   --
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
               Op     => Read,
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
               Idx    : Crypto.Jobs_Index_Type;
               Prim_1 : Primitive.Object_Type;
            begin

               Crypto.Peek_Generated_Primitive (Obj.Crypto_Obj,  Idx, Prim_1);
               Data_Index := Crypto.Cipher_Buffer_Index_Type (Idx);

               if Primitive.Valid (Prim_1) and then
                  Primitive.Operation (Prim_1) = Read
               then

                  case Primitive.Tag (Prim_1) is
                  when
                     Primitive.Tag_SB_Ctrl_Crypto_Add_Key |
                     Primitive.Tag_SB_Ctrl_Crypto_Remove_Key |
                     Primitive.Tag_Crypto_IO_Crypto_Dev_Decrypt |
                     Primitive.Tag_Crypto_IO_Crypto_Dev_Encrypt
                  =>

                     Req := Request.Invalid_Object;
                     return;

                  when others =>

                     Req := Request.Valid_Object (
                        Op     => Read,
                        Succ   => False,
                        Blk_Nr => Primitive.Block_Number (Prim_1),
                        Off    => 0,
                        Cnt    => 1,
                        Key    =>
                           Crypto.Peek_Generated_Key_ID (Obj.Crypto_Obj, Idx),
                        Tg     => 0);

                     return;

                  end case;

               else

                  Req := Request.Invalid_Object;
                  return;

               end if;

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

   --
   --  Peek_Generated_TA_Request
   --
   procedure Peek_Generated_TA_Request (
      Obj :     Object_Type;
      Req : out TA_Request.Object_Type)
   is
   begin
      Trust_Anchor.Peek_Generated_Request (Obj.TA, Req);
   end Peek_Generated_TA_Request;

   --
   --  Drop_Generated_TA_Request
   --
   procedure Drop_Generated_TA_Request (
      Obj : in out Object_Type;
      Req :        TA_Request.Object_Type)
   is
   begin
      Trust_Anchor.Drop_Generated_Request (Obj.TA, Req);
   end Drop_Generated_TA_Request;

   --
   --  Peek_Generated_TA_SB_Hash
   --
   procedure Peek_Generated_TA_SB_Hash (
      Obj  :     Object_Type;
      Req  :     TA_Request.Object_Type;
      Hash : out Hash_Type)
   is
   begin
      Trust_Anchor.Peek_Generated_SB_Hash (Obj.TA, Req, Hash);
   end Peek_Generated_TA_SB_Hash;

   --
   --  Peek_Generated_TA_Key_Cipher
   --
   procedure Peek_Generated_TA_Key_Cipher (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Ciphertext_Type)
   is
   begin
      Trust_Anchor.Peek_Generated_Key_Value_Ciphertext (
         Obj.TA, Req, Key_Value);
   end Peek_Generated_TA_Key_Cipher;

   --
   --  Peek_Generated_TA_Key_Plain
   --
   procedure Peek_Generated_TA_Key_Plain (
      Obj       :     Object_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Plaintext_Type)
   is
   begin
      Trust_Anchor.Peek_Generated_Key_Value_Plaintext (
         Obj.TA, Req, Key_Value);
   end Peek_Generated_TA_Key_Plain;

   --
   --  Mark_Generated_TA_Create_Key_Request_Complete
   --
   procedure Mark_Generated_TA_Create_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
   begin
      Trust_Anchor.Mark_Generated_Create_Key_Request_Complete (
         Obj.TA, Req, Key_Value);
   end Mark_Generated_TA_Create_Key_Request_Complete;

   --
   --  Mark_Generated_TA_Secure_SB_Request_Complete
   --
   procedure Mark_Generated_TA_Secure_SB_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type)
   is
   begin
      Trust_Anchor.Mark_Generated_Secure_SB_Request_Complete (
         Obj.TA, Req);
   end Mark_Generated_TA_Secure_SB_Request_Complete;

   --
   --  Mark_Generated_TA_Decrypt_Key_Request_Complete
   --
   procedure Mark_Generated_TA_Decrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
   begin
      Trust_Anchor.Mark_Generated_Decrypt_Key_Request_Complete (
         Obj.TA, Req, Key_Value);
   end Mark_Generated_TA_Decrypt_Key_Request_Complete;

   procedure Mark_Generated_TA_Encrypt_Key_Request_Complete (
      Obj       : in out Object_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type)
   is
   begin
      Trust_Anchor.Mark_Generated_Encrypt_Key_Request_Complete (
         Obj.TA, Req, Key_Value);
   end Mark_Generated_TA_Encrypt_Key_Request_Complete;

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

   --
   --  Execute_Free_Tree
   --
   procedure Execute_Free_Tree (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
      Prim : Primitive.Object_Type;
      Job_Idx : Cache.Jobs_Index_Type;
   begin

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
               not Primitive.Valid (Prim) or else
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

   --
   --  Execute_Meta_Tree
   --
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
               end;

               New_Free_Tree.Mark_Generated_Meta_Tree_Primitive_Complete (
                  Obj.New_Free_Tree_Obj,
                  Prim,
                  Meta_Tree.Peek_Completed_New_PBA (Obj.Meta_Tree_Obj, Prim));

               Meta_Tree.Drop_Completed_Primitive (Obj.Meta_Tree_Obj, Prim);
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

            when
               Primitive.Tag_Pool_SB_Ctrl_Read_VBA |
               Primitive.Tag_Pool_SB_Ctrl_Write_VBA
            =>

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

               case Primitive.Tag (Prim) is
               when Primitive.Tag_VBD_Rkg_Blk_IO =>

                  Declare_Blk_IO_Buf_Idx :
                  declare
                     Idx : Block_IO.Data_Index_Type;
                  begin

                     Block_IO.Submit_Primitive (
                        Obj.IO_Obj, Primitive.Tag_VBD_Rkg_Blk_IO, Prim, Idx);

                     Blk_IO_Buf (Idx) :=
                        VBD_Rekeying.Peek_Generated_Blk_Data (
                           Obj.VBD_Rkg, Prim);

                  end Declare_Blk_IO_Buf_Idx;

                  VBD_Rekeying.Drop_Generated_Primitive (Obj.VBD_Rkg, Prim);
                  Progress := True;

               when Primitive.Tag_VBD_Rkg_Blk_IO_Write_Client_Data =>

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

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Write_VBA =>

               Superblock_Control.Mark_Generated_Prim_Complete_Snap (
                  Obj.SB_Ctrl,
                  Prim,
                  VBD_Rekeying.Peek_Completed_Snap (Obj.VBD_Rkg, Prim));

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

            when Primitive.Tag_SB_Ctrl_VBD_Rkg_Write_VBA =>

               VBD_Rekeying.Submit_Primitive_Write_VBA (
                  Obj.VBD_Rkg, Prim, Obj.Cur_Gen,
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
               Primitive.Tag_Pool_SB_Ctrl_Write_VBA |
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

   --
   --  Execute_Crypto
   --
   procedure Execute_Crypto (
      Obj               : in out Object_Type;
      Blk_IO_Buf        : in out Block_IO.Data_Type;
      Crypto_Plain_Buf  :        Crypto.Plain_Buffer_Type;
      Crypto_Cipher_Buf :        Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean)
   is
   begin
      Crypto.Execute (Obj.Crypto_Obj, Progress);

      Loop_Crypto_Completed_Prims :
      loop
         Declare_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Crypto.Peek_Completed_Primitive (Obj.Crypto_Obj);
         begin
            exit Loop_Crypto_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_SB_Ctrl_Crypto_Add_Key |
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

            when Primitive.Tag_Blk_IO_Crypto_Obtain_And_Encrypt_Client_Data =>

               Blk_IO_Buf (
                  Crypto.Peek_Completed_Blk_IO_Data_Idx (
                     Obj.Crypto_Obj, Prim))
               :=
                  Crypto_Cipher_Buf (
                     Crypto.Peek_Completed_Cipher_Buf_Idx (
                        Obj.Crypto_Obj, Prim));

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

               raise Program_Error;

            end case;

         end Declare_Prim;

      end loop Loop_Crypto_Completed_Prims;

   end Execute_Crypto;

   --
   --  Execute_Blk_IO
   --
   procedure Execute_Blk_IO (
      Obj               : in out Object_Type;
      IO_Buf            :        Block_IO.Data_Type;
      Crypto_Cipher_Buf : in out Crypto.Cipher_Buffer_Type;
      Progress          : in out Boolean)
   is
   begin
      Block_IO.Execute (Obj.IO_Obj, IO_Buf, Progress);

      Loop_Generated_Crypto_Prims :
      loop

         Declare_Generated_Crypto_Prim :
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

                  Crypto.Submit_Primitive_Decrypt_Client_Data (
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

            when Primitive.Tag_Blk_IO_Crypto_Obtain_And_Encrypt_Client_Data =>

               Crypto.Submit_Primitive_Encrypt_Client_Data (
                  Obj.Crypto_Obj,
                  Prim,
                  Block_IO.Peek_Generated_Req (Obj.IO_Obj, Prim),
                  Block_IO.Peek_Generated_VBA (Obj.IO_Obj, Prim),
                  Block_IO.Peek_Generated_Key_ID (Obj.IO_Obj, Prim),
                  Block_IO.Peek_Generated_Data_Index (Obj.IO_Obj, Prim));

               Block_IO.Drop_Generated_Primitive_New (Obj.IO_Obj, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Generated_Crypto_Prim;

      end loop Loop_Generated_Crypto_Prims;

      Loop_Completed_Prims :
      loop

         Declare_Completed_Prim :
         declare
            Prim : constant Primitive.Object_Type :=
               Block_IO.Peek_Completed_Primitive (Obj.IO_Obj);
         begin
            exit Loop_Completed_Prims when not Primitive.Valid (Prim);

            case Primitive.Tag (Prim) is
            when Primitive.Tag_VBD_Rkg_Blk_IO_Read_Client_Data =>

               VBD_Rekeying.Mark_Generated_Prim_Completed (
                  Obj.VBD_Rkg, Prim);

               Block_IO.Drop_Completed_Primitive_New (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_VBD_Rkg_Blk_IO_Write_Client_Data =>

               VBD_Rekeying.Mark_Generated_Prim_Completed_Hash (
                  Obj.VBD_Rkg,
                  Prim,
                  Block_IO.Peek_Completed_Hash_New (Obj.IO_Obj, Prim));

               Block_IO.Drop_Completed_Primitive_New (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_Cache_Blk_IO =>

               Declare_Slot_Idx :
               declare
                  Slot_Idx : constant Cache.Slots_Index_Type :=
                     Cache.Slots_Index_Type (Primitive.Index (Prim));

                  Data_Idx : constant Block_IO.Data_Index_Type :=
                     Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj);
               begin

                  if Primitive.Operation (Prim) = Read then
                     Obj.Cache_Slots_Data (Slot_Idx) := IO_Buf (Data_Idx);
                  end if;

                  Cache.Mark_Generated_Primitive_Complete (
                     Obj.Cache_Obj, Slot_Idx, Primitive.Success (Prim));

               end Declare_Slot_Idx;

               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_Blk_IO_Write_SB =>

               Superblock_Control.Mark_Generated_Prim_Complete (
                  Obj.SB_Ctrl, Prim);

               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_Blk_IO_Read_SB =>

               Superblock_Control.Mark_Generated_Prim_Complete_Blk_Data (
                  Obj.SB_Ctrl, Prim,
                  IO_Buf (Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj)));

               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_SB_Ctrl_Blk_IO_Sync =>

               Superblock_Control.Mark_Generated_Prim_Complete (
                  Obj.SB_Ctrl, Prim);

               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            when Primitive.Tag_VBD_Rkg_Blk_IO =>

               case Primitive.Operation (Prim) is
               when Read =>

                  VBD_Rekeying.Mark_Generated_Prim_Completed_Blk_Data (
                     Obj.VBD_Rkg, Prim,
                     IO_Buf (Block_IO.Peek_Completed_Data_Index (Obj.IO_Obj)));

               when Write | Sync =>

                  VBD_Rekeying.Mark_Generated_Prim_Completed (
                     Obj.VBD_Rkg, Prim);

               end case;

               Block_IO.Drop_Completed_Primitive (Obj.IO_Obj, Prim);
               Progress := True;

            when others =>

               raise Program_Error;

            end case;

         end Declare_Completed_Prim;

      end loop Loop_Completed_Prims;

   end Execute_Blk_IO;

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

      Execute_Request_Pool (Obj, Progress);
      Execute_SB_Ctrl (Obj, IO_Buf, Progress);
      Execute_TA (Obj, Progress);
      Execute_VBD_Rkg (
         Obj, IO_Buf, Crypto_Plain_Buf, Crypto_Cipher_Buf, Progress);
      Execute_FT_Rszg (Obj, Progress);
      Execute_MT_Rszg (Obj, Progress);
      Execute_Cache (Obj, IO_Buf, Progress);
      Execute_Blk_IO (Obj, IO_Buf, Crypto_Cipher_Buf, Progress);
      Execute_Crypto (
         Obj, IO_Buf, Crypto_Plain_Buf, Crypto_Cipher_Buf, Progress);
      Execute_Meta_Tree (Obj, Progress);
      Execute_Free_Tree (Obj, Progress);

      Obj.Execute_Progress := Progress;
   end Execute;

   --
   --  Execute_Progress
   --
   function Execute_Progress (Obj : Object_Type)
   return Boolean
   is (Obj.Execute_Progress);

end CBE.Library;
