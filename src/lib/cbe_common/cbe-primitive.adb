--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;

package body CBE.Primitive
with SPARK_Mode
is
   function Copy_Valid_Object_New_Tag (
      Obj : Object_Type;
      Tag : Tag_Type)
   return Object_Type
   is
   begin
      if Tag = Tag_Invalid then
         raise Program_Error;
      end if;
      return (
         Valid         => True,
         Operation     => Obj.Operation,
         Success       => Obj.Success,
         Tag           => Tag,
         Pool_Idx_Slot => Obj.Pool_Idx_Slot,
         Block_Number  => Obj.Block_Number,
         Index         => Obj.Index);
   end Copy_Valid_Object_New_Tag;

   function Copy_Valid_Object_New_Succ_Blk_Nr (
      Obj    : Object_Type;
      Succ   : Request.Success_Type;
      Blk_Nr : Block_Number_Type)
   return Object_Type
   is
   begin
      if Obj.Tag = Tag_Invalid then
         raise Program_Error;
      end if;
      return (
         Valid         => True,
         Operation     => Obj.Operation,
         Success       => Succ,
         Tag           => Obj.Tag,
         Pool_Idx_Slot => Obj.Pool_Idx_Slot,
         Block_Number  => Blk_Nr,
         Index         => Obj.Index);
   end Copy_Valid_Object_New_Succ_Blk_Nr;

   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   is (
      Valid         => False,
      Operation     => Read,
      Success       => False,
      Tag           => Tag_Invalid,
      Pool_Idx_Slot => Pool_Idx_Slot_Invalid,
      Block_Number  => 0,
      Index         => 0);

   --
   --  Valid_Object
   --
   function Valid_Object (
      Op     : Primitive_Operation_Type;
      Succ   : Request.Success_Type;
      Tg     : Tag_Type;
      Pl_Idx : Pool_Index_Type;
      Blk_Nr : Block_Number_Type;
      Idx    : Index_Type)
   return Object_Type
   is
   begin
      if Tg = Tag_Invalid then
         raise Program_Error;
      end if;
      return (
         Valid         => True,
         Operation     => Op,
         Success       => Succ,
         Tag           => Tg,
         Pool_Idx_Slot => Pool_Idx_Slot_Valid (Pl_Idx),
         Block_Number  => Blk_Nr,
         Index         => Idx);
   end Valid_Object;

   --
   --  Valid_Object_No_Pool_Idx
   --
   function Valid_Object_No_Pool_Idx (
      Op     : Primitive_Operation_Type;
      Succ   : Request.Success_Type;
      Tg     : Tag_Type;
      Blk_Nr : Block_Number_Type;
      Idx    : Index_Type)
   return Object_Type
   is
   begin
      if Tg = Tag_Invalid then
         raise Program_Error;
      end if;
      return (
         Valid         => True,
         Operation     => Op,
         Success       => Succ,
         Tag           => Tg,
         Pool_Idx_Slot => Pool_Idx_Slot_Invalid,
         Block_Number  => Blk_Nr,
         Index         => Idx);
   end Valid_Object_No_Pool_Idx;

   --
   --  Equal
   --
   --  FIXME When using a pool index range with size > 1 we have to check the
   --  pool index too, we cannot check it with range size = 1 because the
   --  compiler complains that it always renders true.
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   is (
      Obj_1.Block_Number  = Obj_2.Block_Number  and then
      Obj_1.Index         = Obj_2.Index         and then
      Obj_1.Tag           = Obj_2.Tag           and then
      Obj_1.Pool_Idx_Slot = Obj_2.Pool_Idx_Slot and then
      Obj_1.Operation     = Obj_2.Operation);

   function Has_Tag_Lib_SB_Init (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Lib_SB_Init);

   function Has_Tag_SB_Init_VBD_Init (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Init_VBD_Init);

   function Has_Tag_SB_Init_FT_Init (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Init_FT_Init);

   function Has_Tag_SB_Init_MT_Init (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Init_MT_Init);

   function Has_Tag_SB_Init_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Init_Blk_IO);

   function Has_Tag_VBD_Init_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Init_Blk_Alloc);

   function Has_Tag_VBD_Init_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Init_Blk_IO);

   function Has_Tag_FT_Init_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Init_Blk_Alloc);

   function Has_Tag_FT_Init_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Init_Blk_IO);

   function Has_Tag_Lib_SB_Check (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Lib_SB_Check);

   function Has_Tag_SB_Check_VBD_Check (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Check_VBD_Check);

   function Has_Tag_SB_Check_FT_Check (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Check_FT_Check);

   function Has_Tag_SB_Check_MT_Check (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Check_MT_Check);

   function Has_Tag_SB_Check_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Check_Blk_IO);

   function Has_Tag_VBD_Check_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Check_Blk_Alloc);

   function Has_Tag_VBD_Check_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Check_Blk_IO);

   function Has_Tag_FT_Check_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Check_Blk_Alloc);

   function Has_Tag_FT_Check_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Check_Blk_IO);

   function Has_Tag_Lib_SB_Dump (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Lib_SB_Dump);

   function Has_Tag_SB_Dump_VBD_Dump (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Dump_VBD_Dump);

   function Has_Tag_SB_Dump_FT_Dump (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Dump_FT_Dump);

   function Has_Tag_SB_Dump_MT_Dump (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Dump_MT_Dump);

   function Has_Tag_SB_Dump_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Dump_Blk_IO);

   function Has_Tag_VBD_Dump_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Dump_Blk_Alloc);

   function Has_Tag_VBD_Dump_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Dump_Blk_IO);

   function Has_Tag_FT_Dump_Blk_Alloc (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Dump_Blk_Alloc);

   function Has_Tag_FT_Dump_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Dump_Blk_IO);

   function Has_Tag_Pool_Discard_Snap (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_Discard_Snap);

   function Has_Tag_Pool_Create_Snap (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_Create_Snap);

   function Has_Tag_Pool_Sync (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_Sync);

   function Has_Tag_Pool_VBD (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_VBD);

   function Has_Tag_Pool_SB_Ctrl_Init_Rekey (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_SB_Ctrl_Init_Rekey);

   function Has_Tag_Pool_SB_Ctrl_Rekey_VBA (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Pool_SB_Ctrl_Rekey_VBA);

   function Has_Tag_Pool_SB_Ctrl_VBD_Ext_Step (Obj : Object_Type)
   return Boolean
   is (Obj.Tag = Tag_Pool_SB_Ctrl_VBD_Ext_Step);

   function Has_Tag_SB_Ctrl_TA_Create_Key (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_TA_Create_Key);

   function Has_Tag_SB_Ctrl_TA_Secure_SB (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_TA_Secure_SB);

   function Has_Tag_SB_Ctrl_TA_Encrypt_Key (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_TA_Encrypt_Key);

   function Has_Tag_SB_Ctrl_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_Cache);

   function Has_Tag_SB_Ctrl_VBD_Rkg (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_VBD_Rkg);

   function Has_Tag_SB_Ctrl_Crypto_Add_Key (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_Crypto_Add_Key);

   function Has_Tag_SB_Ctrl_Blk_IO_Write_SB (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_Blk_IO_Write_SB);

   function Has_Tag_SB_Ctrl_Blk_IO_Sync (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SB_Ctrl_Blk_IO_Sync);

   function Has_Tag_VBD_Rkg_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_Cache);

   function Has_Tag_VBD_Rkg_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_Blk_IO);

   function Has_Tag_VBD_Rkg_Crypto_Encrypt (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_Crypto_Encrypt);

   function Has_Tag_VBD_Rkg_Crypto_Decrypt (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_Crypto_Decrypt);

   function Has_Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks (Obj : Object_Type)
   return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks);

   function Has_Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks (Obj : Object_Type)
   return Boolean
   is (Obj.Tag = Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks);

   function Has_Tag_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_IO);

   function Has_Tag_Translation (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Translation);

   function Has_Tag_Write_Back (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Write_Back);

   function Has_Tag_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Cache);

   function Has_Tag_Cache_Flush (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Cache_Flush);

   function Has_Tag_Cache_Blk_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Cache_Blk_IO);

   function Has_Tag_Lib_Cache_Sync (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Lib_Cache_Sync);

   function Has_Tag_Decrypt (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Decrypt);

   function Has_Tag_Encrypt (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Encrypt);

   function Has_Tag_Sync_SB_Cache_Flush (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Sync_SB_Cache_Flush);

   function Has_Tag_Sync_SB_Write_SB (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Sync_SB_Write_SB);

   function Has_Tag_Sync_SB_Sync (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Sync_SB_Sync);

   function Has_Tag_VBD_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_VBD_Cache);

   function Has_Tag_FT_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_Cache);

   function Has_Tag_WB_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_WB_Cache);

   function Has_Tag_SCD_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_SCD_Cache);

   function Has_Tag_Free_Tree_IO (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Free_Tree_IO);

   function Has_Tag_Free_Tree_WB (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_Free_Tree_WB);

   function Has_Tag_FT_MT (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_FT_MT);

   function Has_Tag_MT_Cache (Obj : Object_Type) return Boolean
   is (Obj.Tag = Tag_MT_Cache);

   ----------------------
   --  Read Accessors  --
   ----------------------

   function Valid (Obj : Object_Type) return Boolean
   is (Obj.Valid);

   function Operation (Obj : Object_Type) return Primitive_Operation_Type
   is (Obj.Operation);

   function Success (Obj : Object_Type) return Request.Success_Type
   is (Obj.Success);

   function Tag (Obj : Object_Type) return Tag_Type
   is (Obj.Tag);

   function Pool_Idx_Slot (Obj : Object_Type) return Pool_Index_Slot_Type
   is (Obj.Pool_Idx_Slot);

   function Block_Number (Obj : Object_Type) return Block_Number_Type
   is (Obj.Block_Number);

   function Index (Obj : Object_Type) return Index_Type
   is (Obj.Index);

   -----------------------
   --  Write Accessors  --
   -----------------------

   procedure Success (Obj : in out Object_Type; Value : Request.Success_Type)
   is
   begin Obj.Success := Value; end Success;

   procedure Block_Number (Obj : in out Object_Type; Value : Block_Number_Type)
   is
   begin Obj.Block_Number := Value; end Block_Number;

   procedure Operation (
      Obj : in out Object_Type;
      Value : Primitive_Operation_Type)
   is
   begin Obj.Operation := Value; end Operation;

   function To_String (Obj : Object_Type)
   return String
   is (
      if not Obj.Valid then
         "Invalid Primitive"
      else
         "Primitive (Op=" &
         Obj.Operation'Image &
         ", Tag=" &
         To_String (Obj.Tag) &
         ", Pool_Idx=" &
         To_String (Obj.Pool_Idx_Slot) &
         ", Success=" &
         Debug.To_String (Obj.Success) &
         ", Block_Number=" &
         Debug.To_String (Debug.Uint64_Type (Obj.Block_Number)) &
         ", Index=" &
         Debug.To_String (Debug.Uint64_Type (Obj.Index)) &
         ")");

end CBE.Primitive;
