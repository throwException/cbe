--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;

package CBE.Primitive
with SPARK_Mode
is
   pragma Pure;

   type Tag_Type is (
      Tag_Invalid,
      Tag_Lib_SB_Init,
      Tag_SB_Init_VBD_Init,
      Tag_SB_Init_FT_Init,
      Tag_SB_Init_MT_Init,
      Tag_SB_Init_Blk_IO,
      Tag_SB_Init_TA_Create_Key,
      Tag_SB_Init_TA_Encrypt_Key,
      Tag_VBD_Init_Blk_Alloc,
      Tag_VBD_Init_Blk_IO,
      Tag_FT_Init_Blk_Alloc,
      Tag_FT_Init_Blk_IO,
      Tag_Lib_SB_Check,
      Tag_SB_Check_VBD_Check,
      Tag_SB_Check_FT_Check,
      Tag_SB_Check_MT_Check,
      Tag_SB_Check_Blk_IO,
      Tag_VBD_Check_Blk_Alloc,
      Tag_VBD_Check_Blk_IO,
      Tag_FT_Check_Blk_Alloc,
      Tag_FT_Check_Blk_IO,
      Tag_Lib_SB_Dump,
      Tag_SB_Dump_VBD_Dump,
      Tag_SB_Dump_FT_Dump,
      Tag_SB_Dump_MT_Dump,
      Tag_SB_Dump_Blk_IO,
      Tag_VBD_Dump_Blk_Alloc,
      Tag_VBD_Dump_Blk_IO,
      Tag_FT_Dump_Blk_Alloc,
      Tag_FT_Dump_Blk_IO,
      Tag_Pool_Discard_Snap,
      Tag_Pool_Create_Snap,
      Tag_Pool_Sync,
      Tag_Pool_VBD,
      Tag_Pool_SB_Ctrl_Init_Rekey,
      Tag_Pool_SB_Ctrl_Rekey_VBA,
      Tag_Pool_SB_Ctrl_VBD_Ext_Step,
      Tag_Pool_SB_Ctrl_FT_Ext_Step,
      Tag_Pool_SB_Ctrl_Create_Snap,
      Tag_Pool_SB_Ctrl_Discard_Snap,
      Tag_Pool_SB_Ctrl_Initialize,
      Tag_Pool_SB_Ctrl_Deinitialize,
      Tag_SB_Ctrl_TA_Create_Key,
      Tag_SB_Ctrl_TA_Secure_SB,
      Tag_SB_Ctrl_TA_Encrypt_Key,
      Tag_SB_Ctrl_TA_Decrypt_Key,
      Tag_SB_Ctrl_Cache,
      Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA,
      Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step,
      Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step,
      Tag_SB_Ctrl_Crypto_Add_Key,
      Tag_SB_Ctrl_Crypto_Remove_Key,
      Tag_SB_Ctrl_Blk_IO_Write_SB,
      Tag_SB_Ctrl_Blk_IO_Read_SB,
      Tag_SB_Ctrl_Blk_IO_Sync,
      Tag_VBD_Rkg_Cache,
      Tag_FT_Rszg_Cache,
      Tag_FT_Rszg_MT_Alloc,
      Tag_VBD_Rkg_Blk_IO,
      Tag_VBD_Rkg_Crypto_Encrypt,
      Tag_VBD_Rkg_Crypto_Decrypt,
      Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg,
      Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks,
      Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks,
      Tag_IO,
      Tag_Translation,
      Tag_Write_Back,
      Tag_Cache,
      Tag_Cache_Flush,
      Tag_Cache_Blk_IO,
      Tag_Lib_Cache_Sync,
      Tag_Decrypt,
      Tag_Encrypt,
      Tag_Sync_SB_Cache_Flush,
      Tag_Sync_SB_TA_Encrypt_Key,
      Tag_Sync_SB_Write_SB,
      Tag_Sync_SB_Sync,
      Tag_VBD_Cache,
      Tag_FT_Cache,
      Tag_WB_Cache,
      Tag_SCD_Cache,
      Tag_Free_Tree_Query,
      Tag_Free_Tree_IO,
      Tag_Free_Tree_WB,
      Tag_FT_MT,
      Tag_MT_Cache);

   function To_String (Tag : Tag_Type)
   return String
   is (
      case Tag is
      when Tag_Invalid => "Invalid",
      when Tag_Lib_SB_Init => "Lib_SB_Init",
      when Tag_SB_Init_VBD_Init => "SB_Init_VBD_Init",
      when Tag_SB_Init_FT_Init => "SB_Init_FT_Init",
      when Tag_SB_Init_MT_Init => "SB_Init_MT_Init",
      when Tag_SB_Init_Blk_IO => "SB_Init_Blk_IO",
      when Tag_SB_Init_TA_Create_Key => "SB_Init_TA_Create_Key",
      when Tag_SB_Init_TA_Encrypt_Key => "SB_Init_TA_Encrypt_Key",
      when Tag_VBD_Init_Blk_Alloc => "VBD_Init_Blk_Alloc",
      when Tag_VBD_Init_Blk_IO => "VBD_Init_Blk_IO",
      when Tag_FT_Init_Blk_Alloc => "FT_Init_Blk_Alloc",
      when Tag_FT_Init_Blk_IO => "FT_Init_Blk_IO",
      when Tag_Lib_SB_Check => "Lib_SB_Check",
      when Tag_SB_Check_VBD_Check => "SB_Check_VBD_Check",
      when Tag_SB_Check_FT_Check => "SB_Check_FT_Check",
      when Tag_SB_Check_MT_Check => "SB_Check_MT_Check",
      when Tag_SB_Check_Blk_IO => "SB_Check_Blk_IO",
      when Tag_VBD_Check_Blk_Alloc => "VBD_Check_Blk_Alloc",
      when Tag_VBD_Check_Blk_IO => "VBD_Check_Blk_IO",
      when Tag_FT_Check_Blk_Alloc => "FT_Check_Blk_Alloc",
      when Tag_FT_Check_Blk_IO => "FT_Check_Blk_IO",
      when Tag_Lib_SB_Dump => "Lib_SB_Dump",
      when Tag_SB_Dump_VBD_Dump => "SB_Dump_VBD_Dump",
      when Tag_SB_Dump_FT_Dump => "SB_Dump_FT_Dump",
      when Tag_SB_Dump_MT_Dump => "SB_Dump_MT_Dump",
      when Tag_SB_Dump_Blk_IO => "SB_Dump_Blk_IO",
      when Tag_VBD_Dump_Blk_Alloc => "VBD_Dump_Blk_Alloc",
      when Tag_VBD_Dump_Blk_IO => "VBD_Dump_Blk_IO",
      when Tag_FT_Dump_Blk_Alloc => "FT_Dump_Blk_Alloc",
      when Tag_FT_Dump_Blk_IO => "FT_Dump_Blk_IO",
      when Tag_Pool_Discard_Snap => "Pool_Discard_Snap",
      when Tag_Pool_Create_Snap => "Pool_Create_Snap",
      when Tag_Pool_Sync => "Pool_Sync",
      when Tag_Pool_VBD => "Pool_VBD",
      when Tag_Pool_SB_Ctrl_Init_Rekey => "Pool_SB_Ctrl_Init_Rekey",
      when Tag_Pool_SB_Ctrl_Rekey_VBA => "Pool_SB_Ctrl_Rekey_VBA",
      when Tag_Pool_SB_Ctrl_VBD_Ext_Step => "Pool_SB_Ctrl_VBD_Ext_Step",
      when Tag_Pool_SB_Ctrl_FT_Ext_Step => "Pool_SB_Ctrl_FT_Ext_Step",
      when Tag_Pool_SB_Ctrl_Create_Snap => "Pool_SB_Ctrl_Create_Snap",
      when Tag_Pool_SB_Ctrl_Discard_Snap => "Pool_SB_Ctrl_Discard_Snap",
      when Tag_Pool_SB_Ctrl_Initialize => "Pool_SB_Ctrl_Initialize",
      when Tag_Pool_SB_Ctrl_Deinitialize => "Pool_SB_Ctrl_Deinitialize",
      when Tag_SB_Ctrl_TA_Create_Key => "SB_Ctrl_TA_Create_Key",
      when Tag_SB_Ctrl_TA_Secure_SB => "SB_Ctrl_TA_Secure_SB",
      when Tag_SB_Ctrl_TA_Encrypt_Key => "SB_Ctrl_TA_Encrypt_Key",
      when Tag_SB_Ctrl_TA_Decrypt_Key => "SB_Ctrl_TA_Decrypt_Key",
      when Tag_SB_Ctrl_Cache => "SB_Ctrl_Cache",
      when Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA => "SB_Ctrl_VBD_Rkg_Rekey_VBA",
      when Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step => "SB_Ctrl_VBD_Rkg_VBD_Ext_Step",
      when Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step => "SB_Ctrl_FT_Rszg_FT_Ext_Step",
      when Tag_SB_Ctrl_Crypto_Add_Key => "SB_Ctrl_Crypto_Add_Key",
      when Tag_SB_Ctrl_Crypto_Remove_Key => "SB_Ctrl_Crypto_Remove_Key",
      when Tag_SB_Ctrl_Blk_IO_Sync => "SB_Ctrl_Blk_IO_Sync",
      when Tag_VBD_Rkg_Cache => "VBD_Rkg_Cache",
      when Tag_FT_Rszg_Cache => "FT_Rszg_Cache",
      when Tag_FT_Rszg_MT_Alloc => "FT_Rszg_MT_Alloc",
      when Tag_VBD_Rkg_Blk_IO => "VBD_Rkg_Blk_IO",
      when Tag_VBD_Rkg_Crypto_Encrypt => "VBD_Rkg_Crypto_Encrypt",
      when Tag_VBD_Rkg_Crypto_Decrypt => "VBD_Rkg_Crypto_Decrypt",
      when Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg => "VBD_Rkg_FT_Alloc_For_Non_Rkg",
      when Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks =>
         "VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blk",
      when Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks =>
         "VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blk",
      when Tag_SB_Ctrl_Blk_IO_Write_SB => "SB_Ctrl_Blk_IO_Write_SB",
      when Tag_SB_Ctrl_Blk_IO_Read_SB => "SB_Ctrl_Blk_IO_Read_SB",
      when Tag_IO => "IO",
      when Tag_Translation => "Translation",
      when Tag_Write_Back => "Write_Back",
      when Tag_Cache => "Cache",
      when Tag_Cache_Flush => "Cache_Flush",
      when Tag_Cache_Blk_IO => "Cache_Blk_IO",
      when Tag_Lib_Cache_Sync => "Lib_Cache_Sync",
      when Tag_Decrypt => "Decrypt",
      when Tag_Encrypt => "Encrypt",
      when Tag_Sync_SB_Cache_Flush => "Sync_SB_Cache_Flush",
      when Tag_Sync_SB_TA_Encrypt_Key => "Tag_Sync_SB_TA_Encrypt_Key",
      when Tag_Sync_SB_Write_SB => "Sync_SB_Write_SB",
      when Tag_Sync_SB_Sync => "Sync_SB_Sync",
      when Tag_VBD_Cache => "VBD_Cache",
      when Tag_FT_Cache => "FT_Cache",
      when Tag_WB_Cache => "WB_Cache",
      when Tag_SCD_Cache => "SCD_Cache",
      when Tag_Free_Tree_Query => "Free_Tree_Query",
      when Tag_Free_Tree_IO => "Free_Tree_IO",
      when Tag_Free_Tree_WB => "Free_Tree_WB",
      when Tag_FT_MT => "FT_MT",
      when Tag_MT_Cache => "MT_Cache");

   type Index_Type  is range 0 .. 2**32 - 1;
   type Object_Type is private;

   function Invalid_Index return Index_Type is (Index_Type'Last);

   --
   --  Copy_Valid_Object_New_Tag
   --
   function Copy_Valid_Object_New_Tag (
      Obj : Object_Type;
      Tag : Tag_Type)
   return Object_Type
   with Pre => (Valid (Obj));

   --
   --  Copy_Valid_Object_New_Succ_Blk_Nr
   --
   function Copy_Valid_Object_New_Succ_Blk_Nr (
      Obj    : Object_Type;
      Succ   : Request.Success_Type;
      Blk_Nr : Block_Number_Type)
   return Object_Type
   with Pre => (Valid (Obj));

   --
   --  Invalid_Object
   --
   function Invalid_Object
   return Object_Type
   with Post => (not Valid (Invalid_Object'Result));

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
   with
      Post => (
         Valid (Valid_Object'Result) and then (
            Operation    (Valid_Object'Result) = Op     and then
            Success      (Valid_Object'Result) = Succ   and then
            Block_Number (Valid_Object'Result) = Blk_Nr and then
            Index        (Valid_Object'Result) = Idx    and then
            Tag          (Valid_Object'Result) = Tg));

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
   with
      Post => (
         Valid (Valid_Object_No_Pool_Idx'Result) and then (
            Operation    (Valid_Object_No_Pool_Idx'Result) = Op     and then
            Success      (Valid_Object_No_Pool_Idx'Result) = Succ   and then
            Block_Number (Valid_Object_No_Pool_Idx'Result) = Blk_Nr and then
            Index        (Valid_Object_No_Pool_Idx'Result) = Idx    and then
            Tag          (Valid_Object_No_Pool_Idx'Result) = Tg));

   --
   --  Equal
   --
   function Equal (
      Obj_1 : Object_Type;
      Obj_2 : Object_Type)
   return Boolean
   with
      Pre => Valid (Obj_1) and then Valid (Obj_2);

   ----------------------
   --  Read Accessors  --
   ----------------------

   function Valid (Obj : Object_Type) return Boolean;

   function Operation (Obj : Object_Type) return Primitive_Operation_Type
   with Pre => (Valid (Obj));

   function Success (Obj : Object_Type) return Request.Success_Type
   with Pre => (Valid (Obj));

   function Tag (Obj : Object_Type) return Tag_Type
   with Pre => (Valid (Obj));

   function Pool_Idx_Slot (Obj : Object_Type) return Pool_Index_Slot_Type
   with Pre => (Valid (Obj));

   function Block_Number (Obj : Object_Type) return Block_Number_Type
   with Pre => (Valid (Obj));

   function Index (Obj : Object_Type) return Index_Type
   with Pre => (Valid (Obj));

   -----------------------
   --  Write Accessors  --
   -----------------------

   procedure Success (
      Obj   : in out Object_Type;
      Value :        Request.Success_Type)
   with Pre => (Valid (Obj));

   procedure Block_Number (
      Obj   : in out Object_Type;
      Value :        Block_Number_Type)
   with Pre => (Valid (Obj));

   procedure Operation (
      Obj   : in out Object_Type;
      Value :        Primitive_Operation_Type)
   with Pre => (Valid (Obj));

   function To_String (Obj : Object_Type) return String;

   function Has_Tag_Lib_SB_Init (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_VBD_Init (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_FT_Init (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_MT_Init (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_TA_Create_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Init_TA_Encrypt_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Init_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Init_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Init_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Init_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Lib_SB_Check (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Check_VBD_Check (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Check_FT_Check (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Check_MT_Check (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Check_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Check_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Check_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Check_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Check_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Lib_SB_Dump (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Dump_VBD_Dump (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Dump_FT_Dump (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Dump_MT_Dump (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Dump_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Dump_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Dump_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Dump_Blk_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Dump_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_Discard_Snap (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_Create_Snap (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_Sync (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_VBD (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_SB_Ctrl_Init_Rekey (Obj : Object_Type) return Boolean;
   function Has_Tag_Pool_SB_Ctrl_Rekey_VBA (Obj : Object_Type) return Boolean;

   function Has_Tag_Pool_SB_Ctrl_VBD_Ext_Step (Obj : Object_Type)
   return Boolean;

   function Has_Tag_Pool_SB_Ctrl_FT_Ext_Step (Obj : Object_Type)
   return Boolean;

   function Has_Tag_Pool_SB_Ctrl_Create_Snap (Obj : Object_Type)
   return Boolean;

   function Has_Tag_Pool_SB_Ctrl_Discard_Snap (Obj : Object_Type)
   return Boolean;

   function Has_Tag_Pool_SB_Ctrl_Initialize (Obj : Object_Type)
   return Boolean;

   function Has_Tag_Pool_SB_Ctrl_Deinitialize (Obj : Object_Type)
   return Boolean;

   function Has_Tag_SB_Ctrl_TA_Create_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_TA_Secure_SB (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_TA_Encrypt_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_TA_Decrypt_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_Cache (Obj : Object_Type) return Boolean;

   function Has_Tag_SB_Ctrl_VBD_Rkg_Rekey_VBA (Obj : Object_Type)
   return Boolean;

   function Has_Tag_SB_Ctrl_VBD_Rkg_VBD_Ext_Step (Obj : Object_Type)
   return Boolean;

   function Has_Tag_SB_Ctrl_FT_Rszg_FT_Ext_Step (Obj : Object_Type)
   return Boolean;

   function Has_Tag_SB_Ctrl_Crypto_Add_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_Crypto_Remove_Key (Obj : Object_Type)
   return Boolean;

   function Has_Tag_SB_Ctrl_Blk_IO_Write_SB (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_Blk_IO_Read_SB (Obj : Object_Type) return Boolean;
   function Has_Tag_SB_Ctrl_Blk_IO_Sync (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Rkg_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Rszg_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Rszg_MT_Alloc (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Rkg_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Rkg_Crypto_Encrypt (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Rkg_Crypto_Decrypt (Obj : Object_Type) return Boolean;

   function Has_Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg (Obj : Object_Type)
   return Boolean;

   function Has_Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks (Obj : Object_Type)
   return Boolean;

   function Has_Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks (Obj : Object_Type)
   return Boolean;

   function Has_Tag_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Translation (Obj : Object_Type) return Boolean;
   function Has_Tag_Write_Back (Obj : Object_Type) return Boolean;
   function Has_Tag_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_Cache_Flush (Obj : Object_Type) return Boolean;
   function Has_Tag_Cache_Blk_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Lib_Cache_Sync (Obj : Object_Type) return Boolean;
   function Has_Tag_Decrypt (Obj : Object_Type) return Boolean;
   function Has_Tag_Encrypt (Obj : Object_Type) return Boolean;
   function Has_Tag_Sync_SB_Cache_Flush (Obj : Object_Type) return Boolean;
   function Has_Tag_Sync_SB_TA_Encrypt_Key (Obj : Object_Type) return Boolean;
   function Has_Tag_Sync_SB_Write_SB (Obj : Object_Type) return Boolean;
   function Has_Tag_Sync_SB_Sync (Obj : Object_Type) return Boolean;
   function Has_Tag_VBD_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_WB_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_SCD_Cache (Obj : Object_Type) return Boolean;
   function Has_Tag_Free_Tree_IO (Obj : Object_Type) return Boolean;
   function Has_Tag_Free_Tree_WB (Obj : Object_Type) return Boolean;
   function Has_Tag_FT_MT (Obj : Object_Type) return Boolean;
   function Has_Tag_MT_Cache (Obj : Object_Type) return Boolean;

private

   --
   --  Object_Type
   --
   type Object_Type is record
      Valid         : Boolean;
      Operation     : Primitive_Operation_Type;
      Success       : Request.Success_Type;
      Tag           : Tag_Type;
      Pool_Idx_Slot : Pool_Index_Slot_Type;
      Block_Number  : Block_Number_Type;
      Index         : Index_Type;
   end record;

end CBE.Primitive;
