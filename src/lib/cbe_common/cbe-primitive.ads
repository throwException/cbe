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
      Tag_SB_Init_TA_Secure_SB,
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
      Tag_Pool_VBD,
      Tag_Pool_SB_Ctrl_Init_Rekey,
      Tag_Pool_SB_Ctrl_Rekey_VBA,
      Tag_Pool_SB_Ctrl_VBD_Ext_Step,
      Tag_Pool_SB_Ctrl_FT_Ext_Step,
      Tag_Pool_SB_Ctrl_Create_Snap,
      Tag_Pool_SB_Ctrl_Read_VBA,
      Tag_Pool_SB_Ctrl_Write_VBA,
      Tag_Pool_SB_Ctrl_Sync,
      Tag_Pool_SB_Ctrl_Discard_Snap,
      Tag_Pool_SB_Ctrl_Initialize,
      Tag_Pool_SB_Ctrl_Deinitialize,
      Tag_SB_Ctrl_TA_Create_Key,
      Tag_SB_Ctrl_TA_Secure_SB,
      Tag_SB_Ctrl_TA_Encrypt_Key,
      Tag_SB_Ctrl_TA_Decrypt_Key,
      Tag_SB_Ctrl_TA_Last_SB_Hash,
      Tag_SB_Ctrl_Cache,
      Tag_SB_Ctrl_VBD_Rkg_Read_VBA,
      Tag_SB_Ctrl_VBD_Rkg_Write_VBA,
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
      Tag_FT_Rszg_MT_Rszg_Extend_By_One_Leaf,
      Tag_Blk_IO_Blk_Dev_Read,
      Tag_Blk_IO_Blk_Dev_Write,
      Tag_Blk_IO_Crypto_Decrypt_And_Supply_Client_Data,
      Tag_Blk_IO_Crypto_Obtain_And_Encrypt_Client_Data,
      Tag_Crypto_IO_Crypto_Dev_Decrypt,
      Tag_Crypto_IO_Client_Supply_Data,
      Tag_Crypto_IO_Crypto_Dev_Encrypt,
      Tag_Crypto_IO_Client_Obtain_Data,
      Tag_VBD_Rkg_Blk_IO,
      Tag_VBD_Rkg_Blk_IO_Read_Client_Data,
      Tag_VBD_Rkg_Blk_IO_Write_Client_Data,
      Tag_VBD_Rkg_Crypto_Encrypt,
      Tag_VBD_Rkg_Crypto_Decrypt,
      Tag_VBD_Rkg_FT_Alloc_For_Non_Rkg,
      Tag_VBD_Rkg_FT_Alloc_For_Rkg_Curr_Gen_Blks,
      Tag_VBD_Rkg_FT_Alloc_For_Rkg_Old_Gen_Blks,
      Tag_IO,
      Tag_Translation,
      Tag_Cache,
      Tag_Cache_Flush,
      Tag_Cache_Blk_IO,
      Tag_Decrypt,
      Tag_Encrypt,
      Tag_FT_Cache,
      Tag_Free_Tree_Query,
      Tag_Free_Tree_IO,
      Tag_FT_MT,
      Tag_MT_Rszg_Cache,
      Tag_MT_Rszg_MT_Alloc,
      Tag_MT_Cache);

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
