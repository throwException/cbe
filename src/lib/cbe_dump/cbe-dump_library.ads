--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Superblock_Dump;
with CBE.VBD_Dump;
with CBE.Free_Tree_Dump;
with CBE.Block_IO;
with CBE.Request;

package CBE.Dump_Library
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   procedure Initialize_Object (Obj : out Object_Type);

   function Client_Request_Acceptable (Obj : Object_Type)
   return Boolean;

   procedure Submit_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type;
      Cfg :        Dump_Configuration_Type);

   function Peek_Completed_Client_Request (Obj : Object_Type)
   return Request.Object_Type;

   procedure Drop_Completed_Client_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   procedure IO_Request_Completed (
      Obj        : in out Object_Type;
      Data_Index :        Block_IO.Data_Index_Type;
      Success    :        Boolean);

   procedure Has_IO_Request (
      Obj      :     Object_Type;
      Req      : out Request.Object_Type;
      Data_Idx : out Block_IO.Data_Index_Type);

   procedure IO_Request_In_Progress (
      Obj      : in out Object_Type;
      Data_Idx :        Block_IO.Data_Index_Type);

   procedure Execute (
      Obj        : in out Object_Type;
      Blk_IO_Buf :        Block_IO.Data_Type);

   function Execute_Progress (Obj : Object_Type)
   return Boolean;

private

   type Object_Type is record
      SB_Dmp              : Superblock_Dump.Object_Type;
      VBD_Dmp             : VBD_Dump.Object_Type;
      FT_Dmp              : Free_Tree_Dump.Object_Type;
      Blk_IO              : Block_IO.Object_Type;
      Client_Req          : Request.Object_Type;
      Client_Cfg          : Dump_Configuration_Type;
      Client_Req_Complete : Boolean;
      Execute_Progress    : Boolean;
   end record;

   procedure Execute_Superblock_Dump (Obj : in out Object_Type);

   procedure Execute_VBD_Dump (Obj : in out Object_Type);

   procedure Execute_Free_Tree_Dump (Obj : in out Object_Type);

   procedure Execute_Block_IO (
      Obj        : in out Object_Type;
      Blk_IO_Buf :        Block_IO.Data_Type);

end CBE.Dump_Library;
