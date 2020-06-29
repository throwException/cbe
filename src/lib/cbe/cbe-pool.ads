--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Request;
with CBE.Primitive;
with CBE.Generic_Index_Queue;

package CBE.Pool
with SPARK_Mode
is
   pragma Pure;

   type Object_Type is private;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type;

   --
   --  Request_Acceptable
   --
   function Request_Acceptable (Obj : Object_Type)
   return Boolean;

   --
   --  Submit_Request
   --
   procedure Submit_Request (
      Obj     : in out Object_Type;
      Req     :        Request.Object_Type;
      Snap_ID :        Snapshot_ID_Type);

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean);

   --
   --  Peek_Generated_Discard_Snap_Primitive
   --
   function Peek_Generated_Discard_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Create_Snap_Primitive
   --
   function Peek_Generated_Create_Snap_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Sync_Primitive
   --
   function Peek_Generated_Sync_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_VBD_Primitive
   --
   function Peek_Generated_VBD_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_VBD_Primitive_ID
   --
   function Peek_Generated_VBD_Primitive_ID (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Snapshot_ID_Type;

   --
   --  Peek_Generated_SB_Ctrl_Primitive
   --
   function Peek_Generated_SB_Ctrl_Primitive (Obj : Object_Type)
   return Primitive.Object_Type;

   --
   --  Peek_Generated_Nr_Of_Blks
   --
   function Peek_Generated_Nr_Of_Blks (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Number_Of_Blocks_Type;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Idx  :        Pool_Index_Type);

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj     : in out Object_Type;
      Idx     :        Pool_Index_Type;
      Success :        Boolean);

   --
   --  Mark_Generated_Primitive_Complete_Req_Fin
   --
   procedure Mark_Generated_Primitive_Complete_Req_Fin (
      Obj              : in out Object_Type;
      Idx              :        Pool_Index_Type;
      Success          :        Boolean;
      Request_Finished :        Boolean);

   --
   --  Mark_Generated_Primitive_Complete_SB_State
   --
   procedure Mark_Generated_Primitive_Complete_SB_State (
      Obj      : in out Object_Type;
      Idx      :        Pool_Index_Type;
      Success  :        Boolean;
      SB_State :        Superblock_State_Type);

   --
   --  Peek_Completed_Request
   --
   function Peek_Completed_Request (Obj : Pool.Object_Type)
   return Request.Object_Type;

   --
   --  Drop_Completed_Request
   --
   procedure Drop_Completed_Request (
      Obj : in out Object_Type;
      Req :        Request.Object_Type);

   --
   --  Request_For_Index
   --
   function Request_For_Index (
      Obj : Object_Type;
      Idx : Pool_Index_Type)
   return Request.Object_Type;

private

   Max_Nr_Of_Requests_Preponed_At_A_Time : constant := 8;

   type Item_State_Type is (
      Invalid,
      Pending,
      In_Progress,
      Submitted,
      Submitted_Resume_Rekeying,
      Rekey_Init_Pending,
      Rekey_Init_In_Progress,
      Rekey_Init_Complete,
      Prepone_Requests_Pending,
      Prepone_Requests_Complete,
      VBD_Extension_Step_Pending,
      VBD_Extension_Step_In_Progress,
      VBD_Extension_Step_Complete,
      FT_Extension_Step_Pending,
      FT_Extension_Step_In_Progress,
      FT_Extension_Step_Complete,
      Rekey_VBA_Pending,
      Rekey_VBA_In_Progress,
      Rekey_VBA_Complete,
      Initialize_SB_Ctrl_Pending,
      Initialize_SB_Ctrl_In_Progress,
      Initialize_SB_Ctrl_Complete,
      Deinitialize_SB_Ctrl_Pending,
      Deinitialize_SB_Ctrl_In_Progress,
      Deinitialize_SB_Ctrl_Complete,
      Complete);

   type Item_Type is record
      State                   : Item_State_Type;
      Req                     : Request.Object_Type;
      Snap_ID                 : Snapshot_ID_Type;
      Prim                    : Primitive.Object_Type;
      Request_Finished        : Boolean;
      Nr_Of_Requests_Preponed : Number_Of_Requests_Type;
      Nr_Of_Prims_Completed   : Number_Of_Primitives_Type;
      SB_State                : Superblock_State_Type;
   end record;

   type Items_Type is array (Pool_Index_Type) of Item_Type;

   type Index_Queue_Index_Type is range 0 .. Pool_Index_Type'Range_Length;

   package Index_Queue is new Generic_Index_Queue (
      Pool_Index_Type, Index_Queue_Index_Type);

   type Object_Type is record
      Items   : Items_Type;
      Indices : Index_Queue.Queue_Type;
   end record;

   --
   --  Item_Invalid
   --
   function Item_Invalid
   return Item_Type;

   --
   --  Item_Nr_Of_Prims
   --
   function Item_Nr_Of_Prims (Itm : Item_Type)
   return Number_Of_Primitives_Type;

   --
   --  Execute_Rekey
   --
   procedure Execute_Rekey (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Extend_VBD
   --
   procedure Execute_Extend_VBD (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Extend_FT
   --
   procedure Execute_Extend_FT (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Initialize
   --
   procedure Execute_Initialize (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean);

   --
   --  Execute_Deinitialize
   --
   procedure Execute_Deinitialize (
      Items    : in out Items_Type;
      Indices  : in out Index_Queue.Queue_Type;
      Idx      :        Pool_Index_Type;
      Progress : in out Boolean);

end CBE.Pool;
