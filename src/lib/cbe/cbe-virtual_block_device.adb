--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with CBE.Debug;

package body CBE.Virtual_Block_Device
with SPARK_Mode
is
   --
   --  Initialize_Object
   --
   procedure Initialize_Object (Obj : out Object_Type)
   is
   begin
      Obj := Initialized_Object;
   end Initialize_Object;

   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is
   begin
      return (
         Trans_Helper     => Tree_Helper.Invalid_Object,
         Trans            => Translation.Initialized_Object (False),
         Execute_Progress => False,
         Cache_Prim       => Primitive.Invalid_Object,
         Cache_Prim_State => Invalid,
         Cache_Prim_Data  => (others => 0),
         Key_ID           => Key_ID_Type'First);
   end Initialized_Object;

   --
   --  Trans_Inhibit_Translation
   --
   procedure Trans_Inhibit_Translation (Obj : in out Object_Type)
   is
   begin
      Translation.Suspend (Obj.Trans);
   end Trans_Inhibit_Translation;

   --
   --  Trans_Resume_Translation
   --
   procedure Trans_Resume_Translation (Obj : in out Object_Type)
   is
   begin
      Translation.Resume (Obj.Trans);
   end Trans_Resume_Translation;

   --
   --  Trans_Get_Virtual_Block_Address
   --
   function Trans_Get_Virtual_Block_Address (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Block_Number_Type
   is
   begin
      if
         Primitive.Block_Number (Prim) /=
            Block_Number_Type (Translation.Data_PBA (Obj.Trans))
      then
         raise Program_Error;
      end if;

      return Translation.Get_Virtual_Block_Address (Obj.Trans);
   end Trans_Get_Virtual_Block_Address;

   --
   --  Trans_Can_Get_Type_1_Node_Walk
   --
   function Trans_Can_Get_Type_1_Node_Walk (
      Obj   : Object_Type;
      Prim  : Primitive.Object_Type)
   return Boolean
   is (Translation.Can_Get_Type_1_Node_Walk (Obj.Trans, Prim));

   --
   --  Trans_Get_Type_1_Node_Walk
   --
   procedure Trans_Get_Type_1_Node_Walk (
      Obj  :        Object_Type;
      Walk : in out Type_1_Node_Walk_Type)
   is
   begin
      Translation.Get_Type_1_Node_Walk (Obj.Trans, Walk);
   end Trans_Get_Type_1_Node_Walk;

   --
   --  Tree_Max_Level
   --
   function Tree_Max_Level (Obj : Object_Type)
   return Tree_Level_Index_Type
   is (Tree_Helper.Max_Level (Obj.Trans_Helper));

   --
   --  Index_For_Level
   --
   function Index_For_Level (
      Obj   : Object_Type;
      VBA   : Virtual_Block_Address_Type;
      Level : Tree_Level_Index_Type)
   return Tree_Child_Index_Type
   is (Tree_Helper.Index (Obj.Trans_Helper, VBA, Level));

   --
   --  Get_Tree_Helper
   --
   function Get_Tree_Helper (Obj : Object_Type)
   return Tree_Helper.Object_Type
   is (Obj.Trans_Helper);

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (Translation.Acceptable (Obj.Trans));

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj             : in out Object_Type;
      PBA             :        Physical_Block_Address_Type;
      Gen             :        Generation_Type;
      Hash            :        Hash_Type;
      Max_Level       :        Tree_Level_Index_Type;
      Degree          :        Tree_Degree_Type;
      Leafs           :        Tree_Number_Of_Leafs_Type;
      Prim            :        Primitive.Object_Type;
      Rekeying        :        Boolean;
      Rekeying_VBA    :        Virtual_Block_Address_Type;
      Previous_Key_ID :        Key_ID_Type;
      Current_Key_ID  :        Key_ID_Type)
   is
      Prim_VBA : constant Virtual_Block_Address_Type :=
         Virtual_Block_Address_Type (Primitive.Block_Number (Prim));
   begin
      Obj.Trans_Helper :=
         Tree_Helper.Initialized_Object (Degree, Max_Level, Leafs);

      if Rekeying and then
         Rekeying_VBA <= Prim_VBA
      then
         Obj.Key_ID := Previous_Key_ID;
      else
         Obj.Key_ID := Current_Key_ID;
      end if;

      Translation.Submit_Primitive (
         Obj.Trans, PBA, Gen, Hash, Obj.Trans_Helper, Prim);
   end Submit_Primitive;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (Translation.Peek_Completed_Primitive (Obj.Trans));

   --
   --  Peek_Completed_Hash
   --
   function Peek_Completed_Hash (Obj : Object_Type)
   return Hash_Type
   is (Translation.Peek_Completed_Hash (Obj.Trans));

   --
   --  Peek_Completed_Generation
   --
   function Peek_Completed_Generation (Obj : Object_Type)
   return Generation_Type
   is (Translation.Peek_Completed_Generation (Obj.Trans));

   --
   --  Peek_Completed_Key_ID
   --
   function Peek_Completed_Key_ID (Obj : Object_Type)
   return Key_ID_Type
   is (Obj.Key_ID);

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin
      Translation.Drop_Completed_Primitive (Obj.Trans);
   end Drop_Completed_Primitive;

   --
   --  Execute
   --
   procedure Execute (
      Obj        : in out Object_Type;
      Trans_Data : in out Translation_Data_Type)
   is
   begin
      Obj.Execute_Progress := False;

      Translation.Execute (Obj.Trans, Trans_Data);
      if Translation.Execute_Progress (Obj.Trans) then
         Obj.Execute_Progress := True;
      end if;

      Declare_Prim :
      declare
         Prim : constant Primitive.Object_Type :=
            Translation.Peek_Generated_Primitive (Obj.Trans);
      begin
         if Primitive.Valid (Prim) then
            Declare_Cache_Prim :
            declare
               Cache_Prim : constant Primitive.Object_Type :=
                  Primitive.Valid_Object_No_Pool_Idx (
                     Read, False, Primitive.Tag_VBD_Cache,
                     Primitive.Block_Number (Prim), 0);
            begin
               if Obj.Cache_Prim_State = Invalid then

                  Obj.Cache_Prim_State := Generated;
                  Obj.Cache_Prim := Cache_Prim;
                  Obj.Execute_Progress := True;
               elsif
                  Obj.Cache_Prim_State = Complete and then
                  Primitive.Equal (Obj.Cache_Prim, Cache_Prim)
               then
                  if not Primitive.Success (Obj.Cache_Prim) then
                     raise Program_Error;
                  end if;
                  Translation.Mark_Generated_Primitive_Complete (
                     Obj.Trans, Obj.Cache_Prim_Data, Trans_Data);

                  Translation.Discard_Generated_Primitive (Obj.Trans);
                  Obj.Cache_Prim_State := Invalid;
                  Obj.Execute_Progress := True;
               end if;
            end Declare_Cache_Prim;
         end if;
      end Declare_Prim;
   end Execute;

   --
   --  Peek_Generated_Cache_Primitive
   --
   function Peek_Generated_Cache_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is (
      if Obj.Cache_Prim_State = Generated then
         Obj.Cache_Prim
      else
         Primitive.Invalid_Object);

   --
   --  Peek_Generated_Cache_Lvl
   --
   function Peek_Generated_Cache_Lvl (Obj : Object_Type)
   return Tree_Level_Index_Type
   is
   begin
      if Obj.Cache_Prim_State = Generated then
         return Translation.Peek_Generated_Level (Obj.Trans);
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Lvl;

   --
   --  Peek_Generated_Cache_Data
   --
   function Peek_Generated_Cache_Data (Obj : Object_Type)
   return Block_Data_Type
   is
   begin
      if Obj.Cache_Prim_State = Generated then
         return Obj.Cache_Prim_Data;
      else
         raise Program_Error;
      end if;
   end Peek_Generated_Cache_Data;

   --
   --  Drop_Generated_Cache_Primitive
   --
   procedure Drop_Generated_Cache_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      if Obj.Cache_Prim_State = Generated and then
         Primitive.Equal (Obj.Cache_Prim, Prim)
      then
         Obj.Cache_Prim_State := Dropped;
      else
         raise Program_Error;
      end if;
   end Drop_Generated_Cache_Primitive;

   --
   --  Mark_Generated_Cache_Primitive_Complete
   --
   procedure Mark_Generated_Cache_Primitive_Complete (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Data :        Block_Data_Type)
   is
   begin
      if Obj.Cache_Prim_State = Dropped and then
         Primitive.Equal (Obj.Cache_Prim, Prim)
      then
         Obj.Cache_Prim_State := Complete;
         Obj.Cache_Prim_Data := Data;
         Obj.Cache_Prim := Prim;
      else
         raise Program_Error;
      end if;
   end Mark_Generated_Cache_Primitive_Complete;

   -----------------
   --  Accessors  --
   -----------------

   function Execute_Progress (Obj : Object_Type) return Boolean
   is (Obj.Execute_Progress);

   function To_String (Obj : Object_Type) return String
   is (
      "VBD (Execute_Progress=" &
      Debug.To_String (Obj.Execute_Progress) &
      ", Trans=" &
      Translation.To_String (Obj.Trans) &
      ")");

end CBE.Virtual_Block_Device;
