--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

with SHA256_4K;

package body CBE.Block_IO
with SPARK_Mode
is
   --
   --  CBE_Hash_From_SHA256_4K_Hash
   --
   procedure CBE_Hash_From_SHA256_4K_Hash (
      CBE_Hash : out Hash_Type;
      SHA_Hash :     SHA256_4K.Hash_Type);

   --
   --  SHA256_4K_Data_From_CBE_Data
   --
   procedure SHA256_4K_Data_From_CBE_Data (
      SHA_Data : out SHA256_4K.Data_Type;
      CBE_Data :     Block_Data_Type);

   --
   --  Hash_Of_Data_Blk
   --
   function Hash_Of_Data_Blk (CBE_Data : Block_Data_Type)
   return Hash_Type;

   --
   --  CBE_Hash_From_SHA256_4K_Hash
   --
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

   --
   --  SHA256_4K_Data_From_CBE_Data
   --
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

   --
   --  Hash_Of_Data_Blk
   --
   function Hash_Of_Data_Blk (CBE_Data : Block_Data_Type)
   return Hash_Type
   is
      SHA_Hash : SHA256_4K.Hash_Type;
      SHA_Data : SHA256_4K.Data_Type;
      CBE_Hash : Hash_Type;
   begin
      SHA256_4K_Data_From_CBE_Data (SHA_Data, CBE_Data);
      SHA256_4K.Hash (SHA_Data, SHA_Hash);
      CBE_Hash_From_SHA256_4K_Hash (CBE_Hash, SHA_Hash);
      return CBE_Hash;
   end Hash_Of_Data_Blk;

   function Invalid_Entry return Entry_Type
   is (
     Orig_Tag       => Primitive.Tag_Invalid,
     Prim           => Primitive.Invalid_Object,
     Submitted_Prim => Primitive.Invalid_Object,
     Generated_Prim => Primitive.Invalid_Object,
     Hash_Valid     => False,
     Hash           => (others => 0),
     Req            => Request.Invalid_Object,
     VBA            => Virtual_Block_Address_Type'First,
     State          => Unused,
     Key_ID         => Key_ID_Invalid);

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
   is (
      Entries => (others => Invalid_Entry),
      Used_Entries => 0);

   function Primitive_Acceptable (Obj : Object_Type) return Boolean
   is (Obj.Used_Entries < Num_Entries_Type'Last);

   procedure Submit_Primitive (
      Obj  : in out Object_Type;
      Tag  :        Primitive.Tag_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for Idx in Obj.Entries'Range loop
         if Obj.Entries (Idx).State = Unused then

            Obj.Entries (Idx).Orig_Tag   := Primitive.Tag (Prim);
            Obj.Entries (Idx).Prim       :=
               Primitive.Copy_Valid_Object_New_Tag (Prim, Tag);
            Obj.Entries (Idx).Hash_Valid := False;
            Obj.Entries (Idx).Hash       := (others => 0);
            Obj.Entries (Idx).State      := Pending;
            Obj.Entries (Idx).Key_ID     := Key_ID_Type'First;

            Obj.Used_Entries := Obj.Used_Entries + 1;
            return;
         end if;
      end loop;
      raise Program_Error;
   end Submit_Primitive;

   --
   --  Submit_Primitive_Client_Data
   --
   procedure Submit_Primitive_Client_Data (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Req    :        Request.Object_Type;
      VBA    :        Virtual_Block_Address_Type;
      Key_ID :        Key_ID_Type)
   is
   begin

      for Idx in Obj.Entries'Range loop

         if Obj.Entries (Idx).State = Unused then

            case Primitive.Tag (Prim) is
            when Primitive.Tag_VBD_Rkg_Blk_IO_Read_Client_Data =>

               Obj.Entries (Idx).Submitted_Prim := Prim;
               Obj.Entries (Idx).Req := Req;
               Obj.Entries (Idx).VBA := VBA;
               Obj.Entries (Idx).State := Read_Client_Data_Submitted;
               Obj.Used_Entries := Obj.Used_Entries + 1;
               Obj.Entries (Idx).Key_ID := Key_ID;
               return;

            when Primitive.Tag_VBD_Rkg_Blk_IO_Write_Client_Data =>

               Obj.Entries (Idx).Submitted_Prim := Prim;
               Obj.Entries (Idx).Req := Req;
               Obj.Entries (Idx).VBA := VBA;
               Obj.Entries (Idx).State := Write_Client_Data_Submitted;
               Obj.Used_Entries := Obj.Used_Entries + 1;
               Obj.Entries (Idx).Key_ID := Key_ID;
               return;

            when others =>

               raise Program_Error;

            end case;

         end if;

      end loop;
      raise Program_Error;

   end Submit_Primitive_Client_Data;

   procedure Submit_Primitive_Decrypt (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Hash   :        Hash_Type;
      Key_ID :        Key_ID_Type)
   is
   begin
      for Idx in Obj.Entries'Range loop
         if Obj.Entries (Idx).State = Unused then

            Obj.Entries (Idx).Orig_Tag   := Primitive.Tag (Prim);
            Obj.Entries (Idx).Prim       :=
               Primitive.Copy_Valid_Object_New_Tag (
                  Prim, Primitive.Tag_Decrypt);
            Obj.Entries (Idx).Hash_Valid := True;
            Obj.Entries (Idx).Hash       := Hash;
            Obj.Entries (Idx).State      := Pending;
            Obj.Entries (Idx).Key_ID     := Key_ID;

            Obj.Used_Entries := Obj.Used_Entries + 1;
            return;
         end if;
      end loop;
      raise Program_Error;
   end Submit_Primitive_Decrypt;

   procedure Submit_Primitive (
      Obj        : in out Object_Type;
      Tag        :        Primitive.Tag_Type;
      Prim       :        Primitive.Object_Type;
      Data_Index :    out Data_Index_Type)
   is
   begin
      for Idx in Obj.Entries'Range loop
         if Obj.Entries (Idx).State = Unused then

            Obj.Entries (Idx).Orig_Tag := Primitive.Tag (Prim);
            Obj.Entries (Idx).Prim     := Primitive.Valid_Object_No_Pool_Idx (
               Op     => Primitive.Operation (Prim),
               Succ   => Primitive.Success (Prim),
               Tg     => Tag,
               Blk_Nr => Primitive.Block_Number (Prim),
               Idx    => Primitive.Index (Prim));
            Obj.Entries (Idx).State      := Pending;
            Obj.Entries (Idx).Key_ID     := Key_ID_Invalid;
            Obj.Entries (Idx).Hash_Valid := False;
            Obj.Entries (Idx).Hash       := (others => 0);

            Data_Index       := Idx;
            Obj.Used_Entries := Obj.Used_Entries + 1;
            return;
         end if;
      end loop;
      raise Program_Error;
   end Submit_Primitive;

   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin
      for I in Obj.Entries'Range loop
         case Obj.Entries (I).State is
         when Complete =>
            return Obj.Entries (I).Prim;
         when Read_Client_Data_Completed =>
            return Obj.Entries (I).Submitted_Prim;
         when Write_Client_Data_Completed =>
            return Obj.Entries (I).Submitted_Prim;
         when others =>
            null;
         end case;
      end loop;

      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   function Peek_Completed_Key_ID (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            return Obj.Entries (I).Key_ID;
         end if;
      end loop;

      raise Program_Error;
   end Peek_Completed_Key_ID;

   function Peek_Completed_Data_Index (Obj : Object_Type)
   return Data_Index_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if Obj.Entries (I).State = Complete then
            return I;
         end if;
      end loop;

      --  XXX precondition
      raise Program_Error;
   end Peek_Completed_Data_Index;

   function Peek_Completed_Tag (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Primitive.Tag_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            return Obj.Entries (I).Orig_Tag;
         end if;
      end loop;

      --  XXX precondition
      raise Program_Error;
   end Peek_Completed_Tag;

   function Peek_Completed_Hash (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim) and then
            Obj.Entries (I).Hash_Valid
         then
            return Obj.Entries (I).Hash;
         end if;
      end loop;

      raise Program_Error;
   end Peek_Completed_Hash;

   --
   --  Peek_Completed_Hash_New
   --
   function Peek_Completed_Hash_New (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Hash_Type
   is
   begin

      Find_Corresponding_Entry :
      for Idx in Obj.Entries'Range loop

         case Obj.Entries (Idx).State is
         when Write_Client_Data_Completed =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Submitted_Prim) then
               return Obj.Entries (Idx).Hash;
            end if;

         when others =>

            null;

         end case;

      end loop Find_Corresponding_Entry;
      raise Program_Error;

   end Peek_Completed_Hash_New;

   procedure Drop_Completed_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Complete and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            Obj.Entries (I) := Invalid_Entry;
            Obj.Used_Entries := Obj.Used_Entries - 1;
            return;
         end if;
      end loop;
   end Drop_Completed_Primitive;

   --
   --  Drop_Completed_Primitive_New
   --
   procedure Drop_Completed_Primitive_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Find_Corresponding_Job :
      for Idx in Obj.Entries'Range loop
         if (Obj.Entries (Idx).State = Read_Client_Data_Completed or else
             Obj.Entries (Idx).State = Write_Client_Data_Completed) and then
            Primitive.Equal (Prim, Obj.Entries (Idx).Submitted_Prim)
         then
            Obj.Entries (Idx).State := Unused;
            Obj.Used_Entries := Obj.Used_Entries - 1;
            return;
         end if;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Drop_Completed_Primitive_New;

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Data_Buf :        Data_Type;
      Progress : in out Boolean)
   is
   begin

      Execute_Each_Valid_Entry :
      for Idx in Obj.Entries'Range loop

         case Obj.Entries (Idx).State is
         when
            Read_Client_Data_Submitted |
            Read_Client_Data_Read_Data_Pending |
            Read_Client_Data_Read_Data_In_Progress |
            Read_Client_Data_Read_Data_Completed |
            Read_Client_Data_Decrypt_And_Supply_Data_Pending |
            Read_Client_Data_Decrypt_And_Supply_Data_In_Progress |
            Read_Client_Data_Decrypt_And_Supply_Data_Completed
         =>

            Execute_Read_Client_Data (Obj.Entries (Idx), Idx, Progress);

         when
            Write_Client_Data_Submitted |
            Write_Client_Data_Write_Data_Pending |
            Write_Client_Data_Write_Data_In_Progress |
            Write_Client_Data_Write_Data_Completed |
            Write_Client_Data_Obtain_And_Encrypt_Data_Pending |
            Write_Client_Data_Obtain_And_Encrypt_Data_In_Progress |
            Write_Client_Data_Obtain_And_Encrypt_Data_Completed
         =>

            Execute_Write_Client_Data (
               Obj.Entries (Idx), Idx, Data_Buf, Progress);

         when others =>

            null;

         end case;

      end loop Execute_Each_Valid_Entry;

   end Execute;

   --
   --  Peek_Generated_Blk_Dev_Primitive
   --
   function Peek_Generated_Blk_Dev_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      for Idx in Obj.Entries'Range loop

         case Obj.Entries (Idx).State is
         when Pending =>

            return Obj.Entries (Idx).Prim;

         when Read_Client_Data_Read_Data_Pending =>

            return Obj.Entries (Idx).Generated_Prim;

         when Write_Client_Data_Write_Data_Pending =>

            return Obj.Entries (Idx).Generated_Prim;

         when others =>

            null;

         end case;

      end loop;

      return Primitive.Invalid_Object;

   end Peek_Generated_Blk_Dev_Primitive;

   --
   --  Peek_Generated_Crypto_Primitive
   --
   function Peek_Generated_Crypto_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      for Idx in Obj.Entries'Range loop

         case Obj.Entries (Idx).State is
         when Read_Client_Data_Decrypt_And_Supply_Data_Pending =>

            return Obj.Entries (Idx).Generated_Prim;

         when Write_Client_Data_Obtain_And_Encrypt_Data_Pending =>

            return Obj.Entries (Idx).Generated_Prim;

         when others =>

            null;

         end case;

      end loop;

      return Primitive.Invalid_Object;

   end Peek_Generated_Crypto_Primitive;

   --
   --  Peek_Generated_Data_Index
   --
   function Peek_Generated_Data_Index (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Data_Index_Type
   is
   begin

      for Idx in Obj.Entries'Range loop

         --
         --  FIXME
         --
         --  Why must the function return a valid index when the entry is
         --  'In_Progress'?
         --
         case Obj.Entries (Idx).State is
         when Pending | In_Progress =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Prim) then
               return Idx;
            end if;

         when
            Read_Client_Data_Read_Data_Pending |
            Read_Client_Data_Decrypt_And_Supply_Data_Pending |
            Write_Client_Data_Write_Data_Pending |
            Write_Client_Data_Obtain_And_Encrypt_Data_Pending
         =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
               return Idx;
            end if;

         when others =>

            null;

         end case;

      end loop;
      raise Program_Error;

   end Peek_Generated_Data_Index;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type
   is
      Idx : constant Entries_Index_Type :=
         Entries_Index_Type (Primitive.Index (Prim));
   begin

      case Obj.Entries (Idx).State is
      when
         Read_Client_Data_Decrypt_And_Supply_Data_Pending |
         Write_Client_Data_Obtain_And_Encrypt_Data_Pending
      =>

         if not Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
            raise Program_Error;
         end if;

         return Obj.Entries (Idx).Key_ID;

      when others =>

         raise Program_Error;

      end case;

   end Peek_Generated_Key_ID;

   --
   --  Peek_Generated_Req
   --
   function Peek_Generated_Req (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Request.Object_Type
   is
      Idx : constant Entries_Index_Type :=
         Entries_Index_Type (Primitive.Index (Prim));
   begin

      case Obj.Entries (Idx).State is
      when
         Read_Client_Data_Decrypt_And_Supply_Data_Pending |
         Write_Client_Data_Obtain_And_Encrypt_Data_Pending
      =>

         if not Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
            raise Program_Error;
         end if;

         return Obj.Entries (Idx).Req;

      when others =>

         raise Program_Error;

      end case;

   end Peek_Generated_Req;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Virtual_Block_Address_Type
   is
      Idx : constant Entries_Index_Type :=
         Entries_Index_Type (Primitive.Index (Prim));
   begin

      case Obj.Entries (Idx).State is
      when
         Read_Client_Data_Decrypt_And_Supply_Data_Pending |
         Write_Client_Data_Obtain_And_Encrypt_Data_Pending
      =>

         if not Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
            raise Program_Error;
         end if;

         return Obj.Entries (Idx).VBA;

      when others =>

         raise Program_Error;

      end case;

   end Peek_Generated_VBA;

   --
   --  Drop_Generated_Primitive_New
   --
   procedure Drop_Generated_Primitive_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Entries_Index_Type :=
         Entries_Index_Type (Primitive.Index (Prim));
   begin
      if Obj.Entries (Idx).State /= Unused then

         case Obj.Entries (Idx).State is
         when Read_Client_Data_Decrypt_And_Supply_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
               Obj.Entries (Idx).State :=
                  Read_Client_Data_Decrypt_And_Supply_Data_In_Progress;
               return;
            end if;
            raise Program_Error;

         when Write_Client_Data_Obtain_And_Encrypt_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then

               Obj.Entries (Idx).State :=
                  Write_Client_Data_Obtain_And_Encrypt_Data_In_Progress;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Drop_Generated_Primitive_New;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      for I in Obj.Entries'Range loop
         if
            Obj.Entries (I).State = Pending and then
            Primitive.Equal (Prim, Obj.Entries (I).Prim)
         then
            Obj.Entries (I).State := In_Progress;
            return;
         end if;
      end loop;
   end Drop_Generated_Primitive;

   --
   --  Drop_Generated_Primitive_2
   --
   procedure Drop_Generated_Primitive_2 (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type)
   is
   begin

      case Obj.Entries (Data_Idx).State is
      when Pending =>

         Obj.Entries (Data_Idx).State := In_Progress;

      when Read_Client_Data_Read_Data_Pending =>

         Obj.Entries (Data_Idx).State :=
            Read_Client_Data_Read_Data_In_Progress;

      when Write_Client_Data_Write_Data_Pending =>

         Obj.Entries (Data_Idx).State :=
            Write_Client_Data_Write_Data_In_Progress;

      when others =>

         raise Program_Error;

      end case;

   end Drop_Generated_Primitive_2;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj      : in out Object_Type;
      Data_Idx :        Data_Index_Type;
      Success  :        Boolean)
   is
   begin

      case Obj.Entries (Data_Idx).State is
      when In_Progress =>

         Primitive.Success (Obj.Entries (Data_Idx).Prim, Success);
         Obj.Entries (Data_Idx).State := Complete;

      when Read_Client_Data_Read_Data_In_Progress =>

         Primitive.Success (Obj.Entries (Data_Idx).Generated_Prim, Success);
         Obj.Entries (Data_Idx).State := Read_Client_Data_Read_Data_Completed;

      when Write_Client_Data_Write_Data_In_Progress =>

         Primitive.Success (Obj.Entries (Data_Idx).Generated_Prim, Success);

         Obj.Entries (Data_Idx).State :=
            Write_Client_Data_Write_Data_Completed;

      when others =>

         raise Program_Error;

      end case;

   end Mark_Generated_Primitive_Complete;

   --
   --  Mark_Generated_Primitive_Complete_New
   --
   procedure Mark_Generated_Primitive_Complete_New (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type)
   is
      Idx : constant Entries_Index_Type :=
         Entries_Index_Type (Primitive.Index (Prim));
   begin
      if Obj.Entries (Idx).State /= Unused then

         case Obj.Entries (Idx).State is
         when Read_Client_Data_Decrypt_And_Supply_Data_In_Progress =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
               Obj.Entries (Idx).Generated_Prim := Prim;
               Obj.Entries (Idx).State :=
                  Read_Client_Data_Decrypt_And_Supply_Data_Completed;
               return;
            end if;
            raise Program_Error;

         when Write_Client_Data_Obtain_And_Encrypt_Data_In_Progress =>

            if Primitive.Equal (Prim, Obj.Entries (Idx).Generated_Prim) then
               Obj.Entries (Idx).Generated_Prim := Prim;
               Obj.Entries (Idx).State :=
                  Write_Client_Data_Obtain_And_Encrypt_Data_Completed;
               return;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Mark_Generated_Primitive_Complete_New;

   --
   --  Execute_Read_Client_Data
   --
   procedure Execute_Read_Client_Data (
      Entr      : in out Entry_Type;
      Entry_Idx :        Entries_Index_Type;
      Progress  : in out Boolean)
   is
   begin

      case Entr.State is
      when Read_Client_Data_Submitted =>

         Entr.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_Blk_IO_Blk_Dev_Read,
            Blk_Nr => Primitive.Block_Number (Entr.Submitted_Prim),
            Idx    => Primitive.Index_Type (Entry_Idx));

         Entr.State := Read_Client_Data_Read_Data_Pending;
         Progress := True;

      when Read_Client_Data_Read_Data_Completed =>

         if not Primitive.Success (Entr.Generated_Prim) then
            raise Program_Error;
         end if;

         Entr.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     =>
               Primitive.Tag_Blk_IO_Crypto_Decrypt_And_Supply_Client_Data,
            Blk_Nr => Primitive.Block_Number (Entr.Submitted_Prim),
            Idx    => Primitive.Index_Type (Entry_Idx));

         Entr.State := Read_Client_Data_Decrypt_And_Supply_Data_Pending;
         Progress := True;

      when Read_Client_Data_Decrypt_And_Supply_Data_Completed =>

         if not Primitive.Success (Entr.Generated_Prim) then
            raise Program_Error;
         end if;

         Primitive.Success (Entr.Submitted_Prim, True);
         Entr.State := Read_Client_Data_Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Read_Client_Data;

   --
   --  Execute_Write_Client_Data
   --
   procedure Execute_Write_Client_Data (
      Entr      : in out Entry_Type;
      Entry_Idx :        Entries_Index_Type;
      Data_Buf  :        Data_Type;
      Progress  : in out Boolean)
   is
   begin

      case Entr.State is
      when Write_Client_Data_Submitted =>

         Entr.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     =>
               Primitive.Tag_Blk_IO_Crypto_Obtain_And_Encrypt_Client_Data,
            Blk_Nr => Primitive.Block_Number (Entr.Submitted_Prim),
            Idx    => Primitive.Index_Type (Entry_Idx));

         Entr.State := Write_Client_Data_Obtain_And_Encrypt_Data_Pending;
         Progress := True;

      when Write_Client_Data_Obtain_And_Encrypt_Data_Completed =>

         if not Primitive.Success (Entr.Generated_Prim) then
            raise Program_Error;
         end if;

         Entr.Hash := Hash_Of_Data_Blk (Data_Buf (Entry_Idx));
         Entr.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Write,
            Succ   => False,
            Tg     => Primitive.Tag_Blk_IO_Blk_Dev_Write,
            Blk_Nr => Primitive.Block_Number (Entr.Submitted_Prim),
            Idx    => Primitive.Index_Type (Entry_Idx));

         Entr.State := Write_Client_Data_Write_Data_Pending;
         Progress := True;

      when Write_Client_Data_Write_Data_Completed =>

         if not Primitive.Success (Entr.Generated_Prim) then
            raise Program_Error;
         end if;

         Primitive.Success (Entr.Submitted_Prim, True);
         Entr.State := Write_Client_Data_Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Write_Client_Data;

end CBE.Block_IO;
