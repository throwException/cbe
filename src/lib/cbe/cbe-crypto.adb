--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Crypto
with SPARK_Mode
is
   --
   --  Initialized_Object
   --
   function Initialized_Object
   return Object_Type
   is (
      Jobs => (
         others => (
            State => Invalid,
            Prim => Primitive.Invalid_Object,
            Submitted_Prim => Primitive.Invalid_Object,
            Generated_Prim => Primitive.Invalid_Object,
            Req => Request.Invalid_Object,
            VBA => Virtual_Block_Address_Type'First,
            Key => Key_Plaintext_Invalid)));

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Obj : Object_Type)
   return Boolean
   is (
      for some Itm of Obj.Jobs => Itm.State = Invalid);

   --
   --  Submit_Primitive_Key
   --
   procedure Submit_Primitive_Key (
      Obj  : in out Object_Type;
      Prim :        Primitive.Object_Type;
      Key  :        Key_Plaintext_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key := Key;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive_Key;

   --
   --  Submit_Primitive_Key_ID
   --
   procedure Submit_Primitive_Key_ID (
      Obj    : in out Object_Type;
      Prim   :        Primitive.Object_Type;
      Key_ID :        Key_ID_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive_Key_ID;

   --
   --  Submit_Primitive_Client_Data
   --
   procedure Submit_Primitive_Client_Data (
      Obj            : in out Object_Type;
      Prim           :        Primitive.Object_Type;
      Req            :        Request.Object_Type;
      VBA            :        Virtual_Block_Address_Type;
      Key_ID         :        Key_ID_Type;
      Cipher_Buf_Idx :    out Cipher_Buffer_Index_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            case Primitive.Tag (Prim) is
            when Primitive.Tag_Blk_IO_Crypto_Decrypt_And_Supply_Client_Data =>

               Obj.Jobs (Job_Idx).State := DSCD_Submitted;
               Obj.Jobs (Job_Idx).Submitted_Prim := Prim;
               Obj.Jobs (Job_Idx).Req := Req;
               Obj.Jobs (Job_Idx).VBA := VBA;
               Obj.Jobs (Job_Idx).Key.ID := Key_ID;

               Cipher_Buf_Idx := Cipher_Buffer_Index_Type (Job_Idx);
               return;

            when others =>

               raise Program_Error;

            end case;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive_Client_Data;

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Pending;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;

            Data_Idx := Job_Idx;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Primitive;

   --
   --  Submit_Completed_Primitive
   --
   procedure Submit_Completed_Primitive (
      Obj      : in out Object_Type;
      Prim     :        Primitive.Object_Type;
      Key_ID   :        Key_ID_Type;
      Data_Idx :    out Jobs_Index_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Invalid then

            Obj.Jobs (Job_Idx).State := Complete;
            Obj.Jobs (Job_Idx).Prim := Prim;
            Obj.Jobs (Job_Idx).Key.ID := Key_ID;

            Data_Idx := Job_Idx;
            return;

         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Submit_Completed_Primitive;

   --
   --  Peek_Generated_Primitive
   --
   procedure Peek_Generated_Primitive (
      Obj     :     Object_Type;
      Job_Idx : out Jobs_Index_Type;
      Prim    : out Primitive.Object_Type)
   is
   begin

      For_Each_Job :
      for Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Idx).State = Pending then

            Prim := Obj.Jobs (Idx).Prim;
            Job_Idx := Idx;
            return;

         end if;

      end loop For_Each_Job;

      Prim := Primitive.Invalid_Object;
      Job_Idx := Jobs_Index_Type'First;

   end Peek_Generated_Primitive;

   --
   --  Peek_Generated_Crypto_Dev_Primitive
   --
   function Peek_Generated_Crypto_Dev_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      For_Each_Job :
      for Idx in Obj.Jobs'Range loop

         case Obj.Jobs (Idx).State is
         when Pending =>

            return Obj.Jobs (Idx).Prim;

         when DSCD_Decrypt_Data_Pending =>

            return Obj.Jobs (Idx).Generated_Prim;

         when others =>

            null;

         end case;

      end loop For_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Crypto_Dev_Primitive;

   --
   --  Peek_Generated_Client_Primitive
   --
   function Peek_Generated_Client_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      For_Each_Job :
      for Idx in Obj.Jobs'Range loop

         case Obj.Jobs (Idx).State is
         when DSCD_Supply_Data_Pending =>

            return Obj.Jobs (Idx).Generated_Prim;

         when others =>

            null;

         end case;

      end loop For_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Generated_Client_Primitive;

   --
   --  Peek_Generated_Cipher_Buf_Idx
   --
   function Peek_Generated_Cipher_Buf_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Cipher_Buffer_Index_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Obj.Jobs (Idx).State /= Invalid then

         case Obj.Jobs (Idx).State is
         when DSCD_Decrypt_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Jobs (Idx).Generated_Prim) then
               return Cipher_Buffer_Index_Type (Idx);
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Cipher_Buf_Idx;

   --
   --  Peek_Generated_Plain_Buf_Idx
   --
   function Peek_Generated_Plain_Buf_Idx (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Plain_Buffer_Index_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Obj.Jobs (Idx).State /= Invalid then

         case Obj.Jobs (Idx).State is
         when DSCD_Supply_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Jobs (Idx).Generated_Prim) then
               return Plain_Buffer_Index_Type (Idx);
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Plain_Buf_Idx;

   --
   --  Peek_Generated_Key_ID_New
   --
   function Peek_Generated_Key_ID_New (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Key_ID_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Obj.Jobs (Idx).State /= Invalid then

         case Obj.Jobs (Idx).State is
         when DSCD_Decrypt_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Jobs (Idx).Generated_Prim) then
               return Obj.Jobs (Idx).Key.ID;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Key_ID_New;

   --
   --  Peek_Generated_Req
   --
   function Peek_Generated_Req (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Request.Object_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Obj.Jobs (Idx).State /= Invalid then

         case Obj.Jobs (Idx).State is
         when DSCD_Supply_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Jobs (Idx).Generated_Prim) then
               return Obj.Jobs (Idx).Req;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_Req;

   --
   --  Peek_Generated_VBA
   --
   function Peek_Generated_VBA (
      Obj  : Object_Type;
      Prim : Primitive.Object_Type)
   return Virtual_Block_Address_Type
   is
      Idx : constant Jobs_Index_Type :=
         Jobs_Index_Type (Primitive.Index (Prim));
   begin

      if Obj.Jobs (Idx).State /= Invalid then

         case Obj.Jobs (Idx).State is
         when DSCD_Supply_Data_Pending =>

            if Primitive.Equal (Prim, Obj.Jobs (Idx).Generated_Prim) then
               return Obj.Jobs (Idx).VBA;
            end if;
            raise Program_Error;

         when others =>

            raise Program_Error;

         end case;

      end if;
      raise Program_Error;

   end Peek_Generated_VBA;

   --
   --  Execute
   --
   procedure Execute (
      Obj      : in out Object_Type;
      Progress : in out Boolean)
   is
   begin

      Execute_Each_Valid_Job :
      for Idx in Obj.Jobs'Range loop

         case Obj.Jobs (Idx).State is
         when
            DSCD_Submitted |
            DSCD_Completed |
            DSCD_Decrypt_Data_Pending |
            DSCD_Decrypt_Data_In_Progress |
            DSCD_Decrypt_Data_Completed |
            DSCD_Supply_Data_Pending |
            DSCD_Supply_Data_In_Progress |
            DSCD_Supply_Data_Completed
         =>

            Execute_Decrypt_And_Supply_Client_Data (
               Obj.Jobs (Idx), Idx, Progress);

         when others =>

            null;

         end case;

      end loop Execute_Each_Valid_Job;

   end Execute;

   --
   --  Execute_Decrypt_And_Supply_Client_Data
   --
   procedure Execute_Decrypt_And_Supply_Client_Data (
      Job      : in out Job_Type;
      Job_Idx  :        Jobs_Index_Type;
      Progress : in out Boolean)
   is
   begin

      case Job.State is
      when DSCD_Submitted =>

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_Crypto_IO_Crypto_Dev_Decrypt,
            Blk_Nr => Primitive.Block_Number (Job.Submitted_Prim),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := DSCD_Decrypt_Data_Pending;
         Progress := True;

      when DSCD_Decrypt_Data_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Job.Generated_Prim := Primitive.Valid_Object_No_Pool_Idx (
            Op     => Read,
            Succ   => False,
            Tg     => Primitive.Tag_Crypto_IO_Client_Supply_Data,
            Blk_Nr => Primitive.Block_Number (Job.Submitted_Prim),
            Idx    => Primitive.Index_Type (Job_Idx));

         Job.State := DSCD_Supply_Data_Pending;
         Progress := True;

      when DSCD_Supply_Data_Completed =>

         if not Primitive.Success (Job.Generated_Prim) then
            raise Program_Error;
         end if;

         Primitive.Success (Job.Submitted_Prim, True);
         Job.State := DSCD_Completed;
         Progress := True;

      when others =>

         null;

      end case;

   end Execute_Decrypt_And_Supply_Client_Data;

   --
   --  Drop_Generated_Primitive
   --
   procedure Drop_Generated_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type)
   is
   begin

      case Obj.Jobs (Job_Idx).State is
      when Pending =>

         Obj.Jobs (Job_Idx).State := In_Progress;

      when others =>

         raise Program_Error;

      end case;

   end Drop_Generated_Primitive;

   --
   --  Drop_Generated_Primitive_New
   --
   procedure Drop_Generated_Primitive_New (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type)
   is
   begin

      case Obj.Jobs (Job_Idx).State is
      when Pending =>

         Obj.Jobs (Job_Idx).State := In_Progress;

      when DSCD_Decrypt_Data_Pending =>

         Obj.Jobs (Job_Idx).State := DSCD_Decrypt_Data_In_Progress;

      when DSCD_Supply_Data_Pending =>

         Obj.Jobs (Job_Idx).State := DSCD_Supply_Data_In_Progress;

      when others =>

         raise Program_Error;

      end case;

   end Drop_Generated_Primitive_New;

   --
   --  Peek_Generated_Key_ID
   --
   function Peek_Generated_Key_ID (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_ID_Type
   is
   begin

      if Obj.Jobs (Job_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Jobs (Job_Idx).Key.ID;

   end Peek_Generated_Key_ID;

   --
   --  Peek_Generated_Key
   --
   function Peek_Generated_Key (
      Obj     : Object_Type;
      Job_Idx : Jobs_Index_Type)
   return Key_Plaintext_Type
   is
   begin

      if Obj.Jobs (Job_Idx).State /= Pending then
         raise Program_Error;
      end if;
      return Obj.Jobs (Job_Idx).Key;

   end Peek_Generated_Key;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Obj : Object_Type)
   return Primitive.Object_Type
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         case Obj.Jobs (Job_Idx).State is
         when Complete =>

            return Obj.Jobs (Job_Idx).Prim;

         when DSCD_Completed =>

            return Obj.Jobs (Job_Idx).Submitted_Prim;

         when others =>

            null;

         end case;

      end loop For_Each_Job;
      return Primitive.Invalid_Object;

   end Peek_Completed_Primitive;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (Obj : in out Object_Type)
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Obj.Jobs (Job_Idx).State = Complete then
            Obj.Jobs (Job_Idx).State := Invalid;
            return;
         end if;

      end loop For_Each_Job;

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
      for Idx in Obj.Jobs'Range loop
         if Obj.Jobs (Idx).State = DSCD_Completed and then
            Primitive.Equal (Prim, Obj.Jobs (Idx).Submitted_Prim)
         then
            Obj.Jobs (Idx).State := Invalid;
            return;
         end if;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Drop_Completed_Primitive_New;

   --
   --  Mark_Completed_Primitive
   --
   procedure Mark_Completed_Primitive (
      Obj     : in out Object_Type;
      Job_Idx :        Jobs_Index_Type;
      Success :        Boolean)
   is
   begin

      case Obj.Jobs (Job_Idx).State is
      when In_Progress =>

         Obj.Jobs (Job_Idx).State := Complete;
         Primitive.Success (Obj.Jobs (Job_Idx).Prim, Success);

      when DSCD_Decrypt_Data_In_Progress =>

         Obj.Jobs (Job_Idx).State := DSCD_Decrypt_Data_Completed;
         Primitive.Success (Obj.Jobs (Job_Idx).Generated_Prim, Success);

      when others =>

         raise Program_Error;

      end case;

   end Mark_Completed_Primitive;

   --
   --  Mark_Generated_Primitive_Complete
   --
   procedure Mark_Generated_Primitive_Complete (
      Obj           : in out Object_Type;
      Plain_Buf_Idx :        Plain_Buffer_Index_Type;
      Success       :        Boolean)
   is
      Job_Idx : constant Jobs_Index_Type := Jobs_Index_Type (Plain_Buf_Idx);
   begin

      case Obj.Jobs (Job_Idx).State is
      when DSCD_Supply_Data_In_Progress =>

         Obj.Jobs (Job_Idx).State := DSCD_Supply_Data_Completed;
         Primitive.Success (Obj.Jobs (Job_Idx).Generated_Prim, Success);

      when others =>

         raise Program_Error;

      end case;

   end Mark_Generated_Primitive_Complete;

   --
   --  Data_Index
   --
   function Data_Index (
      Obj  : Crypto.Object_Type;
      Prim : Primitive.Object_Type)
   return Jobs_Index_Type
   is
   begin

      For_Each_Job :
      for Job_Idx in Obj.Jobs'Range loop

         if Primitive.Equal (Obj.Jobs (Job_Idx).Prim, Prim) then
            return Job_Idx;
         end if;

      end loop For_Each_Job;
      raise Program_Error;

   end Data_Index;

end CBE.Crypto;
