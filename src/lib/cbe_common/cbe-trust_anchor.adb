--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.Trust_Anchor
with SPARK_Mode
is
   --
   --  Initialize_Anchor
   --
   procedure Initialize_Anchor (Anchor : out Anchor_Type)
   is
   begin
      Initialize_Each_Job :
      for Idx in Anchor.Jobs'Range loop

         Anchor.Jobs (Idx) := (
            Operation => Invalid,
            State => Job_State_Type'First,
            Submitted_Prim => Primitive.Invalid_Object,
            Key_Value_Plaintext => (others => Byte_Type'First),
            Key_Value_Ciphertext => (others => Byte_Type'First),
            Hash => (others => Byte_Type'First),
            Request => TA_Request.Invalid_Object);

      end loop Initialize_Each_Job;

   end Initialize_Anchor;

   --
   --  Primitive_Acceptable
   --
   function Primitive_Acceptable (Anchor : Anchor_Type)
   return Boolean
   is (for some Job of Anchor.Jobs => Job.Operation = Invalid);

   --
   --  Submit_Primitive
   --
   procedure Submit_Primitive (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_TA_Create_Key
               | Primitive.Tag_SB_Init_TA_Create_Key
            =>

               Anchor.Jobs (Idx).Operation := Create_Key;
               Anchor.Jobs (Idx).State := Submitted;
               Anchor.Jobs (Idx).Submitted_Prim := Prim;
               Anchor.Jobs (Idx).Request := TA_Request.Valid_Object (
                  Op   => TA_Request.Create_Key,
                  Succ => False,
                  Tg   => 0);
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive;

   --
   --  Submit_Primitive_Hash
   --
   procedure Submit_Primitive_Hash (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Hash   :        Hash_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_SB_Ctrl_TA_Secure_SB |
               Primitive.Tag_SB_Init_TA_Secure_SB
            =>

               Anchor.Jobs (Idx).Operation := Secure_Superblock;
               Anchor.Jobs (Idx).State := Submitted;
               Anchor.Jobs (Idx).Submitted_Prim := Prim;
               Anchor.Jobs (Idx).Hash := Hash;
               Anchor.Jobs (Idx).Request := TA_Request.Valid_Object (
                  Op   => TA_Request.Secure_Superblock,
                  Succ => False,
                  Tg   => 0);
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive_Hash;

   --
   --  Submit_Primitive_Key_Value_Plaintext
   --
   procedure Submit_Primitive_Key_Value_Plaintext (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Key    :        Key_Value_Plaintext_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when
               Primitive.Tag_SB_Ctrl_TA_Encrypt_Key |
               Primitive.Tag_SB_Init_TA_Encrypt_Key
            =>

               Anchor.Jobs (Idx).Operation := Encrypt_Key;
               Anchor.Jobs (Idx).State := Submitted;
               Anchor.Jobs (Idx).Submitted_Prim := Prim;
               Anchor.Jobs (Idx).Key_Value_Plaintext := Key;
               Anchor.Jobs (Idx).Request := TA_Request.Valid_Object (
                  Op   => TA_Request.Encrypt_Key,
                  Succ => False,
                  Tg   => 0);
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive_Key_Value_Plaintext;

   --
   --  Submit_Primitive_Key_Value_Ciphertext
   --
   procedure Submit_Primitive_Key_Value_Ciphertext (
      Anchor : in out Anchor_Type;
      Prim   :        Primitive.Object_Type;
      Key    :        Key_Value_Ciphertext_Type)
   is
   begin
      Find_Invalid_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation = Invalid then
            case Primitive.Tag (Prim) is
            when Primitive.Tag_SB_Ctrl_TA_Decrypt_Key =>

               Anchor.Jobs (Idx).Operation := Decrypt_Key;
               Anchor.Jobs (Idx).State := Submitted;
               Anchor.Jobs (Idx).Submitted_Prim := Prim;
               Anchor.Jobs (Idx).Key_Value_Ciphertext := Key;
               Anchor.Jobs (Idx).Request := TA_Request.Valid_Object (
                  Op   => TA_Request.Decrypt_Key,
                  Succ => False,
                  Tg   => 0);
               return;

            when others =>

               raise Program_Error;

            end case;
         end if;
      end loop Find_Invalid_Job;

      raise Program_Error;
   end Submit_Primitive_Key_Value_Ciphertext;

   --
   --  Peek_Completed_Primitive
   --
   function Peek_Completed_Primitive (Anchor : Anchor_Type)
   return Primitive.Object_Type
   is
   begin
      Find_Completed_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Completed
         then
            return Anchor.Jobs (Idx).Submitted_Prim;
         end if;
      end loop Find_Completed_Job;
      return Primitive.Invalid_Object;
   end Peek_Completed_Primitive;

   --
   --  Peek_Completed_Key_Value_Plaintext
   --
   function Peek_Completed_Key_Value_Plaintext (
      Anchor : Anchor_Type;
      Prim   : Primitive.Object_Type)
   return Key_Value_Plaintext_Type
   is
   begin
      Find_Corresponding_Job :
      for Idx in Anchor.Jobs'Range loop

         case Anchor.Jobs (Idx).Operation is
         when Create_Key | Decrypt_Key =>

            if Anchor.Jobs (Idx).State = Completed and then
               Primitive.Equal (Prim, Anchor.Jobs (Idx).Submitted_Prim)
            then
               return Anchor.Jobs (Idx).Key_Value_Plaintext;
            end if;

         when others =>

            raise Program_Error;

         end case;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Peek_Completed_Key_Value_Plaintext;

   --
   --  Peek_Completed_Key_Value_Ciphertext
   --
   function Peek_Completed_Key_Value_Ciphertext (
      Anchor : Anchor_Type;
      Prim   : Primitive.Object_Type)
   return Key_Value_Ciphertext_Type
   is
   begin
      Find_Corresponding_Job :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation = Encrypt_Key and then
            Anchor.Jobs (Idx).State = Completed and then
            Primitive.Equal (Prim, Anchor.Jobs (Idx).Submitted_Prim)
         then
            return Anchor.Jobs (Idx).Key_Value_Ciphertext;
         end if;

      end loop Find_Corresponding_Job;
      raise Program_Error;

   end Peek_Completed_Key_Value_Ciphertext;

   --
   --  Drop_Completed_Primitive
   --
   procedure Drop_Completed_Primitive (
      Anchor : in out Anchor_Type;
      Prim :        Primitive.Object_Type)
   is
   begin
      Find_Corresponding_Job :
      for Idx in Anchor.Jobs'Range loop
         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Completed and then
            Primitive.Equal (Prim, Anchor.Jobs (Idx).Submitted_Prim)
         then
            Anchor.Jobs (Idx).Operation := Invalid;
            return;
         end if;
      end loop Find_Corresponding_Job;
      raise Program_Error;
   end Drop_Completed_Primitive;

   --
   --  Peek generated request
   --
   procedure Peek_Generated_Request (
      Anchor :     Anchor_Type;
      Req    : out TA_Request.Object_Type)
   is
   begin

      Find_Pending_Generated_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Submitted
         then
            Req := Anchor.Jobs (Idx).Request;
            return;
         end if;
      end loop Find_Pending_Generated_Request;

      Req := TA_Request.Invalid_Object;

   end Peek_Generated_Request;

   --
   --  Drop generated request
   --
   procedure Drop_Generated_Request (
      Anchor : in out Anchor_Type;
      Req    :        TA_Request.Object_Type)
   is
   begin
      Find_Dropped_Generated_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Submitted and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Anchor.Jobs (Idx).State := Dropped;
            return;
         end if;
      end loop Find_Dropped_Generated_Request;

      raise Program_Error;

   end Drop_Generated_Request;

   --
   --  Peek generated superblock hash
   --
   procedure Peek_Generated_SB_Hash (
      Anchor :     Anchor_Type;
      Req    :     TA_Request.Object_Type;
      Hash   : out Hash_Type)
   is
   begin

      Find_Hash_Generated_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Submitted and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Hash := Anchor.Jobs (Idx).Hash;
            return;
         end if;
      end loop Find_Hash_Generated_Request;

      raise Program_Error;

   end Peek_Generated_SB_Hash;

   --
   --  Peek generated key value ciphertext
   --
   procedure Peek_Generated_Key_Value_Ciphertext (
      Anchor    :     Anchor_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Ciphertext_Type)
   is
   begin
      Find_Ciphertext_Generated_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Submitted and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Key_Value := Anchor.Jobs (Idx).Key_Value_Ciphertext;
            return;
         end if;
      end loop Find_Ciphertext_Generated_Request;

      raise Program_Error;

   end Peek_Generated_Key_Value_Ciphertext;

   --
   --  Peek generated key value plaintext
   --
   procedure Peek_Generated_Key_Value_Plaintext (
      Anchor    :     Anchor_Type;
      Req       :     TA_Request.Object_Type;
      Key_Value : out Key_Value_Plaintext_Type)
   is
   begin
      Find_Plaintext_Generated_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation /= Invalid and then
            Anchor.Jobs (Idx).State = Submitted and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Key_Value := Anchor.Jobs (Idx).Key_Value_Plaintext;
            return;
         end if;
      end loop Find_Plaintext_Generated_Request;

      raise Program_Error;

   end Peek_Generated_Key_Value_Plaintext;

   --
   --  Mark generated TA create key request complete
   --
   procedure Mark_Generated_Create_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
   begin
      Find_Dropped_Generated_Create_Key_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation = Create_Key and then
            Anchor.Jobs (Idx).State = Dropped and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Anchor.Jobs (Idx).Key_Value_Plaintext := Key_Value;
            Anchor.Jobs (Idx).State := Completed;
            Primitive.Success (Anchor.Jobs (Idx).Submitted_Prim,
               TA_Request.Success (Req));
            return;
         end if;
      end loop Find_Dropped_Generated_Create_Key_Request;

      raise Program_Error;

   end Mark_Generated_Create_Key_Request_Complete;

   --
   --  Mark generated TA secure superblock request complete
   --
   procedure Mark_Generated_Secure_SB_Request_Complete (
      Anchor : in out Anchor_Type;
      Req    :        TA_Request.Object_Type)
   is
   begin
      Find_Dropped_Generated_Secure_SB_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation = Secure_Superblock and then
            Anchor.Jobs (Idx).State = Dropped and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Anchor.Jobs (Idx).State := Completed;
            Primitive.Success (Anchor.Jobs (Idx).Submitted_Prim,
               TA_Request.Success (Req));
            return;
         end if;
      end loop Find_Dropped_Generated_Secure_SB_Request;

      raise Program_Error;

   end Mark_Generated_Secure_SB_Request_Complete;

   --
   --  Mark generated TA decrypt key request complete
   --
   procedure Mark_Generated_Decrypt_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Plaintext_Type)
   is
   begin
      Find_Dropped_Generated_Decrypt_Key_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation = Decrypt_Key and then
            Anchor.Jobs (Idx).State = Dropped and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Anchor.Jobs (Idx).Key_Value_Plaintext := Key_Value;
            Anchor.Jobs (Idx).State := Completed;
            Primitive.Success (Anchor.Jobs (Idx).Submitted_Prim,
               TA_Request.Success (Req));
            return;
         end if;
      end loop Find_Dropped_Generated_Decrypt_Key_Request;

      raise Program_Error;

   end Mark_Generated_Decrypt_Key_Request_Complete;

   --
   --  Mark generated TA encrypt key request complete
   --
   procedure Mark_Generated_Encrypt_Key_Request_Complete (
      Anchor    : in out Anchor_Type;
      Req       :        TA_Request.Object_Type;
      Key_Value :        Key_Value_Ciphertext_Type)
   is
   begin
      Find_Dropped_Generated_Encrypt_Key_Request :
      for Idx in Anchor.Jobs'Range loop

         if Anchor.Jobs (Idx).Operation = Encrypt_Key and then
            Anchor.Jobs (Idx).State = Dropped and then
            TA_Request.Equal (Anchor.Jobs (Idx).Request, Req)
         then
            Anchor.Jobs (Idx).Key_Value_Ciphertext := Key_Value;
            Anchor.Jobs (Idx).State := Completed;
            Primitive.Success (Anchor.Jobs (Idx).Submitted_Prim,
               TA_Request.Success (Req));
            return;
         end if;
      end loop Find_Dropped_Generated_Encrypt_Key_Request;

      raise Program_Error;

   end Mark_Generated_Encrypt_Key_Request_Complete;

end CBE.Trust_Anchor;
