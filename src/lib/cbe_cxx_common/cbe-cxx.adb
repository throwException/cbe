--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package body CBE.CXX
with SPARK_Mode
is
   --
   --  CXX_Bool_To_SPARK
   --
   function CXX_Bool_To_SPARK (Input : CXX_Bool_Type)
   return Boolean
   is
   begin
      case Input is
      when 0 => return False;
      when 1 => return True;
      when others => raise Program_Error;
      end case;
   end CXX_Bool_To_SPARK;

   --
   --  CXX_Success_To_SPARK
   --
   function CXX_Success_To_SPARK (Input : CXX_Success_Type)
   return Request.Success_Type
   is
   begin
      case Input is
      when 0 => return False;
      when 1 => return True;
      when others => raise Program_Error;
      end case;
   end CXX_Success_To_SPARK;

   --
   --  CXX_Request_To_SPARK
   --
   function CXX_Request_To_SPARK (Input : CXX_Request_Type)
   return Request.Object_Type
   is
   begin
      case Input.Operation is
      when 0 => return Request.Invalid_Object;
      when 1 => return CXX_Request_Valid_To_SPARK (Input, Read);
      when 2 => return CXX_Request_Valid_To_SPARK (Input, Write);
      when 3 => return CXX_Request_Valid_To_SPARK (Input, Sync);
      when 4 => return CXX_Request_Valid_To_SPARK (Input, Create_Snapshot);
      when 5 => return CXX_Request_Valid_To_SPARK (Input, Discard_Snapshot);
      when others => raise Program_Error;
      end case;
   end CXX_Request_To_SPARK;

end CBE.CXX;
