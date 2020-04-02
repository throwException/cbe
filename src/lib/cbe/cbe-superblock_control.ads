--
--  Copyright (C) 2019 Genode Labs GmbH, Componolit GmbH, secunet AG
--
--  This file is part of the Consistent Block Encrypter project, which is
--  distributed under the terms of the GNU Affero General Public License
--  version 3.
--

pragma Ada_2012;

package CBE.Superblock_Control
with SPARK_Mode
is
   pragma Pure;

   type Primitive is private;

private:

   type Operation_Type is (Init_Rekey, Secure_SB);

   type Primitive is record
      Opereration : Operation_Type;
      Key         : Key_Type;
   end record;

end CBE.Superblock_Control;
