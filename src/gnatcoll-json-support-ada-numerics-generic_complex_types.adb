------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types is
   use GNATCOLL.JSON;
   ------------
   -- Create --
   ------------

   function Create
     (Val : Complex)
      return JSON_Value
   is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field_Long_Float (Ret, "re", Long_Float (Val.Re));
         Set_Field_Long_Float (Ret, "im", Long_Float (Val.Im));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Complex is
   begin
      return Ret : Complex do
         Ret.Re := Real (Get_Long_Float (Val, "re"));
         Ret.Im := Real (Get_Long_Float (Val, "im"));
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Complex
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Complex)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   -------------
   -- To_Time --
   -------------
end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types;
