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

package body GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays is
   use Complex_Types_JSON;
   use Real_Arrays_JSON;
   ------------
   -- Create --
   ------------

   function Create
     (Val : Complex_Vector)
      return JSON_Value
   is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "re", Create (Re (Val)));
         Set_Field (Ret, "im", Create (Im (Val)));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Complex_Vector is
      Re : constant Real_Arrays.Real_Vector := Get (Get (Val, "re"));
      Im : constant Real_Arrays.Real_Vector := Get (Get (Val, "im"));
   begin
      return Ret : Complex_Vector (Re'Range) do
         Set_Re (Ret, Re);
         Set_Im (Ret, Im);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Complex_Vector
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
      Field      : Complex_Vector)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create
     (Val : Complex_Matrix)
      return JSON_Value
   is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "re", Create (Re (Val)));
         Set_Field (Ret, "im", Create (Im (Val)));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Complex_Matrix is
      Re : constant Real_Arrays.Real_Matrix := Get (Get (Val, "re"));
      Im : constant Real_Arrays.Real_Matrix := Get (Get (Val, "im"));
   begin
      return Ret : Complex_Matrix (Re'Range (1), Re'Range (2)) do
         Set_Re (Ret, Re);
         Set_Im (Ret, Im);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Complex_Matrix
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
      Field      : Complex_Matrix)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
