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

package body GNATCOLL.JSON.Support.Ada.Containers is
   ------------
   -- Create --
   ------------

   function Create (Val : Hash_Type) return JSON_Value is
   begin
      return Create (Long_Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Hash_Type is
   begin
      return Hash_Type (Long_Long_Integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Hash_Type
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
      Field      : Hash_Type)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create (Val : Count_Type) return JSON_Value is
   begin
      return Create (Long_Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Count_Type is
   begin
      return Count_Type (Long_Long_Integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Count_Type
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
      Field      : Count_Type)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers;
