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

package body GNATCOLL.JSON.Support.Enum_Generic is

   ------------
   -- Create --
   ------------

   function Create (Val : Enum) return JSON_Value is
      Img : constant String := Val'Img;
   begin
      return Create (Link_Prefix & Img (Img'First + Code_Prefix'Length .. Img'Last - Code_Suffix'Length) & Link_Suffix);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Enum is
      Img : constant String := Get (Val);
   begin
      return Enum'Value (Code_Prefix & Img (Img'First + Link_Prefix'Length .. Img'Last - Link_Suffix'Length) & Code_Suffix);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Enum is
   begin
      return Enum'Value (Val.Get (Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Enum)
   is
   begin
      Set_Field (Val, Field_Name, Field'Img);
   end Set_Field;

end GNATCOLL.JSON.Support.Enum_Generic;
