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
with Ada.Containers;
with Ada.Strings.Fixed;
package GNATCOLL.JSON.Support.Test.Utilities is
   use Ada.Containers;
   procedure Clean;

   procedure Write (Path : String; Item : String);
   function Read (Path : String) return String;

   function Ada2file (Item : String) return String;

   function Ada2file_Simple (Ada_Name : String) return String;
   --  Converts the Ada_Name to a filename without suffix.
   -----------------------------------------------------------------------------

   function Hash (Element : Integer) return Hash_Type is (Hash_Type (Element));

   function Image (Item : String) return String is (Item);
   function Value (Item : String) return String is (Item);

   function Image (Item : Integer) return String is (Ada.Strings.Fixed.Trim (Item'Img, Ada.Strings.Both));
   function Value (Item : String) return Integer is (Integer'Value (Item));

   function Read_Json_Value (From_Path : String) return JSON_Value;

end GNATCOLL.JSON.Support.Test.Utilities;
