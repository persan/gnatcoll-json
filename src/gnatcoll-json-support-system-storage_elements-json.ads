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

with GNATCOLL.JSON; use GNATCOLL.JSON;
with System.Storage_Elements; use System.Storage_Elements;
package GNATCOLL.JSON.Support.System.Storage_Elements.JSON is
   function Create (Val : Storage_Offset) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Offset;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Offset;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Offset);

   function Create (Val : Storage_Element) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Element;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Element;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Element);

   function Create (Val : Storage_Array) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Array;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Array;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Array);

private
   function Image (Item : Storage_Element) return String  with
     Inline_Always => True;
   function Image (Item : Storage_Array) return String  with
     Inline_Always => True;
   function Image (Base   : Standard.System.Address;
                   Length : Storage_Offset) return String
     with Inline_Always => True;
   function Value (Item : String) return Storage_Element  with
     Inline_Always => True;

   function Value (Item : String) return Storage_Array  with
     Inline_Always => True;

end GNATCOLL.JSON.Support.System.Storage_Elements.JSON;
