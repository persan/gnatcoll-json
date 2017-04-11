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

------------------------------------------------------------------------------
-- Stores data as a JSON-Array:
-- [Element, Element, ...]
------------------------------------------------------------------------------
with Ada.Containers.Bounded_Doubly_Linked_Lists;
generic
   with package L is new Standard.Ada.Containers.Bounded_Doubly_Linked_Lists (<>);
   use L;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;
package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists is


   function Create (Val : List) return JSON_Array with
     Inline_Always => True;

   function Get (Val : JSON_Value) return List;

   function Get (Val : JSON_Value; Field : UTF8_String) return List;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : List);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists;
