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
--  Stores Data as a nested JSON_Object:
--  { "Capacity" => Capacity,
--    "Data"     => [{"Element": Element,
--                    "Children" => [{"Element":Element,
--                                    "Children" => [...], ...]},
--                   {"Element": Element, "Children" => [...]},
--                  ...]}
------------------------------------------------------------------------------
with Ada.Containers.Bounded_Multiway_Trees;
generic
   with package T is new Standard.Ada.Containers.Bounded_Multiway_Trees (<>);
   use T;

   with function Create (Val : T.Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return T.Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees is

   function Create (Val : T.Tree) return JSON_Value;

   function Get (Val : JSON_Value) return T.Tree with Obsolescent => "Not implemented yet";

   function Get (Val : JSON_Value; Field : UTF8_String) return T.Tree with Obsolescent => "Not implemented yet";

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : T.Tree);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
