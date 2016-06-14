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

--------------------------------------------------------------------------------
--
--  This herarchy cotains a set of "support" packages for packing and
--  unpacking "Standard Types" to JSON_Values.
--  The folowing methods are providet for each type and the layout of the
--  JSON objects are kept equal for when its possible hence:
--     Ada.Containers.Vectors,
--     Ada.Containers.Indefinite_Vectors
--     Ada.Containers.Bounded_Vectors
--     Ada.Containers.Orderd_Sets
--     Ada.Containers.Boundeded_Hashed_Sets
--     Ada.Containers.Hashed_Sets
--     Ada.Containers.Bounded_Doubly_Linked_Lists
--     ...
--  Got the same JSON representation.
--
-- The methods provided for each type is:
--     function Create (Val : TYPE) return JSON_Value;
--     function Get (Val : JSON_Value) return TYPE;
--
--     function Get (Val : JSON_Value; Field : UTF8_String) return TYPE;
--     procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : TYPE);
--------------------------------------------------------------------------------

package GNATCOLL.JSON.Support is
   VERSION : constant String := "1.0.1";
   -- Note Tha above version shall be in sync with
   -- gnatcoll-jison.gpr  file and the README.md file
end GNATCOLL.JSON.Support;
