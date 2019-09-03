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
--  This herarchy contains a set of "support" packages for packing and
--  unpacking "Standard Types" to JSON_Values.
--  The folowing methods are provided for each type and the layout of the
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
--  The methods provided for each type is:
--     function Create (Val : TYPE) return JSON_Value;
--     function Get (Val : JSON_Value) return TYPE;
--
--     function Get (Val : JSON_Value; Field : UTF8_String) return TYPE;
--     procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : TYPE);
--------------------------------------------------------------------------------

package GNATCOLL.JSON.Support is
   VERSION      : constant String := "1.3.0";
   VERSION_DATE : constant String := "2018-12-22";
   --  Note The above versions shall be in sync with
   --  gnatcoll-json.gpr  file and the README.md file

   Path_Delimiter          : constant String := ".";
   Start_Indexed_Delimiter : constant String := "(";
   End_Indexed_Delimiter   : constant String := ")";

   function Get_Path (Val : JSON_Value; Path : UTF8_String) return JSON_Value;
   --  Return the JSON_Value on a textual path.
   --
   --  "data.text.values(1).fix" will return a JSON_Object of JSON_Int_Type containing 1
   --  "data.text.active" will return a JSON_Object of JSON_Boolean_Type containing False
   --
   --    { "data"
   --        { "text" :
   --            { "values" :
   --                [ {"fix"  : 1},
   --                  {"fool" : 2}
   --                ],
   --              "numbers"   : [1,2,3,5,6,7,9],
   --              "description" : "fix",
   --              "speed"       : 1.2,
   --              "active"      : False
   --            }
   --        }
   --    }
   --
   --  JSON_Null  will be returned if the Path dont exist.
   -------------------------------------------------------------------

   function Has_Value (Value : JSON_Value; Path : UTF8_String) return Boolean is
     (Get (Value, Path).Kind in JSON_Elementary_Value_Type);

end GNATCOLL.JSON.Support;
