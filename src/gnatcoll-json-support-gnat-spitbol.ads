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
with GNAT.Spitbol;
package GNATCOLL.JSON.Support.GNAT.Spitbol is

   generic
      with package V is new Standard.GNAT.Spitbol.Table (<>);
      with function Create (Val : V.Value_Type) return JSON_Value is <>;
      with function Get (Val : JSON_Value) return V.Value_Type is <>;
   package Table is
      function Create (Val : V.Table) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table;
      procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table);

      function Create (Val : V.Table_Entry) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Entry;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table_Entry;
      procedure Set_Field  (Val  : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table_Entry);

      function Create (Val : V.Table_Array) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Array;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table_Array;
      procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table_Array);
   end Table;

end GNATCOLL.JSON.Support.GNAT.Spitbol;
