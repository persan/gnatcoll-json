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

generic
   type Enum is (<>);
   Link_Prefix : String := ""; -- Prepend to the Enum before sending
   Link_Suffix : String := ""; -- Appended to the Enum before sending
   Code_Prefix : String := ""; -- Prepended to the data before mapping to enum
   Code_Suffix : String := ""; -- Appended to the data before mapping to enum

package GNATCOLL.JSON.Support.Enum_Generic is

   pragma Compile_Time_Error
     (not Enum_Generic'Library_Level,
      "Enum_Generic can only be instantiated at library level");

   function Create (Val : Enum) return JSON_Value;
   function Get (Val : JSON_Value) return Enum;
   function Get (Val : JSON_Value; Field : UTF8_String) return Enum;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Enum);

end GNATCOLL.JSON.Support.Enum_Generic;
