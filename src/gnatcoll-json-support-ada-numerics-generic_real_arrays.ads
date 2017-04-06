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

with Ada.Numerics.Generic_Real_Arrays;
generic
   with package A is new Standard.Ada.Numerics.Generic_Real_Arrays (<>);
   use A;
package GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays is

   function Create (Val : Real_Vector) return JSON_Array with Inline_Always;

   function Get (Val : JSON_Value) return Real_Vector with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Real_Vector with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Vector) with Inline_Always;



   function Create (Val : Real_Matrix) return JSON_Array with Inline_Always;

   function Get (Val : JSON_Value) return Real_Matrix with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Real_Matrix with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Matrix) with Inline_Always;

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays;
