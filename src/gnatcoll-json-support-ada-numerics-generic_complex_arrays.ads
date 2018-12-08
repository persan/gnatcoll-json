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

with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Arrays;

generic
   with package Complex_Arrays is new Standard.Ada.Numerics.Generic_Complex_Arrays (<>);
   with package Real_Arrays_JSON is new GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays (Complex_Arrays.Real_Arrays);
package GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays is
   use Complex_Arrays;
   function Create (Val : Complex_Vector) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Complex_Vector with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Complex_Vector with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Complex_Vector) with Inline_Always;

   function Create (Val : Complex_Matrix) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Complex_Matrix with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Complex_Matrix with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Complex_Matrix) with Inline_Always;

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
