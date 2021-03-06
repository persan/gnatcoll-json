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

-----------------------------------------------------
--
--  Stores Time as:
--     {"Year" : Year,
--      "Month : Month,
--      "Day"  : Day,
--      "Seconds" : Seconds}
-----------------------------------------------

with Ada.Calendar;
package GNATCOLL.JSON.Support.Ada.Calendar is
   use Standard.Ada.Calendar;

   function Create (Val : Time) return JSON_Value  with
     Inline_Always => True;

   function Get (Val : JSON_Value) return Time;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Time with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Time) with Inline_Always;
private

   type Internal_Time is record
      Year    : Standard.Ada.Calendar.Year_Number;
      Month   : Standard.Ada.Calendar.Month_Number;
      Day     : Standard.Ada.Calendar.Day_Number;
      Seconds : Standard.Ada.Calendar.Day_Duration;
   end record;

   function Create (Val : Internal_Time) return JSON_Value with Inline_Always;
   function Get (Val : JSON_Value) return Internal_Time;
   function Get (Val   : JSON_Value;
                 Field : UTF8_String) return Internal_Time with Inline_Always;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String; Field  : Internal_Time) with Inline_Always;

   function To_Time (Src : Internal_Time) return Time;
   function To_Internal_Time (Src : Time) return Internal_Time;

end GNATCOLL.JSON.Support.Ada.Calendar;
