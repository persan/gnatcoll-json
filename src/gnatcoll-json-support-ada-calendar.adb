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

package body GNATCOLL.JSON.Support.Ada.Calendar is

   ------------
   -- Create --
   ------------

   function Create (Val : Time) return JSON_Value is
   begin
      return Create (To_Internal_Time (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Time is
   begin
      return To_Time (Get (Val));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Time
   is
   begin
      return To_Time (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Time)
   is
   begin
      Set_Field (Val, Field_Name, To_Internal_Time (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create
     (Val : Internal_Time)
      return JSON_Value
   is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "Year", Val.Year);
         Set_Field (Ret, "Month", Val.Month);
         Set_Field (Ret, "Day",  Val.Day);
         Set_Field (Ret, "Seconds", Float (Val.Seconds));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Internal_Time is
   begin
      return Ret : Internal_Time do
         Ret.Year := Get (Val, "Year");
         Ret.Month := Get (Val, "Month");
         Ret.Day := Get (Val, "Day");
         Ret.Seconds := Duration (Float'(Get (Val, "Seconds")));
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Internal_Time
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Internal_Time)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   -------------
   -- To_Time --
   -------------

   function To_Time (Src : Internal_Time) return Time is
   begin
      return Time_Of (Src.Year, Src.Month, Src.Day, Src.Seconds);
   end To_Time;

   ----------------------
   -- To_Internal_Time --
   ----------------------

   function To_Internal_Time (Src : Time) return Internal_Time is
   begin
      return Ret : Internal_Time do
         Split (Date => Src, Year => Ret.Year, Month => Ret.Month, Day => Ret.Day, Seconds => Ret.Seconds);
      end return;
   end To_Internal_Time;

end GNATCOLL.JSON.Support.Ada.Calendar;
