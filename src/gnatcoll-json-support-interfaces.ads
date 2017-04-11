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


with Interfaces;
with GNATCOLL.JSON.Support.Integer_Generic;
with GNATCOLL.JSON.Support.Modular_Generic;
package GNATCOLL.JSON.Support.Interfaces is
   use Standard.Interfaces;

   package Integer_8_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Integer_8);
   function Create (Val : Integer_8) return JSON_Value renames Integer_8_Impl.Create;
   function Get (Val : JSON_Value) return Integer_8 renames Integer_8_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Integer_8 renames Integer_8_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Integer_8) renames Integer_8_Impl.Set_Field;

   package Integer_16_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Integer_16);
   function Create (Val : Integer_16) return JSON_Value renames Integer_16_Impl.Create;
   function Get (Val : JSON_Value) return Integer_16 renames Integer_16_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Integer_16 renames Integer_16_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Integer_16) renames Integer_16_Impl.Set_Field;

   package Integer_32_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Integer_32);
   function Create (Val : Integer_32) return JSON_Value renames Integer_32_Impl.Create;
   function Get (Val : JSON_Value) return Integer_32 renames Integer_32_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Integer_32 renames Integer_32_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Integer_32) renames Integer_32_Impl.Set_Field;

   package Integer_64_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Integer_64);
   function Create (Val : Integer_64) return JSON_Value renames Integer_64_Impl.Create;
   function Get (Val : JSON_Value) return Integer_64 renames Integer_64_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Integer_64 renames Integer_64_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Integer_64) renames Integer_64_Impl.Set_Field;



   package Unsigned_8_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Unsigned_8);
   function Create (Val : Unsigned_8) return JSON_Value renames Unsigned_8_Impl.Create;
   function Get (Val : JSON_Value) return Unsigned_8 renames Unsigned_8_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Unsigned_8 renames Unsigned_8_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Unsigned_8) renames Unsigned_8_Impl.Set_Field;

   package Unsigned_16_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Unsigned_16);
   function Create (Val : Unsigned_16) return JSON_Value renames Unsigned_16_Impl.Create;
   function Get (Val : JSON_Value) return Unsigned_16 renames Unsigned_16_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Unsigned_16 renames Unsigned_16_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Unsigned_16) renames Unsigned_16_Impl.Set_Field;

   package Unsigned_32_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Unsigned_32);
   function Create (Val : Unsigned_32) return JSON_Value renames Unsigned_32_Impl.Create;
   function Get (Val : JSON_Value) return Unsigned_32 renames Unsigned_32_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Unsigned_32 renames Unsigned_32_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Unsigned_32) renames Unsigned_32_Impl.Set_Field;

   package Unsigned_64_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Unsigned_64);
   function Create (Val : Unsigned_64) return JSON_Value renames Unsigned_64_Impl.Create;
   function Get (Val : JSON_Value) return Unsigned_64 renames Unsigned_64_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Unsigned_64 renames Unsigned_64_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Unsigned_64) renames Unsigned_64_Impl.Set_Field;

end GNATCOLL.JSON.Support.Interfaces;
