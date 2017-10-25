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

with Ada.Containers;
with GNATCOLL.JSON.Support.Modular_Generic;
with GNATCOLL.JSON.Support.Integer_Generic;
package GNATCOLL.JSON.Support.Ada.Containers is
   use Standard.Ada.Containers;

   package Hash_Type_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Hash_Type);

   function Create (Val : Hash_Type) return JSON_Value renames Hash_Type_Impl.Create;
   function Get (Val : JSON_Value) return Hash_Type renames Hash_Type_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Hash_Type renames Hash_Type_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Hash_Type) renames Hash_Type_Impl.Set_Field;

   package Count_Type_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Count_Type);
   function Create (Val : Count_Type) return JSON_Value renames Count_Type_Impl.Create;
   function Get (Val : JSON_Value) return Count_Type renames Count_Type_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Count_Type renames Count_Type_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Count_Type) renames Count_Type_Impl.Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers;
