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

with GNATCOLL.JSON; use GNATCOLL.JSON;
with System.Storage_Elements; use System.Storage_Elements;
with GNATCOLL.JSON.Support.Modular_Generic;
with GNATCOLL.JSON.Support.Simple_Arrays_Generic;
with GNATCOLL.JSON.Support.Integer_Generic;
package GNATCOLL.JSON.Support.System.Storage_Elements is

   package Storage_Offset_Impl is new GNATCOLL.JSON.Support.Integer_Generic (Storage_Offset);

   function Create (Val : Storage_Offset) return JSON_Value renames Storage_Offset_Impl.Create;
   function Get (Val : JSON_Value) return Storage_Offset renames Storage_Offset_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Offset renames Storage_Offset_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Offset) renames Storage_Offset_Impl.Set_Field;



   package Storage_Element_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Storage_Element);

   function Create (Val : Storage_Element) return JSON_Value renames Storage_Element_Impl.Create;

   function Get (Val : JSON_Value) return Storage_Element renames Storage_Element_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Element renames Storage_Element_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Element) renames Storage_Element_Impl.Set_Field;


   package Storage_Array_Impl is new GNATCOLL.JSON.Support.Simple_Arrays_Generic
     (Index_Type   => Storage_Offset,
      Array_Type   => Storage_Array,
      Element_Type => Storage_Element);

   function Create (Val : Storage_Array) return JSON_Array renames Storage_Array_Impl.Create;

   function Get (Val : JSON_Array) return Storage_Array  renames Storage_Array_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Array  renames Storage_Array_Impl.Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Array) renames Storage_Array_Impl.Set_Field;


   package Integer_Address_Impl is new GNATCOLL.JSON.Support.Modular_Generic (Integer_Address);

   function Create (Val : Integer_Address) return JSON_Value renames Integer_Address_Impl.Create;

   function Get (Val : JSON_Value) return Integer_Address renames Integer_Address_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Integer_Address renames Integer_Address_Impl.Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Integer_Address) renames Integer_Address_Impl.Set_Field;


end GNATCOLL.JSON.Support.System.Storage_Elements;
