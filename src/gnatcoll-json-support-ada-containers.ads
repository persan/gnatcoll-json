with Ada.Containers;
package GNATCOLL.JSON.Support.Ada.Containers is
   use Standard.Ada.Containers;

   function Create (Val : Hash_Type) return JSON_Value with Inline_Always;
   function Get (Val : JSON_Value) return Hash_Type with Inline_Always;
   function Get (Val : JSON_Value; Field : UTF8_String) return Hash_Type with Inline_Always;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Hash_Type) with Inline_Always;

   function Create (Val : Count_Type) return JSON_Value with Inline_Always;
   function Get (Val : JSON_Value) return Count_Type with Inline_Always;
   function Get (Val : JSON_Value; Field : UTF8_String) return Count_Type with Inline_Always;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Count_Type) with Inline_Always;

end GNATCOLL.JSON.Support.Ada.Containers;
