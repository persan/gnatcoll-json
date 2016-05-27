generic
   type Enum is (<>);
   Prefix : String := "";
   Suffix : String := "";
package GNATCOLL.JSON.Support.Enum_Generic is

   function Create (Val : Enum) return JSON_Value;
   function Get (Val : JSON_Value) return Enum;
   function Get (Val : JSON_Value; Field : UTF8_String) return Enum;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Enum);

end GNATCOLL.JSON.Support.Enum_Generic;
