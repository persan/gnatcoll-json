
--------------------------------------------------------------------------------
-- This package provides a simpler format of the JSON_Value
--------------------------------------------------------------------------------
with Ada.Containers.Hashed_Maps;
generic
   with package M is new Standard.Ada.Containers.Hashed_Maps (<>);
   use M;

   with function Image (Val : Key_Type) return String is <>;
   with function Value (Val : String) return  Key_Type is <>;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps_Simple is
   function Create (Val : Map) return JSON_Value;
   function Get (Val : JSON_Value) return Map;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map);

end GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps_Simple;
