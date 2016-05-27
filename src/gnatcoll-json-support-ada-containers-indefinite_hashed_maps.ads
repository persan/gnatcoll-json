
with Ada.Containers.Indefinite_Hashed_Maps;
generic
   with package M is new Standard.Ada.Containers.Indefinite_Hashed_Maps (<>);
   use M;

   with function Create_Key (Val : Key_Type) return JSON_Value is <>;

   with function Get_Name_Key (Val : JSON_Value; Field : UTF8_String) return Key_Type is <>;

   with function Create_Element (Val : Element_Type) return JSON_Value is <>;

   with function Get_Name_Element (Val : JSON_Value; Field : UTF8_String) return Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps is
   function Create (Val : Map) return JSON_Value;
   function Get (Val : JSON_Value) return Map;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map);

end GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps;
