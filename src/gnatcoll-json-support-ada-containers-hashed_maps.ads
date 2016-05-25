
with Ada.Containers.Hashed_Maps;
generic
   type Key_Type is private;
   type Element_Type is private;

   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Hashed_Maps
     (Key_Type,
      Element_Type,
      Hash,
      Equivalent_Keys,
      "=");
   use V;

   with function Create (Val : Key_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Key_Type is <> with unreferenced;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Key_Type is <>;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Key_Type) is <> with unreferenced;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <> with unreferenced;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <>;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <> with unreferenced;

package GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps is
   function Create (Val : Map) return JSON_Value;
   function Get (Val : JSON_Value) return Map;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map);

end GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps;
