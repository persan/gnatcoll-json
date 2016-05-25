with Ada.Containers.Indefinite_Vectors;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Indefinite_Vectors
     (Index_Type,
      Element_Type,
      "=");
   use V;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with function Get (Val : JSON_Value; Field : UTF8_String) return Element_Type is <> with unreferenced;
   with procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <> with unreferenced;

package GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors is
   function Create (Val : Vector) return JSON_Value;
   function Get (Val : JSON_Value) return Vector;

   function Get (Val : JSON_Value; Field : UTF8_String) return Vector;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Vector);
end GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors;
