
with Ada.Containers.Bounded_Hashed_Sets;
generic
   type Element_Type is private;
   with function Hash (Element : Element_Type) return Hash_Type;

   with function Equivalent_Elements
     (Left, Right : Element_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Bounded_Hashed_Sets (Element_Type, Hash,
                                                                      Equivalent_Elements, "=");
   use V;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <>;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Sets is
   function Create (Val : Set) return JSON_Value;
   function Get (Val : JSON_Value) return Set;

   function Get (Val : JSON_Value; Field : UTF8_String) return Set;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Set);
end;
