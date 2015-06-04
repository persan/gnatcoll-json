with Ada.Containers.Bounded_Vectors;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Bounded_Vectors (Index_Type,
                                                          Element_Type, "=");
   use V;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <>;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors is
   function Create (Val : Vector) return JSON_Value;

   function Get (Val : JSON_Value) return Vector;

   function Get (Val : JSON_Value; Field : UTF8_String) return Vector;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Vector);

end;
