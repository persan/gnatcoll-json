
with Ada.Containers.Bounded_Doubly_Linked_Lists;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Bounded_Doubly_Linked_Lists (Element_Type,
                                                                              "=");

   use V;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <> with unreferenced;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <> with unreferenced;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists is
   function Create (Val : List) return JSON_Value;
   function Get (Val : JSON_Value) return List;

   function Get (Val : JSON_Value; Field : UTF8_String) return List;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : List);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists;
