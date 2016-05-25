with Ada.Containers.Bounded_Multiway_Trees;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   with package V is new Standard.Ada.Containers.Bounded_Multiway_Trees (Element_Type, "=");
   use V;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <> with unreferenced;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <> with unreferenced;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <> with unreferenced;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees is
   pragma Obsolescent ("This is not implemented yet");
   function Create (Val : Tree) return JSON_Value;

   function Get (Val : JSON_Value) return Tree;

   function Get (Val : JSON_Value; Field : UTF8_String) return Tree;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Tree);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
