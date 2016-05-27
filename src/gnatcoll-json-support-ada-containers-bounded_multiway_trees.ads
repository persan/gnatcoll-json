with Ada.Containers.Bounded_Multiway_Trees;
generic
   with package T is new Standard.Ada.Containers.Bounded_Multiway_Trees (<>);
   use T;

   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <> with Unreferenced;

   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <> with Unreferenced;
   with procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Element_Type) is <> with Unreferenced;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees is
   pragma Obsolescent ("This is not implemented yet");
   function Create (Val : Tree) return JSON_Value;

   function Get (Val : JSON_Value) return Tree;

   function Get (Val : JSON_Value; Field : UTF8_String) return Tree;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Tree);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
