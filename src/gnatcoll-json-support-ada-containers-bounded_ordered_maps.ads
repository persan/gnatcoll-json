
with Ada.Containers.Bounded_Ordered_Maps;
generic
   with package M is new Standard.Ada.Containers.Bounded_Ordered_Maps (<>);
   use M;
   with function Create (Val : Key_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Key_Type is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps is

   pragma Compile_Time_Error
     (not Bounded_Ordered_Maps'Library_Level,
      "Bounded_Ordered_Maps can only be instantiated at library level");

   function Create (Val : Map) return JSON_Value;
   function Get (Val : JSON_Value) return Map;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map);

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps;
