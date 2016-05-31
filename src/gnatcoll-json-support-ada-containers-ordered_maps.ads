with Ada.Containers.Ordered_Maps;
generic
   with package V is new Standard.Ada.Containers.Ordered_Maps (<>);
   use V;

   with function Create (Val : Key_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Key_Type is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps is

   pragma Compile_Time_Error
     (not Ordered_Maps'Library_Level,
      "Ordered_Maps can only be instantiated at library level");

   function Create (Val : Map) return JSON_Value;
   function Get (Val : JSON_Value) return Map;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map);
private
   type Map_Entry is record
      Key     : Key_Type;
      Element : Element_Type;
   end record;

   function Create (Val : Map_Entry) return JSON_Value;
   function Get (Val : JSON_Value) return Map_Entry;

end GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps;
