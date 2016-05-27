with Ada.Containers.Ordered_Maps;
generic
   with package V is new Standard.Ada.Containers.Ordered_Maps (<>);
   use V;

   with function Create (Val : Key_Type) return JSON_Value is <>;
   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Key_Type is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get_Name (Val : JSON_Value; Field : UTF8_String) return Element_Type is <>;

package GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps is
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
