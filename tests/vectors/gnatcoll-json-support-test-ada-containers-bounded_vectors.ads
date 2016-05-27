with AUnit.Test_Cases;
with Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with package V is new Standard.Ada.Containers.Bounded_Vectors (Index_Type, Element_Type, "=" => "=");
   with package JSON is new   GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors (V);
   with function Initialize return V.Vector;
package GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors is
   type Test_Case is new AUnit.Test_Cases.Test_Case with  record
      Test_Data : access V.Vector;
      Result    : access V.Vector;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end  GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors;
