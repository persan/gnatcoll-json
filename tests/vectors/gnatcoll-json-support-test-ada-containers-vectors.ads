with AUnit.Test_Cases;
with Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Ada.Containers.Vectors;
generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with package V is new Standard.Ada.Containers.Vectors (Index_Type, Element_Type, "=" => "=");

   with package JSON is new   GNATCOLL.JSON.Support.Ada.Containers.Vectors
     (Index_Type      => Index_Type,
      Element_Type    => Element_Type,
      V               => V,
      Create          => Create,
      Get             => Get);
   with procedure Initialize (Data : in out V.Vector);
package GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors is
   type Test_Case is new AUnit.Test_Cases.Test_Case with  record
      Test_Data : V.Vector;
      Result    : V.Vector;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end  GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors;
