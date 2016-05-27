with AUnit.Test_Cases;
with Ada.Containers.Ordered_Sets;
with GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function Create (Val : Element_Type) return JSON_Value is <>;
   with function Get (Val : JSON_Value) return Element_Type is <>;

   with package S is new Standard.Ada.Containers.Ordered_Sets
     (Element_Type, "=" => "=", "<" => "<");

   with package JSON is new   GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets
     (Element_Type  => Element_Type,
      "="           => "=",
      "<"           => "<",
      S             => S,
      Create        => Create,
      Get           => Get);
   with procedure Initialize (Data : in out S.Set);
package GNATCOLL.JSON.Support.Test.Ada.Containers.Ordered_Sets is
   type Test_Case is new AUnit.Test_Cases.Test_Case with  record
      Test_Data : S.Set;
      Result    : S.Set;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end  GNATCOLL.JSON.Support.Test.Ada.Containers.Ordered_Sets;