with AUnit.Test_Cases;
with Ada.Containers.Ordered_Sets;
with GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets;
generic
   with package S is new Standard.Ada.Containers.Ordered_Set  (<>);
   use S;
   with package Sets_JSON is new   GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets     (S  => S);
   use J
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
