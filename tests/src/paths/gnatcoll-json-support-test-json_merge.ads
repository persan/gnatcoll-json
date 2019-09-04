with AUnit.Test_Cases;
package GNATCOLL.JSON.Support.Test.JSON_Merge is
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      null;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;
end GNATCOLL.JSON.Support.Test.JSON_Merge;
