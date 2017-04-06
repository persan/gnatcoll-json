with System; use System;
with AUnit.Test_Cases;
package GNATCOLL.JSON.Support.Test.System is
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      S_0 : Address;
      S_1 : Address;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end GNATCOLL.JSON.Support.Test.System;
