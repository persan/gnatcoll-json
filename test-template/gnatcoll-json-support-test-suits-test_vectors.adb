with GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON_Tests;
package body GNATCOLL.JSON.Support.Test.Suits.Test_@_NAME_@ is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON_Tests.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.Test_@_NAME_@;
