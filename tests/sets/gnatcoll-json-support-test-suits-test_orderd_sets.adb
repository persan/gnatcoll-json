with GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON_Tests;
with GNATCOLL.JSON.Support.Test.Check_Golden;
package body GNATCOLL.JSON.Support.Test.Suits.Test_Orderd_Sets is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON_Tests.Test_Case;
   Golden : aliased GNATCOLL.JSON.Support.Test.Check_Golden.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Golden'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.Test_Orderd_Sets;
