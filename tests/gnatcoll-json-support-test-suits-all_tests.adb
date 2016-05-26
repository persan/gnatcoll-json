with GNATCOLL.JSON.Support.Test.Suits.Test_Vectors;
with GNATCOLL.JSON.Support.Test.Suits.Test_Orderd_Sets;
with GNATCOLL.JSON.Support.Test.Check_Golden;
package body GNATCOLL.JSON.Support.Test.Suits.All_Tests is
   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Golden : aliased GNATCOLL.JSON.Support.Test.Check_Golden.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, GNATCOLL.JSON.Support.Test.Suits.Test_Orderd_Sets.Suite);
      Add_Test (Result'Access, GNATCOLL.JSON.Support.Test.Suits.Test_Vectors.Suite);
      Add_Test (Result'Access, Golden'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.All_Tests;
