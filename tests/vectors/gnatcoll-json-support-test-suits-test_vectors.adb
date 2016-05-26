with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON_Tests;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON_Tests;
package body GNATCOLL.JSON.Support.Test.Suits.Test_Vectors is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON_Tests.Test_Case;
   Test_2 : aliased GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON_Tests.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.Test_Vectors;
