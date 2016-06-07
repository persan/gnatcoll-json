with AUnit.Test_Cases;
with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
generic
   with package Complex_Arrays is new GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays (<>);
package GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests is
   type Test_Case is new AUnit.Test_Cases.Test_Case with  record
      Test_Vector :  Complex_Arrays.Complex_Arrays.Complex_Vector (1 .. 10);
      Test_Matrix : Complex_Arrays.Complex_Arrays.Complex_Matrix (1 .. 10, 1 .. 6);
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests;
