with Aunit.Test_Cases;
generic
   with procedure Initialize (Data : in out Vector);
package GNATCOLL.JSON.Support.Ada.Containers.Vectors.Test is
   type Test_Case is new Aunit.Test_Cases.Test_Case with  record
      Test_Data : Vector;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end  GNATCOLL.JSON.Support.Ada.Containers.Vectors.Test;
