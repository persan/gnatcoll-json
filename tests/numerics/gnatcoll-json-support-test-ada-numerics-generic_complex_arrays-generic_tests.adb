with AUnit; use AUnit;
with GNAT.Source_Info;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with  GNATCOLL.JSON.Support.Test.Utilities;
package body GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests is
   use Complex_Arrays.Complex_Arrays;
   use GNATCOLL.JSON.Support.Test.Utilities;
   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      for I in Test.Test_Vector'Range loop
         Test.Test_Vector (I) :=  (Re => Real (I), Im => Real (I) +1.0);
      end loop;
   end Set_Up_Case;

   procedure Simple_Test (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Tc);
   begin
      Write ("Test_Vector", Write (Create (T.Test_Vector)));
      Write ("Test_Matrix", Write (Create (T.Test_Matrix)));
   end;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (Test : in out Test_Case) is
   begin
      Registration.Register_Routine (Test    => Test,
                                     Routine => Simple_Test'Unrestricted_Access,
                                     Name    =>  "Simple_Test");

   end Register_Tests;

   ----------
   -- Name --
   ----------

   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (GNAT.Source_Info.Enclosing_Entity);
   end Name;

end GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests;
