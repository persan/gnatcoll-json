with AUnit;
with GNAT.Source_Info;
with GNATCOLL.JSON.Support.Test.Utilities;
with AUnit.Assertions; use AUnit.Assertions;
package body GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests is

   use AUnit;
   use AUnit.Test_Cases;
   use GNATCOLL.JSON.Support.Test.Utilities;

   use Complex_Arrays.Real_Arrays;
   use Complex_Arrays;
   use Real_Arrays_JSON;
   use Complex_Types_JSON;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      for I in Test.Test_Vector'Range loop
         Test.Test_Vector (I) :=  (Re => Real (I),
                                   Im => Real (I) +1.0);
      end loop;
   end Set_Up_Case;

   procedure Write_Vector (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Tc);
   begin
      Write ("Test_Vector", Write (Create (T.Test_Vector)));
   end;

   procedure Write_Matrix (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Tc);
   begin
      Write ("Test_Matrix", Write (Create (T.Test_Matrix)));
   end;

   procedure Read_Vector (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T           : Test_Case renames Test_Case (Tc);
      Test_Vector : constant Complex_Arrays.Complex_Vector := Get (Read (Read ("Test_Vector")));
   begin
      Assert (Test_Vector = T.Test_Vector, "Vector missmatch");
   end;

   procedure Read_Matrix (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T           : Test_Case renames Test_Case (Tc);
      Test_Matrix : constant Complex_Arrays.Complex_Matrix := Get (Read (Read ("Test_Matrix")));
   begin
      Assert (Test_Matrix = T.Test_Matrix, "Matrix missmatch");
   end;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (Test : in out Test_Case) is
      use Registration;
   begin
      Register_Routine (Test, Write_Vector'Unrestricted_Access, "Write_Vector");
      Register_Routine (Test, Write_Matrix'Unrestricted_Access, "Write_Matrix");
      Register_Routine (Test, Read_Vector'Unrestricted_Access, "Read_Vector");
      Register_Routine (Test, Read_Matrix'Unrestricted_Access, "Read_Matrix");

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
