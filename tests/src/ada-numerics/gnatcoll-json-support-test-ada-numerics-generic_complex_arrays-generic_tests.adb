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

   Test_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      for I in Test.Test_Vector'Range loop
         Test.Test_Vector (I) :=  (Re => Real (I),
                                   Im => Real (I) + 1.0);
      end loop;
   end Set_Up_Case;

   Vector_File_Name : constant String := Ada2file (Test_Name & ".Vector");
   Matrix_File_Name : constant String := Ada2file (Test_Name & ".Matrix");
   procedure Write_Vector (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Tc);
   begin
      Write (Vector_File_Name, Write (Create (T.Test_Vector)));
   end Write_Vector;

   procedure Write_Matrix (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Tc);
   begin
      Write (Matrix_File_Name, Write (Create (T.Test_Matrix)));
   end Write_Matrix;

   procedure Read_Vector (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T           : Test_Case renames Test_Case (Tc);
      Test_Vector : constant Complex_Arrays.Complex_Vector := Get (Read (Read (Vector_File_Name)));
   begin
      Assert (Test_Vector = T.Test_Vector, "Vector missmatch");
   end Read_Vector;

   procedure Read_Matrix (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      T           : Test_Case renames Test_Case (Tc);
      Test_Matrix : constant Complex_Arrays.Complex_Matrix := Get (Read (Read (Matrix_File_Name)));
   begin
      Assert (Test_Matrix = T.Test_Matrix, "Matrix missmatch");
   end Read_Matrix;

   procedure Empty_Matrix_1 (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Tc);
      Test_Matrix : Complex_Arrays.Complex_Matrix (1 .. 0, 1 .. 0);
      J           : constant JSON_Value := Create (Test_Matrix);
      Result      : constant Complex_Arrays.Complex_Matrix := Get (J);
   begin

      Assert (Test_Matrix = Result, "Matrix missmatch");
   end Empty_Matrix_1;
   procedure Empty_Matrix_2 (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Tc);
      Test_Matrix : Complex_Arrays.Complex_Matrix (1 .. 1, 1 .. 0);
      J           : constant JSON_Value := Create (Test_Matrix);
      Result      : constant Complex_Arrays.Complex_Matrix := Get (J);
   begin
      Assert (Test_Matrix = Result, "Matrix missmatch");
   end Empty_Matrix_2;

   procedure Empty_Vector (Tc : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Tc);
      Test_Vector : Complex_Arrays.Complex_Vector (1 .. 0);
      J           : constant JSON_Value := Create (Test_Vector);
      Result      : constant Complex_Arrays.Complex_Vector := Get (J);
   begin

      Assert (Test_Vector = Result, "Vector missmatch");
   end Empty_Vector;

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
      Register_Routine (Test, Empty_Matrix_1'Unrestricted_Access, "Empty_Matrix_1");
      Register_Routine (Test, Empty_Matrix_2'Unrestricted_Access, "Empty_Matrix_2");
      Register_Routine (Test, Empty_Vector'Unrestricted_Access, "Empty_Vector");

   end Register_Tests;

   ----------
   -- Name --
   ----------
   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (Test_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.Ada.Numerics.Generic_Complex_Arrays.Generic_Tests;
