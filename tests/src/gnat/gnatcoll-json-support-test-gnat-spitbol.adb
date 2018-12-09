with GNAT;
with GNAT.Source_Info;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;

package body GNATCOLL.JSON.Support.Test.GNAT.Spitbol is

   ------------------
   -- Generic_Test --
   ------------------

   package body Generic_Test is
      use AUnit;
      use AUnit.Assertions;
      use GNATCOLL.JSON.Support.Test.Utilities;
      use T;
      use T.V;
      Unit_Name : constant String := Standard.GNAT.Source_Info.Enclosing_Entity;

   -----------------
   -- Set_Up_Case --
   -----------------

      overriding procedure Set_Up_Case (Test : in out Test_Case) is
      begin
         Test.Test_Data := new Table'(Initialize);
      end Set_Up_Case;
   ----------------
   -- Test_Write --
   ----------------
      procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
         Td   : Test_Case renames Test_Case (Test);
      begin
         Write (Ada2file (Unit_Name), GNATCOLL.JSON.Write (Create (Td.Test_Data.all), Compact => False));
      end Test_Write;

   ---------------
   -- Test_Read --
   ---------------
      procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
         Td     : Test_Case renames Test_Case (Test);
         Result : constant Table := Get (GNATCOLL.JSON. Read (Read (Ada2file (Unit_Name)), Filename => Ada2file (Unit_Name)));
      begin
         Assert (Result = Td.Test_Data.all, "data mismatch");
      end Test_Read;

   ------------------------
   -- Test_Get_Set_Filed --
   ------------------------

      procedure Test_Get_Set_Filed (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
         Td  : Test_Case renames Test_Case (Test);
         J   : constant GNATCOLL.JSON.JSON_Value := Create_Object;
      begin
         Set_Field (J, "testData", Td.Test_Data.all);
         Set_Field (J, "testSting", "Dummy");
         declare
            Result : constant Table := Get (J, "testData");
         begin
            Assert (Result = Td.Test_Data.all, "data mismatch");
         end;
      end Test_Get_Set_Filed;

   --------------------
   -- Register_Tests --
   --------------------

      overriding procedure Register_Tests (Test : in out Test_Case) is
         use AUnit.Test_Cases.Registration;
      begin
         Register_Routine (Test, Test_Write'Unrestricted_Access, "Test_Write");
         Register_Routine (Test, Test_Read'Unrestricted_Access, "Test_Read");
         Register_Routine (Test, Test_Get_Set_Filed'Unrestricted_Access, "Test_Get_Set_Filed");

      end Register_Tests;

   ----------
   -- Name --
   ----------

      overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
      begin
         return Format (Unit_Name);
      end Name;

   end Generic_Test;

end GNATCOLL.JSON.Support.Test.GNAT.Spitbol;
