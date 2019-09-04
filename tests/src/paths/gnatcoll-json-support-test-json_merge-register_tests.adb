--  begin read only
separate (GNATCOLL.JSON.Support.Test.JSON_Merge)
overriding procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
   Register_Routine (Test, Test_Add_1'Unrestricted_Access, "Test_Add_1");
   Register_Routine (Test, Test_Add_2'Unrestricted_Access, "Test_Add_2");
   Register_Routine (Test, Test_Add_3'Unrestricted_Access, "Test_Add_3");
   Register_Routine (Test, Test_Or_1'Unrestricted_Access, "Test_Or_1");
   Register_Routine (Test, Test_Or_2'Unrestricted_Access, "Test_Or_2");
   Register_Routine (Test, Test_Or_3'Unrestricted_Access, "Test_Or_3");
   Register_Routine (Test, Test_Normalize_1'Unrestricted_Access, "Test_Normalize_1");
   Register_Routine (Test, Test_Normalize_2'Unrestricted_Access, "Test_Normalize_2");
end Register_Tests;
--  end read only