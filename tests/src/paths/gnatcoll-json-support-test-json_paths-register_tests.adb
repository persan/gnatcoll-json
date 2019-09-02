--  begin read only
separate (GNATCOLL.JSON.Support.Test.JSON_Paths)
overriding procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
   Register_Routine (Test, Test_Get_deep'Unrestricted_Access, "Test_Get_deep");
   Register_Routine (Test, Test_Get_Float'Unrestricted_Access, "Test_Get_Float");
   Register_Routine (Test, Test_Get_Integer'Unrestricted_Access, "Test_Get_Integer");
   Register_Routine (Test, Test_Get_String'Unrestricted_Access, "Test_Get_String");
   Register_Routine (Test, Test_Get_Array_1'Unrestricted_Access, "Test_Get_Array_1");
   Register_Routine (Test, Test_Get_Array_2'Unrestricted_Access, "Test_Get_Array_2");
   Register_Routine (Test, Test_Get_Array_3'Unrestricted_Access, "Test_Get_Array_3");
   Register_Routine (Test, Test_Get_Array_Hidden_Alternate_1'Unrestricted_Access, "Test_Get_Array_Hidden_Alternate_1");
   Register_Routine (Test, Test_Get_Array_Hidden_Alternate_2'Unrestricted_Access, "Test_Get_Array_Hidden_Alternate_2");
   Register_Routine (Test, Test_Get_Array_Hidden_Alternate_3'Unrestricted_Access, "Test_Get_Array_Hidden_Alternate_3");
end Register_Tests;
--  end read only