with GNAT;
with GNAT.Source_Info;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;
with GNATCOLL.JSON.Support.System;use GNATCOLL.JSON.Support.System;
with System.Storage_Elements;use System.Storage_Elements;
package body GNATCOLL.JSON.Support.Test.System is


   use AUnit;
   use AUnit.Assertions;
   use GNATCOLL.JSON.Support.Test.Utilities;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;


   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is

   begin
      Test.S_0 := Null_Address;
      Test.S_1 := Storage_Elements.To_Address(16#CACAFF11#);
   end Set_Up_Case;

   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td   : Test_Case renames Test_Case (Test);
      O    : constant JSON_Value := Create_Object;
   begin
      Set_Field (O, "S_0", Td.S_0);
      Set_Field (O, "S_1", Td.S_1);
      Write (Ada2file (Unit_Name), GNATCOLL.JSON.Write (O, Compact => False));
   end Test_Write;

   procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td    : Test_Case renames Test_Case (Test);
      O     : constant JSON_Value := GNATCOLL.JSON.Read (Read (Ada2file (Unit_Name)), Ada2file (Unit_Name));
      S_0   : Address;
      S_1   : Address;

   begin
      S_0 := Get (O, "S_0");
      S_1 := Get (O, "S_1");
      Assert (S_0 = Td.S_0 , "S_0 missmatch");
      Assert (S_1 = Td.S_1 , "S_0 missmatch");
   end Test_Read;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test    => Test,
                        Routine => Test_Write'Unrestricted_Access,
                        Name    =>  "Test_Write");

      Register_Routine (Test    => Test,
                        Routine => Test_Read'Unrestricted_Access,
                        Name    =>  "Test_Read");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (Unit_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.System;
