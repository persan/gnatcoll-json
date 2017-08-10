with GNAT;
with GNAT.Source_Info;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;
with GNATCOLL.JSON.Support.System.Storage_Elements; use GNATCOLL.JSON.Support.System.Storage_Elements;
package body GNATCOLL.JSON.Support.Test.System.Storage_Elements is

   use AUnit;
   use AUnit.Assertions;
   use GNATCOLL.JSON.Support.Test.Utilities;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is

   begin
      Test.T_0 := 16#1234AFFE#;
      Test.T_1 := 16#87#;
      Test.T_2 := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      Test.T_3 := 16#CACACACA#;
   end Set_Up_Case;

   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td   : Test_Case renames Test_Case (Test);
      O    : constant JSON_Value := Create_Object;
   begin
      Set_Field (O, "T_0", Td.T_0);
      Set_Field (O, "T_1", Td.T_1);
      Set_Field (O, "T_2", Td.T_2);
      Set_Field (O, "T_3", Td.T_3);
      Write (Ada2file (Unit_Name), GNATCOLL.JSON.Write (O, Compact => False));
   end Test_Write;

   procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td    : Test_Case renames Test_Case (Test);
      O     : constant JSON_Value := GNATCOLL.JSON.Read (Read (Ada2file (Unit_Name)), Ada2file (Unit_Name));

      T_0 : Storage_Offset;
      T_1 : Storage_Element;
      T_2 : Storage_Array (1 .. 10);
      T_3 : Integer_Address;

   begin
      T_0 := Get (O, "T_0");
      T_1 := Get (O, "T_1");
      T_2 := Get (O, "T_2");
      T_3 := Get (O, "T_3");
      Assert (T_0 = Td.T_0, "T_0 missmatch");
      Assert (T_1 = Td.T_1, "T_1 missmatch");
      Assert (T_2 = Td.T_2, "T_2 missmatch");
      Assert (T_3 = Td.T_3, "T_3 missmatch");
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

end GNATCOLL.JSON.Support.Test.System.Storage_Elements;
