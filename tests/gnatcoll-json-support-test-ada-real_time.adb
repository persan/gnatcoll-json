with GNAT;
with GNAT.Source_Info;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;
with GNATCOLL.JSON.Support.Ada.Real_Time;
package body GNATCOLL.JSON.Support.Test.Ada.Real_Time is


   use AUnit;
   use AUnit.Assertions;
   use Standard.Ada.Real_Time;
   use GNATCOLL.JSON.Support.Test.Utilities;
   use GNATCOLL.JSON.Support.Ada.Real_Time;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;


   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is

   begin
      Test.T := Time_Of (16#5000_0000#, To_Time_Span (1.0));
      Test.Ts := To_Time_Span (87455.0);
   end Set_Up_Case;

   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td   : Test_Case renames Test_Case (Test);
      O    : constant JSON_Value := Create_Object;
   begin
      Set_Field (O, "Time", Td.T);
      Set_Field (O, "Time_Span", Td.Ts);
      Set_Field (O, "Funny", Integer'(1));
      Write (Ada2file (Unit_Name), GNATCOLL.JSON.Write (O, Compact => False));
   end Test_Write;

   procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      Td : Test_Case renames Test_Case (Test);
      O  : constant JSON_Value := GNATCOLL.JSON.Read (Read (Ada2file (Unit_Name)), Ada2file (Unit_Name));
      T  : Time;
      Ts : Time_Span;

   begin
      T := Get (O, "Time");
      Ts := Get (O, "Time_Span");
      Assert (T = Td.T , "Time missmatch");
      Assert (Ts = Td.Ts , "Time_Span missmatch");
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

end GNATCOLL.JSON.Support.Test.Ada.Real_Time;
