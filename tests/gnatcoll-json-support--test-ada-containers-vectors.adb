with AUnit.Test_Cases;
with GNAT;
with GNAT.Source_Info;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with GNATCOLL.Io.Native;
package body GNATCOLL.JSON.Support.Test .. Ada.Containers.Vectors is
   use AUnit;
   use AUnit.Test_Cases.Registration;
   use Standard.Ada.Text_IO;
   --------------------
   -- Register_Tests --
   --------------------
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;
   use Standard.Ada.Strings;
   function DataPath return String is
      Ts : Maps.Character_Mapping := Maps.To_Mapping (".", "-");
   begin
      return Fixed.Translate ( Unit_Name, Ts) & ".json";
   end;
   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      Initialize (Test.Test_Data);
   end;

   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      F    : Standard.Ada.Text_IO.File_Type;
      Td   : Test_Case renames Test_Case (Test);
   begin
      Open (F, In_File, DataPath);
      String'Write (Text_Streams.Stream (F), GNATCOLL.JSON.Write (Create (Td.Test_Data), Compact => False));
      Close (F);
   end;

   procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      F    : Standard.Ada.Text_IO.File_Type;
      Td   : Test_Case renames Test_Case (Test);
   begin
      Open (F, In_File, DataPath);

      Put (F, Write (Create (Td.Test_Data)));
      Close (F);
   end;

   procedure Register_Tests (Test : in out Test_Case) is
   begin
      Register_Routine (Test    => Test,
                        Routine => Test_Write'Unrestricted_Access,
                        Name    =>  "Test_Write:1");

      Register_Routine (Test    => Test,
                        Routine => Test_Read'Unrestricted_Access,
                        Name    =>  "Test_Read:1");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (Test : Test_Case) return AUnit.Message_String is
   begin
      return Format (Unit_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors;
