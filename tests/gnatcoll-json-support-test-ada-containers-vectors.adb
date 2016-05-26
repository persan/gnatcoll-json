with GNAT;
with GNAT.Source_Info;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Directories;
with AUnit.Assertions;
package body GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors is

   use AUnit;
   use AUnit.Assertions;
   use Standard.Ada.Text_IO;
   use V;
   use JSON;
   use Standard.Ada.Strings;

   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   function DataPath return String is
      Ts : constant Maps.Character_Mapping := Maps.To_Mapping (".", "-");
   begin
      return Fixed.Translate (Unit_Name, Ts) & ".json";
   end DataPath;

   -----------------
   -- Set_Up_Case --
   -----------------
   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      Initialize (Test.Test_Data);
   end Set_Up_Case;

   ----------------
   -- Test_Write --
   ----------------
   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      F    : Standard.Ada.Text_IO.File_Type;
      Td   : Test_Case renames Test_Case (Test);
   begin
      Create (F, Out_File, DataPath);
      String'Write (Text_Streams.Stream (F), GNATCOLL.JSON.Write (Create (Td.Test_Data), Compact => False));
      Close (F);
   end Test_Write;

   ---------------
   -- Test_Read --
   ---------------
   procedure Test_Read (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      F    : Standard.Ada.Text_IO.File_Type;
      Td   : Test_Case renames Test_Case (Test);
      Size : constant Standard.Ada.Directories.File_Size := Standard.Ada.Directories.Size (DataPath);
   begin
      Open (F, In_File, DataPath);
      declare
         Buffer : String (1 .. Natural (Size));
      begin
         String'Read (Text_Streams.Stream (F), Buffer);
         Td.Result := Get (GNATCOLL.JSON. Read (Buffer, Filename => DataPath));
      end;
      Close (F);
      Assert (Td.Result = Td.Test_Data, "data mismatch");
   end Test_Read;

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
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

   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (Unit_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors;
