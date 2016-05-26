with Ada.Directories;
with Ada.Text_IO;
with GNAT.Source_Info;
with AUnit.Assertions;
package body GNATCOLL.JSON.Support.Test.Check_Golden is
   use Ada.Directories;
   use AUnit;
   use AUnit.Assertions;
   ---------------
   -- Test_Read --
   ---------------
   procedure Check_Golden (Test : in out AUnit.Test_Cases.Test_Case'Class)  is
      pragma Unreferenced (Test);
      Ok   : Boolean := True;
      procedure Process (Directory_Entry : Directory_Entry_Type) is
         Name : constant String := Ada.Directories.Simple_Name (Directory_Entry);
      begin
         Ada.Text_IO.Put_Line (Name);
         if not Exists (Name) then
            Assert (False, Name & " Does not exist");
         end if;
      exception
         when others  =>
            Ok := False;
      end Process;
   begin
      if Exists ("golden") then
         Ada.Directories.Search (Directory => "golden", Pattern => "*.json", Process => Process'Access);
      end if;
      if not Ok then
         Assert (Ok, " Goldend check failed");
      end if;
   end Check_Golden;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (Test : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test    => Test,
                        Routine => Check_Golden'Unrestricted_Access,
                        Name    =>  "Check_Golden:1");
   end Register_Tests;

   ----------
   -- Name --
   ----------
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (Unit_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.Check_Golden;
