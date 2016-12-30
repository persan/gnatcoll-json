------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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
      Ok            : Boolean := True;
      procedure Process (Directory_Entry : Directory_Entry_Type) is

         Result_Path   : constant String := Ada.Directories.Compose ("output", Ada.Directories.Simple_Name (Directory_Entry));

         Golden_Path   : constant String := Ada.Directories.Full_Name (Directory_Entry);
         F1, F2        : Ada.Text_IO.File_Type;
         use Ada.Text_IO;
      begin
         if not Exists (Result_Path) then
            Assert (False, Result_Path & " Does not exist");
         end if;
         begin
            Open (F1, In_File, Result_Path);
            Open (F2, In_File, Golden_Path);
            while not (End_Of_File (F1) or else End_Of_File (F2)) loop
               if Get_Line (F1) /= Get_Line (F2) then
                  Assert (False, "Contents of " & Result_Path & " missmatch");
               end if;
            end loop;
         exception
            when others =>
               Ok := False;
         end;
         Close (F1);
         Close (F2);
      exception
         when others  =>
            Ok := False;
      end Process;
   begin
      if Exists ("golden") then
         Ada.Directories.Search (Directory => "golden", Pattern => "*.json", Process => Process'Access);
      end if;

      Assert (Ok, " Goldend check failed");
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
                        Name    =>  "Check_Golden");
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
