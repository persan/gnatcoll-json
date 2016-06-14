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

with GNAT;
with GNAT.Source_Info;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;
package body GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Sets_SImple.Generic_Tests is

   use AUnit;
   use AUnit.Assertions;
   use GNATCOLL.JSON.Support.Test.Utilities;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   -----------------
   -- Set_Up_Case --
   -----------------

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      Test.Test_Data :=  new Set'(Initialize);
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
      Result : constant Set := Get (GNATCOLL.JSON. Read (Read (Ada2file (Unit_Name)), Filename => Ada2file (Unit_Name)));
   begin
      Assert (Result = Td.Test_Data.all, "data mismatch");
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

end GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Sets_SImple.Generic_Tests;
