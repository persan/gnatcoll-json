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

with GNATCOLL.JSON.Support.Test.Suits.Test_Vectors;
with GNATCOLL.JSON.Support.Test.Set_Suites;
with GNATCOLL.JSON.Support.Test.Map_Suites;
with GNATCOLL.JSON.Support.Test.Suits.Numerics;
--  with GNATCOLL.JSON.Support.Test.Tree_Suites;
with GNATCOLL.JSON.Support.Test.Ada.Containers;
with GNATCOLL.JSON.Support.Test.Check_Golden;
with GNATCOLL.JSON.Support.Test.Ada.Calendar;
with GNATCOLL.JSON.Support.Test.Ada.Real_Time;
with GNATCOLL.JSON.Support.Test.System;
with GNATCOLL.JSON.Support.Test.System.Storage_Elements;
with GNATCOLL.JSON.Support.Test.JSON_Paths;
package body GNATCOLL.JSON.Support.Test.Suits.All_Tests is
   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Ada_Containers          : aliased GNATCOLL.JSON.Support.Test.Ada.Containers.Test_Case;
   Test_Ada_Calendar            : aliased GNATCOLL.JSON.Support.Test.Ada.Calendar.Test_Case;
   Test_Ada_Real_Time           : aliased GNATCOLL.JSON.Support.Test.Ada.Real_Time.Test_Case;
   Test_System                  : aliased GNATCOLL.JSON.Support.Test.System.Test_Case;
   Test_System_Storage_Elements : aliased GNATCOLL.JSON.Support.Test.System.Storage_Elements.Test_Case;
   Test_Paths                   : aliased GNATCOLL.JSON.Support.Test.JSON_Paths.Test_Case;
   Golden : aliased GNATCOLL.JSON.Support.Test.Check_Golden.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Ada_Containers'Access);
      Add_Test (Result'Access, Test_Ada_Calendar'Access);
      Add_Test (Result'Access, Test_Ada_Real_Time'Access);
      Add_Test (Result'Access, Test_System'Access);
      Add_Test (Result'Access, Test_System_Storage_Elements'Access);
      Add_Test (Result'Access, Test_Paths'Access);

      Add_Test (Result'Access, Set_Suites.Suite);
      Add_Test (Result'Access, Map_Suites.Suite);
      --  Add_Test (Result'Access, Tree_Suites.Suite);
      Add_Test (Result'Access, Test_Vectors.Suite);
      Add_Test (Result'Access, Numerics.Suite);

      Add_Test (Result'Access, Golden'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.All_Tests;
