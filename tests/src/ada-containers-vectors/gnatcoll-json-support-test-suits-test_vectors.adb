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

with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Bounded_Doubly_Linked_Lists.JSON.Test;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Indefinite_Doubly_Linked_Lists.JSON.Test;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Doubly_Linked_Lists.JSON.Test;
pragma Warnings (Off);
--  with GNATCOLL.JSON.Support.GNAT.Spitbol.Table_VString.Test;
pragma Warnings (On);
package body GNATCOLL.JSON.Support.Test.Suits.Test_Vectors is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_1 : aliased GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors.JSON.Tests.Test_Case;
   Test_2 : aliased GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.JSON.Tests.Test_Case;
   Test_3 : aliased GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Bounded_Doubly_Linked_Lists.JSON.Test.Test_Case;
   Test_4 : aliased GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Doubly_Linked_Lists.JSON.Test.Test_Case;
   Test_5 : aliased GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Indefinite_Doubly_Linked_Lists.JSON.Test.Test_Case;
--    Test_6 : aliased GNATCOLL.JSON.Support.GNAT.Spitbol.Table_VString.Test.Test_Case;
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      Add_Test (Result'Access, Test_3'Access);
      Add_Test (Result'Access, Test_4'Access);
      Add_Test (Result'Access, Test_5'Access);
--        Add_Test (Result'Access, Test_6'Access);
      return Result'Access;
   end Suite;

end GNATCOLL.JSON.Support.Test.Suits.Test_Vectors;
