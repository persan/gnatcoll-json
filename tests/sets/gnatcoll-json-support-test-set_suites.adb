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


with GNATCOLL.JSON.Support.Test.Integer_Bounded_Hashed_Sets.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_Bounded_Ordered_Sets.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_Indefinite_Hashed_Sets.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_Indefinite_Ordered_Sets.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_Hashed_Sets.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_Ordered_Sets.JSON.Tests;

package body GNATCOLL.JSON.Support.Test.Set_Suites is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:

   Test_Bounded_Hashed_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Bounded_Hashed_Sets.JSON.Tests.Test_Case;
   Test_Bounded_Ordered_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Bounded_Ordered_Sets.JSON.Tests.Test_Case;
   Test_Indefinite_Hashed_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Indefinite_Hashed_Sets.JSON.Tests.Test_Case;
   Test_Indefinite_Ordered_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Indefinite_Ordered_Sets.JSON.Tests.Test_Case;
   Test_Hashed_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Hashed_Sets.JSON.Tests.Test_Case;
   Test_Ordered_Sets : aliased GNATCOLL.JSON.Support.Test.Integer_Ordered_Sets.JSON.Tests.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Bounded_Hashed_Sets'Access);
      Add_Test (Result'Access, Test_Bounded_Ordered_Sets'Access);
      Add_Test (Result'Access, Test_Indefinite_Hashed_Sets'Access);
      Add_Test (Result'Access, Test_Indefinite_Ordered_Sets'Access);
      Add_Test (Result'Access, Test_Hashed_Sets'Access);
      Add_Test (Result'Access, Test_Ordered_Sets'Access);
      return Result'Access;
   end Suite;
end GNATCOLL.JSON.Support.Test.Set_Suites;
