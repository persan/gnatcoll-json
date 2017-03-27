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
pragma Warnings (Off);
with GNATCOLL.JSON.Support.GNAT.Spitbol.Table_Boolean;
with GNATCOLL.JSON.Support.GNAT.Spitbol.Table_Integer;
with GNATCOLL.JSON.Support.GNAT.Spitbol.Table_VString;
with GNATCOLL.JSON.Support.Ada.Calendar;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Maps;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Maps_Simple;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Sets;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps_Simple;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Ada.Containers.Doubly_Linked_Lists;
with GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps;
with GNATCOLL.JSON.Support.Ada.Containers.Hashed_Maps_Simple;
with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps;
with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps_Simple;
with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors;
with GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps;
with GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets;
with GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps_Simple;
with GNATCOLL.JSON.Support.Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Ada.Numerics;
with GNATCOLL.JSON.Support.Ada.Numerics.Complex_Arrays_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Complex_Types_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Complex_Arrays_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Complex_Types_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Real_Arrays_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Long_Complex_Arrays_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Long_Complex_Types_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Long_Long_Real_Arrays_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Real_Arrays_JSON;


pragma Warnings (On);


with AUnit.Run;
with AUnit.Reporter.Text;
-- with AUnit.Reporter.GNATtest;
-- with AUnit.Reporter.XML;

with GNATCOLL.JSON.Support.Test.Suits.All_Tests;

procedure GNATCOLL.JSON.Support.Test.Main is
   procedure Run is new AUnit.Run.Test_Runner (GNATCOLL.JSON.Support.Test.Suits.All_Tests.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   -- Reporter : AUnit.Reporter.GNATtest.GNATtest_Reporter;
   -- Reporter : AUnit.Reporter.XML.XML_Reporter;
begin
   Run (Reporter);
end GNATCOLL.JSON.Support.Test.Main;
