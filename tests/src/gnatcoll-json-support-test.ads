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

with Ada.Calendar; -- @ Impl -> TestOK
with Ada.Real_Time; -- @ Impl -> TestOK

with Ada.Containers; -- @ Impl -> TestOK
with Ada.Containers.Bounded_Doubly_Linked_Lists; -- @ Impl
with Ada.Containers.Bounded_Hashed_Maps; -- @ Impl/Simple -> TestOK
with Ada.Containers.Bounded_Hashed_Sets; -- @ Impl
with Ada.Containers.Bounded_Multiway_Trees;
with Ada.Containers.Bounded_Ordered_Maps; -- @ Impl/Simple
with Ada.Containers.Bounded_Ordered_Sets; -- @ Impl
with Ada.Containers.Bounded_Vectors; -- @ Impl-> TestOK
with Ada.Containers.Doubly_Linked_Lists; -- @ Impl-> TestOK

with Ada.Containers.Hashed_Maps; -- @ Impl-> TestOK/Simple-> TestOK
with Ada.Containers.Hashed_Sets; -- @ Impl-> TestOK
with Ada.Containers.Indefinite_Doubly_Linked_Lists; -- @ Impl-> TestOK
with Ada.Containers.Indefinite_Hashed_Maps; -- @ Impl/Simple
with Ada.Containers.Indefinite_Hashed_Sets; -- @ Impl
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps; -- @ Impl
with Ada.Containers.Indefinite_Ordered_Multisets; -- @ Impl
with Ada.Containers.Indefinite_Ordered_Sets; -- @ Impl
with Ada.Containers.Indefinite_Vectors; -- @ Impl
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Ordered_Maps; -- @ Impl/Simple
with Ada.Containers.Ordered_Multisets; -- @ Impl
with Ada.Containers.Ordered_Sets; -- @ Impl-> TestOK
with Ada.Containers.Restricted_Doubly_Linked_Lists;
with Ada.Containers.Vectors; -- @ Impl-> TestOK

pragma Warnings (On);

package GNATCOLL.JSON.Support.Test is
end GNATCOLL.JSON.Support.Test;
