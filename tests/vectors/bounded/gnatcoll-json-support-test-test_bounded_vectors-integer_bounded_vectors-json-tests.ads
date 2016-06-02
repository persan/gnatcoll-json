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

with GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_Initialize;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.JSON;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.JSON.Tests is new
  Standard.GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors
    (Index_Type   => Natural,
     Element_Type => Integer,
     "="          => "=",
     Create       => Create,
     Get          => Get,
     V            => Integer_Bounded_Vectors,
     JSON         => Standard.GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.JSON,
     Initialize   => Integer_Bounded_Vectors_Initialize);
