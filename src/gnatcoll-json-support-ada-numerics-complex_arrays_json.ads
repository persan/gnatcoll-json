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

with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
with GNATCOLL.JSON.Support.Ada.Numerics.Complex_Types_JSON;
with GNATCOLL.JSON.Support.Ada.Numerics.Real_Arrays_JSON;
with Ada.Numerics.Complex_Arrays;

package GNATCOLL.JSON.Support.Ada.Numerics.Complex_Arrays_JSON is new
  GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays
    (Complex_Arrays     => Standard.Ada.Numerics.Complex_Arrays,
     Real_Arrays_JSON   => GNATCOLL.JSON.Support.Ada.Numerics.Real_Arrays_JSON,
     Complex_Types_JSON => GNATCOLL.JSON.Support.Ada.Numerics.Complex_Types_JSON);
