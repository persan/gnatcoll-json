with GNATCOLL.JSON.Support.GNAT.SPitbol;
use GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package GNAT.Spitbol.Table_Boolean.JSON is
  new GNATCOLL.JSON.Support.GNAT.SPitbol.JSON_Table
    (Value_Type => Boolean,
     Null_Value => False,
     Img        => To_String,
     "="        => "=",
     V          => GNAT.Spitbol.Table_Boolean);
