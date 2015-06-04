with GNATCOLL.JSON.Support.GNAT.SPitbol;
use GNATCOLL.JSON;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
package GNAT.Spitbol.Table_VString.JSON is
  new GNATCOLL.JSON.Support.GNAT.SPitbol.Table
    (Value_Type => GNAT.Spitbol.VString,
     Null_Value => GNAT.Spitbol.Nul,
     Img        => To_String,
     "="        => "=",
     V          => GNAT.Spitbol.Table_VString);
