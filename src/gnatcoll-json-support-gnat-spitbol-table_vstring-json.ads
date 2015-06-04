with GNAT.Spitbol.Table_VString;use GNAT.Spitbol;
with Ada.Strings.Unbounded;use Ada.Strings.Unbounded;
package GNATCOLL.JSON.Support.GNAT.Spitbol.Table_VString.JSON is
  new GNATCOLL.JSON.Support.GNAT.SPitbol.Table
    (Value_Type => VString,
     Null_Value => Nul,
     Img        => To_String,
     "="        => "=",
     V          => Standard.GNAT.Spitbol.Table_VString);
