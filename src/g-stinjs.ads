with GNATCOLL.JSON.Support.GNAT.SPitbol;
use GNATCOLL.JSON;
package GNAT.Spitbol.Table_Integer.JSON is
  new GNATCOLL.JSON.Support.GNAT.SPitbol.JSON_Table
    (Value_Type => Integer,
     Null_Value => Integer'First,
     Img        => Integer'Image,
     "="        => "=",
     V          => GNAT.Spitbol.Table_Integer);
