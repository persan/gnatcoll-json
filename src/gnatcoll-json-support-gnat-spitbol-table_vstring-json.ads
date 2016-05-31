with GNAT.Spitbol.Table_VString; use GNAT.Spitbol;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package GNATCOLL.JSON.Support.GNAT.Spitbol.Table_VString.JSON is
  new GNATCOLL.JSON.Support.GNAT.SPitbol.JSON_Table
    (V          => Standard.GNAT.Spitbol.Table_VString);
