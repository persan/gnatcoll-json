with Ada.Finalization; use Ada.Finalization;
package GNATCOLL.JSON.Support.Ada.Finalization is

   procedure Map_JSON_Value (User_Object : in out Controlled;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is null;
   procedure Populate (Val : in out JSON_Value; With_Data : Controlled) is null;

   procedure Map_JSON_Value (User_Object : in out Limited_Controlled;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is null;

   procedure Populate (Val : in out JSON_Value; With_Data : Limited_Controlled) is null;

end GNATCOLL.JSON.Support.Ada.Finalization;
