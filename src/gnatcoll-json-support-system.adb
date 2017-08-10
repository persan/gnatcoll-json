with GNATCOLL.JSON.Support.System.Storage_Elements; use GNATCOLL.JSON.Support.System.Storage_Elements;
with System.Storage_Elements;
package body GNATCOLL.JSON.Support.System is
   use Standard.System.Storage_Elements;
   ------------
   -- Create --
   ------------

   function Create (Val : Address) return JSON_Value is
   begin
      return Create (To_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Address is
   begin
      return To_Address (Get (Val));
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Address is
   begin
      return To_Address (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Address)
   is
   begin
      Set_Field (Val, Field_Name, To_Integer (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.System;
