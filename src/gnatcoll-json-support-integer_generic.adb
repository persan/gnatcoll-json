package body GNATCOLL.JSON.Support.Integer_Generic is

   ------------
   -- Create --
   ------------

   function Create (Val : Num) return JSON_Value is
   begin
      return Create (Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Num is
   begin
      return Num (Long_Integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Num is
   begin
      return Get (Val => Long_Integer'(Get (Val)), Field => Field);
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Num)
   is
   begin
      Set_Field (Val, Field_Name, Long_Long_Integer (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Integer_Generic;
