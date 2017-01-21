package body GNATCOLL.JSON.Support.Float_Generic is

   ------------
   -- Create --
   ------------

   function Create (Val : Num) return JSON_Value is
   begin
      return Create (Long_Float (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Num is
   begin
      return Num (Long_Float'(Get_Long_Float (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Num is
   begin
      return Num(Get_Long_Float (Val =>  Val, Field => Field));
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
      Set_Field_Long_Float (Val, Field_Name, Long_Float (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Float_Generic;
