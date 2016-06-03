package body GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types is
   use GNATCOLL.JSON;
   ------------
   -- Create --
   ------------

   function Create
     (Val : Complex)
      return JSON_Value
   is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field_Long_Float (Ret, "re", Long_Float (Val.Re));
         Set_Field_Long_Float (Ret, "im", Long_Float (Val.Im));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Complex is
   begin
      return Ret : Complex do
         Ret.Re := Real (Get_Long_Float (Val, "re"));
         Ret.Im := Real (Get_Long_Float (Val, "im"));
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Complex
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Complex)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   -------------
   -- To_Time --
   -------------
end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types;
