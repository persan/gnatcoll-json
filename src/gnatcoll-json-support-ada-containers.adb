package body GNATCOLL.JSON.Support.Ada.Containers is
   use GNATCOLL.JSON;
   ------------
   -- Create --
   ------------

   function Create (Val : Hash_Type) return JSON_Value is
   begin
      return Create (Long_Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Hash_Type is
   begin
      return Hash_Type (Long_Long_Integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Hash_Type
   is
   begin
      return Hash_Type (Long_Integer'(Get (Val, Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Hash_Type)
   is
   begin
      Set_Field (Val, Field_Name, Long_Integer (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create (Val : Count_Type) return JSON_Value is
   begin
      return Create (Long_Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Count_Type is
   begin
      return Count_Type (Long_Long_Integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Count_Type
   is
   begin
      return Count_Type (Long_Integer'(Get (Val, Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Count_Type)
   is
   begin
      Set_Field (Val, Field_Name, Long_Integer (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers;
