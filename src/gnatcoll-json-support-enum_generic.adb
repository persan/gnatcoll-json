package body GNATCOLL.JSON.Support.Enum_Generic is

   ------------
   -- Create --
   ------------

   function Create (Val : Enum) return JSON_Value is
      Img : constant String := Val'Img;
   begin
      return Create (Link_Prefix & Img (Img'First + Code_Prefix'Length .. Img'Last - Code_Suffix'Length) & Link_Suffix);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Enum is
      Img : constant String := Get (Val);
   begin
      return Enum'Value (Code_Prefix & Img (Img'First + Link_Prefix'Length .. Img'Last - Link_Suffix'Length) & Code_Suffix);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Enum is
   begin
      return Enum'Value (Val.Get (Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Enum)
   is
   begin
      Set_Field (Val, Field_Name, Field'Img);
   end Set_Field;

end GNATCOLL.JSON.Support.Enum_Generic;
