package body GNATCOLL.JSON.Support.Simple_Arrays_Generic is

   Data_Field_Name : constant String := "Data";
   ------------
   -- Create --
   ------------

   function Create (Val : Array_Type) return JSON_Array is

   begin
      return Data : JSON_Array do
         for I of Val loop
            Append (Data, Create (I));
         end loop;
      end return;
   end Create;

   -------------------
   -- Create_Object --
   -------------------
   function Create_Object (Val : Array_Type) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, Data_Field_Name, Create (Val));
      end return;
   end Create_Object;
   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Array) return Array_Type is
      Cursor : Index_Type;
      First   : constant Index_Type := Index_Type'First;
      Len     : constant Integer := Length (Val);
      Last    : constant Index_Type := First + Index_Type'Pred (Index_Type (Len));
   begin
      return Ret : Array_Type (First .. Last) do
         Cursor :=    Ret'First;
         for I in 1 .. Length (Val) loop
            Ret (Cursor) :=  (Element_Type'(Get (Get (Val, I))));
            Cursor := Index_Type'Succ (Cursor);
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Array_Type is
   begin
      return Get (JSON_Array'(Get (Val, Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Array_Type) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Simple_Arrays_Generic;
