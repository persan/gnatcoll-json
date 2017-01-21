with GNATCOLL.JSON.Support.Simple_Arrays_Generic;
package body GNATCOLL.JSON.Support.Arrays_Generic is
   package Simple is new GNATCOLL.JSON.Support.Simple_Arrays_Generic (Index_Type, Element_Type, Array_Type, Create, Get);
   use Simple;
   ------------
   -- Create --
   ------------

   function Create (Val : Array_Type) return JSON_Value is

   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "First", Integer(Index_Type'Pos (Val'First)));
         Set_Field (Ret, "Last", Integer (Index_Type'Pos (Val'Last)));
         Set_Field (Ret, "Data", Create(Create (Val)));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Array_Type is
      First : Index_Type := Index_Type'Val (Integer'(Get (Val, "First")));
      Last  : constant Index_Type := Index_Type'Val (Integer'(Get (Val, "Last")));
      Data  : constant JSON_Array := Get (Val, "Data");
   begin
      return Ret : Array_Type (First .. Last) do
         for Ix in 1..length(data) loop
            Ret (First) := Get (Get (Data, Ix));
            First := Index_Type'Succ (First);
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Array_Type is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Array_Type)
   is
   begin
      Set_Field (Val, Field_Name, JSON_Value'(Create (Field)));
   end Set_Field;

end GNATCOLL.JSON.Support.Arrays_Generic;
