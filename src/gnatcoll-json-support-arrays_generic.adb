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
         Set_Field (Ret, "First", Create (Val'First));
         Set_Field (Ret, "Last", Create (Val'Last));
         Set_Field (Ret, "Data", Create (Create (Val)));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Array_Type is
      First : Index_Type := Index_Type'Last;
      Last  : Index_Type := Index_Type'First;
      Data  : JSON_Array;
      Length : Natural := 0;

      procedure Cb (Name : UTF8_String; Value : JSON_Value) is
      begin
         if Name = "First" then
            First := Get (Value);
         elsif Name = "Last" then
            Last := Get (Value);
         elsif Name = "Data" then
            Data := Get (Value);
         end if;
      end Cb;

   begin
      Map_JSON_Object (Val, Cb'Access);
      Length := GNATCOLL.JSON.Length (Data);
      if (First > Last) and then Length >= 0 then
         return Ret : Array_Type (Index_Type'Succ (Index_Type'First) .. Index_Type'First) do
            pragma Warnings (Off, Ret); -- Only used to get an empty array.
            null;
         end return;
      elsif First >= Last then
         return Ret : Array_Type (First .. Last) do
            for Ix in 1 .. Length loop
               Ret (First) := Get (Get (Data, Ix));
               if First < Index_Type'Last then
                  First := Index_Type'Succ (First);
               end if;
            end loop;
         end return;
      else
         return Ret : Array_Type (Index_Type'Succ (Index_Type'First) .. Index_Type'First) do
            pragma Warnings (Off, Ret); -- Only used to get an empty array.
            null;
         end return;
      end if;
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
