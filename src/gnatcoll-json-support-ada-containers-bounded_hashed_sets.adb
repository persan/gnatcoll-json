package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Sets is

   ------------
   -- Create --
   ------------

   function Create (Val : Set) return JSON_Value is
      Arr : JSON_Array;
   begin
      return Ret : JSON_Value do
         for I of Val loop
            Append (Arr, Create (I));
         end loop;
         Set_Field (Ret, "Capacity", Create (Val.Capacity));
         Set_Field (Ret, "Modulus", Create (Val.Modulus));
         Set_Field (Ret, "Data", Create (Arr));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Set is
      L : constant JSON_Array := Val.Get ("Data");
   begin
      return Ret : Set (Get (Val, "Capacity"), Get (Val, "Modulus")) do
         for I in 1 .. Length (L) loop
            Ret.Include (Element_Type'(Get (Get (L, I))));
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Set is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Set) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;
end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Sets;
