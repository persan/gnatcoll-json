package body GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets is

   ------------
   -- Create --
   ------------

   function Create (Val : Set) return JSON_Value is
      Data : JSON_Array;
   begin
      return Ret : constant JSON_Value := Create_Object do
         for I of Val loop
            Append (Data, Create (I));
         end loop;
         Set_Field (Ret, "Capacity", Val.Length);
         Set_Field (Ret, "Data", Data);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Set is
      L : constant JSON_Array := Val.Get ("Data");
   begin
      return Ret : V.Set do
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
end GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets;
