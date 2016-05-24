package body GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets is

   ------------
   -- Create --
   ------------

   function Create (Val : Set) return JSON_Value is
      Ret : JSON_Array;
   begin
      for I of Val loop
         Append (Ret, Create (I));
      end loop;
      return Create (Ret);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Set is
      L : JSON_Array := Val.Get;
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
   end ;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Set) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end ;
end GNATCOLL.JSON.Support.Ada.Containers.Ordered_Sets;
