package body GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors is

   ------------
   -- Create --
   ------------

   function Create (Val : Vector) return JSON_Value is
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

   function Get (Val : JSON_Value) return Vector is
      L : constant JSON_Array := Val.Get;
   begin
      return Ret : Vector do
         for I in 1 .. Length (L) loop
            Ret.Append (Element_Type'(Get (Get (L, I))));
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Vector is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Vector) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors;
