package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors is
  ------------
   -- Create --
   ------------

   function Create (Val : Vector) return JSON_Value is
      Data : JSON_Array;

   begin
      return ret : JSON_Value := Create_Object do
         for I of Val  loop
            Append (Data, Create (I));
         end loop;
         Set_Field(Ret,"Capacity",Val.Capacity);
         Set_Field(Ret,"Data",Data);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Vector is
      L : JSON_Array := Val.Get ("Data");
   begin
      return Ret : Vector (Get (Val, "Capacity")) do
         for I in 1 .. Length (L) loop
            Ret.Append (Element_Type'(get((Get (L, I)))));
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Vector is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end ;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Vector) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end ;

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors;
