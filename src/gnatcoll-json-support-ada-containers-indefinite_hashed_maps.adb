package body GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps is

   ------------
   -- Create --
   ------------

   function Create (Val : Map) return JSON_Value is
      Data : JSON_Array;

   begin
      for I in Val.Iterate loop
         declare
            O : constant JSON_Value := Create_Object;
         begin
            O.Set_Field ("Key", Create_Key (Key (I)));
            O.Set_Field ("Element", Create_Element (Element (I)));
            Append (Data, O);
         end;
      end loop;
      return Create (Data);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Map is
      L : constant JSON_Array := Val.Get;
   begin
      return Ret : Map do
         for I in 1 .. Length (L) loop
            declare
               O : constant JSON_Value := Get (L, I);
            begin
               Ret.Insert (Key      => Key_Type'(Get_Name_Key (O, "Key")),
                           New_Item => Element_Type'(Get_Name_Element (O, "Element")));
            end;
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps;
