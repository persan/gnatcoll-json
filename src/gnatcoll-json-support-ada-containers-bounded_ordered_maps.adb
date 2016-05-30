package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps is
------------
-- Create --
------------

   function Create (Val : Map) return JSON_Value is
      Data : JSON_Array;

   begin
      return Ret : constant JSON_Value := Create_Object do
         for I in Val.Iterate (Val.First) loop
            declare
               O : constant JSON_Value := Create_Object;
            begin
               O.Set_Field ("Key", Create (Key (I)));
               O.Set_Field ("Element", Create (Element (I)));
               Append (Data, O);
            end;
         end loop;
         Set_Field (Ret, "Capacity", Val.Capacity);
         Set_Field (Ret, "Data", Data);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Map is
      L : constant JSON_Array := Val.Get ("Data");
   begin
      return Ret : Map (Get (Val, "Capacity")) do
         for I in 1 .. Length (L) loop
            declare
               O : constant JSON_Value := Get (L, I);
            begin
               Ret.Insert (Key      => Key_Type'(Get (Get (O, "Key"))),
                           New_Item => Element_Type'(Get (Get (O, "Element"))));
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

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Ordered_Maps;
