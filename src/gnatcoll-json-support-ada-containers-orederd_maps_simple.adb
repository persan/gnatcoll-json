package body GNATCOLL.JSON.Support.Ada.Containers.Orederd_Maps_Simple is

   ------------
   -- Create --
   ------------

   function Create (Val : Map) return JSON_Value is
      Data : JSON_Array;
   begin
      for I in Val.Iterate loop
         declare
            O : constant JSON_Value := JSON_Null;
         begin
            O.Set_Field (Image (Key (I)), Create (Element (I)));
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
               procedure CB (Name : UTF8_String; Val : JSON_Value) is
               begin
                  Ret.Include (Value (Name), Get (Val));
               end;
            begin
               Map_JSON_Object (O, CB'Access);
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

end GNATCOLL.JSON.Support.Ada.Containers.Orederd_Maps_Simple;
