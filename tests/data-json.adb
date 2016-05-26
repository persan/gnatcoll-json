package body Data.JSON is
   use Foxes_JSON;
   ------------
   -- Create --
   ------------

   function Create (Val : Rec) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "Name", Val.Name);
         Set_Field (Ret, "Count", Val.Count);
         Set_Field (Ret, "Fox", Val.Fox);
      end return;
   end Create;

   function Get (Val : JSON_Value; Field : UTF8_String) return Rec is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Rec) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Rec is
   begin
      return Ret : Rec do
         Ret.Name := Get (Val, "Name");
         Ret.Count := Get (Val, "Count");
         Ret.Fox := Get (Val, "Fox");
      end return;
   end Get;

end Data.JSON;
