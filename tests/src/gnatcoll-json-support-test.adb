pragma Ada_2012;
package body GNATCOLL.JSON.Support.Test is

   ------------
   -- Create --
   ------------

   function Create (Val : Test_Data) return JSON_Value is
   begin
      return Ret : JSON_Value := Create_Object do
         Populate (Ret, Val);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Test_Data is
      Ret : Test_Data;
      procedure Cb (Name : UTF8_String; Value : JSON_Value)  is
      begin
         Map (Name, Value, Ret);
      end Cb;
   begin
      Map_JSON_Object (Val, Cb'Access);
      return Ret;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Test_Data is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value; Field_Name : UTF8_String; Field : Test_Data)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   Test_Data_I1_Field_Name : constant String := "I1";
   Test_Data_F1_Field_Name : constant String := "F1";

   procedure Populate (Item : in out JSON_Value; Val : Test_Data) is
   begin
      Set_Field (Item, Test_Data_I1_Field_Name, Val.I1);
      Set_Field (Item, Test_Data_F1_Field_Name, Val.F1);
   end Populate;

   procedure Map (Name : UTF8_String; Value : JSON_Value; Into : out Test_Data) is
   begin
      if Name = Test_Data_I1_Field_Name then
         Into.I1 := Get (Value);
      elsif Name = Test_Data_F1_Field_Name then
         Into.F1 := Get (Value);
      end if;
   end Map;

   function First_Cursor (Cont : Test_Data_Iter) return Cursor is
   begin
      return Cursor (Cont);
   end First_Cursor;
   function Advance (Cont : Test_Data_Iter; Position : Cursor) return Cursor is
      pragma Unreferenced (Cont);
   begin
      return Position + 1;
   end Advance;
   function Cursor_Has_Element (Cont : Test_Data_Iter; Position : Cursor) return Boolean is
   begin
      return Integer (Cont) <= Integer (Position);
   end Cursor_Has_Element;

   function Get_Element (Cont : Test_Data_Iter; Position : Cursor) return Test_Data is
      pragma Unreferenced (Cont);
   begin
      return Ret : Test_Data do
         Ret.I1 := Integer (Position);
         Ret.F1 := Float (Position);
      end return;
   end Get_Element;
   function Get_Test_Data (Items : Integer) return Test_Data_Iter is
   begin
      return Test_Data_Iter (Items);
   end Get_Test_Data;
end GNATCOLL.JSON.Support.Test;
