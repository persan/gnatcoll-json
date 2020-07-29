pragma Ada_2012;
procedure GNATCOLL.JSON.Support.Gen_Map_JSON_Object
  (Val         : JSON_Value;
   CB          : access procedure
     (User_Object : in out Mapped; Name : UTF8_String; Value : JSON_Value);
   User_Object : in out Mapped)
is
   procedure Internal (Name : UTF8_String; Value : JSON_Value);

   --------------
   -- Internal --
   --------------

   procedure Internal (Name : UTF8_String; Value : JSON_Value) is
   begin
      CB (User_Object, Name, Value);
   end Internal;

begin
   Map_JSON_Object (Val, Internal'Access);
end GNATCOLL.JSON.Support.Gen_Map_JSON_Object;
