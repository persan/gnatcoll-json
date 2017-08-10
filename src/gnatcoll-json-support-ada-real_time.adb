
package body GNATCOLL.JSON.Support.Ada.Real_Time is

   ------------
   -- Create --
   ------------
   function Create
     (Val : Standard.Ada.Real_Time.Time)
      return JSON_Value
   is
      SC : Seconds_Count;
      TS : Time_Span;
   begin
      Split (Val, SC, TS);
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "Seconds_Count", Long_Integer (SC));
         Set_Field (Ret, "Time_Span", TS);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Standard.Ada.Real_Time.Time is
      SC : constant Seconds_Count := Seconds_Count (Long_Integer'(Get (Val, "Seconds_Count")));
      TS : constant Time_Span := Get (Val, "Time_Span");
   begin

      return Time_Of (SC, TS);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Standard.Ada.Real_Time.Time
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Standard.Ada.Real_Time.Time)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create
     (Val : Time_Span)
      return JSON_Value
   is
   begin
      return Create (Long_Float (To_Duration (Val)));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Time_Span is
   begin
      return To_Time_Span (Duration (Get_Long_Float (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Time_Span
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Time_Span)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Real_Time;
