package body GNATCOLL.JSON.Support.Ada.Calendar is

   ------------
   -- Create --
   ------------

   function Create (Val : Time) return JSON_Value is
   begin
      return Create (To_Internal_Time (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Time is
   begin
      return To_Time (Get (Val));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Time
   is
   begin
      return To_Time (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Time)
   is
   begin
      Set_Field (Val, Field_Name, To_Internal_Time (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create
     (Val : Internal_Time)
      return JSON_Value
   is
   begin
      return Ret : JSON_Value := Create_Object do
         Set_Field (Ret, "Year", Val.Year);
         Set_Field (Ret, "Month", Val.Month);
         Set_Field (Ret, "Day",  Val.Day);
         Set_Field (Ret, "Seconds", Float (Val.Seconds));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Internal_Time is
   begin
      return Ret : Internal_Time do
         Ret.Year := Get (Val, "Year");
         Ret.Month := Get (Val, "Month");
         Ret.Day := Get (Val, "Day");
         Ret.Seconds := Duration (Float'(Get (Val, "Seconds")));
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Internal_Time
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
      Field      : Internal_Time)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   -------------
   -- To_Time --
   -------------

   function To_Time (Src : Internal_Time) return Time is
   begin
      return Time_Of (Src.Year, Src.Month, Src.Day, Src.Seconds);
   end To_Time;

   ----------------------
   -- To_Internal_Time --
   ----------------------

   function To_Internal_Time (Src : Time) return Internal_Time is
   begin
      return Ret : Internal_Time do
         Split (Date => Src, Year => Ret.Year, Month => Ret.Month, Day => Ret.Day, Seconds => Ret.Seconds);
      end return;
   end To_Internal_Time;

end GNATCOLL.JSON.Support.Ada.Calendar;
