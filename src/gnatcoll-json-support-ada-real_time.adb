with Ada.Calendar;
with GNATCOLL.JSON.Support.Ada.Calendar;

package body GNATCOLL.JSON.Support.Ada.Real_Time is

   use Standard.Ada.Calendar;

   Real_Time_Epoc : constant Standard.Ada.Real_Time.Time := Standard.Ada.Real_Time.Clock;
   Calendar_Epoc  : constant Standard.Ada.Calendar.Time := Standard.Ada.Calendar.Clock;

   function To_Calendar_Time ( T : Standard.Ada.Real_Time.Time) return Standard.Ada.Calendar.Time is
   begin
      return Calendar_Epoc + To_Duration (T - Real_Time_Epoc);
   end;

   function To_Real_Time_Time ( T : Standard.Ada.Calendar.Time) return Standard.Ada.Real_Time.Time is
   begin
      return Real_Time_Epoc + To_Time_Span (T - Calendar_Epoc);
   end;


   ------------
   -- Create --
   ------------
   function Create
     (Val : Standard.Ada.Real_Time.Time)
      return JSON_Value
   is
   begin
      return GNATCOLL.JSON.Support.Ada.Calendar.Create (To_Calendar_Time (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Standard.Ada.Real_Time.Time is
   begin
      return To_Real_Time_Time (GNATCOLL.JSON.Support.Ada.Calendar.Get (Val));
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
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      raise Program_Error with "Unimplemented function Create";
      return Create (Val => Val);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Time_Span is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      raise Program_Error with "Unimplemented function Get";
      return Get (Val => Val);
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
