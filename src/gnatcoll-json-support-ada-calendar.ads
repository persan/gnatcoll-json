with Ada.Calendar;
package GNATCOLL.JSON.Support.Ada.Calendar is
   use Standard.Ada.Calendar;

   function Create (Val : Time) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Time with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Time with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Time) with Inline_Always;
private

   type Internal_Time is record
      Year    : Standard.Ada.Calendar.Year_Number;
      Month   : Standard.Ada.Calendar.Month_Number;
      Day     : Standard.Ada.Calendar.Day_Number;
      Seconds : Standard.Ada.Calendar.Day_Duration;
   end record;

   function Create (Val : Internal_Time) return JSON_Value with Inline_Always;
   function Get (Val : JSON_Value) return Internal_Time with Inline_Always;
   function Get (Val   : JSON_Value;
                 Field : UTF8_String) return Internal_Time with Inline_Always;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String; Field  : Internal_Time) with Inline_Always;

   function To_Time (Src : Internal_Time) return Time;
   function To_Internal_Time (Src : Time) return Internal_Time;

end GNATCOLL.JSON.Support.Ada.Calendar;
