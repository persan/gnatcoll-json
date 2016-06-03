with Ada.Numerics.Generic_Complex_Types;
generic
   with package C is new Standard.Ada.Numerics.Generic_Complex_Types (<>);
   use C;
package GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types is

   function Create (Val : Complex) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Complex with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Complex with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Complex) with Inline_Always;
end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types;
