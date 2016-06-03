with Ada.Numerics.Generic_Real_Arrays;
generic
   with package A is new Standard.Ada.Numerics.Generic_Real_Arrays (<>);
   use A;
package GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays is

   function Create (Val : Real_Vector) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Real_Vector with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Real_Vector with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Vector) with Inline_Always;



   function Create (Val : Real_Matrix) return JSON_Value with Inline_Always;

   function Get (Val : JSON_Value) return Real_Matrix with Inline_Always;

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String) return Real_Matrix with Inline_Always;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Matrix) with Inline_Always;

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays;
