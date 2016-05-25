with GNATCOLL.JSON; use GNATCOLL.JSON;
with System.Storage_Elements; use System.Storage_Elements;
package GNATCOLL.JSON.Support.System.Storage_Elements.JSON is
   function Create (Val : Storage_Offset) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Offset;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Offset;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Offset);

   function Create (Val : Storage_Element) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Element;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Element;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Element);

   function Create (Val : Storage_Array) return JSON_Value;

   function Get (Val : JSON_Value) return Storage_Array;

   function Get (Val : JSON_Value; Field : UTF8_String) return Storage_Array;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Storage_Array);

private
   function Image (Item : Storage_Element) return String  with
     Inline_Always => True;
   function Image (Item : Storage_Array) return String  with
     Inline_Always => True;
   function Image (Base   : Standard.System.Address;
                   Length : Storage_Offset) return String
     with Inline_Always => True;
   function Value (Item : String) return Storage_Element  with
     Inline_Always => True;

   function Value (Item : String) return Storage_Array  with
     Inline_Always => True;

end GNATCOLL.JSON.Support.System.Storage_Elements.JSON;
