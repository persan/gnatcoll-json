
with GNATCOLL.JSON.Support.Ada.Containers.Vectors;
use GNATCOLL.JSON;
package Data.JSON is
   function Create (Val : Rec) return JSON_Value;
   function Get (Val : JSON_Value) return Rec;

   function Get (Val : JSON_Value; Field : UTF8_String) return Rec;
   procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : Rec);

private

   package Foxes_JSON is new GNATCOLL.JSON.Support.Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type =>  Natural,
      "="          => "=",
      V            => Foxes,
      Create       => Create,
      Get          => Get,
      Get_Name     => Get,
      Set_Field    => Set_Field);


end Data.JSON;
