
with GNATCOLL.JSON.Support.Fixed_Generic;
with GNATCOLL.JSON.Support.Integer_Generic;
WITH GNATCOLL.JSON.Support.Ada.Containers.Vectors;
with GNATCOLL.JSON;
package Data.JSON is
   use GNATCOLL.JSON;

   package Money_Impl is new GNATCOLL.JSON.Support.Fixed_Generic (Num => Money);
   function Create (Val : Money) return JSON_Value renames Money_Impl.Create;
   function Get (Val : JSON_Value) return Money renames Money_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Money renames Money_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Money) renames Money_Impl.Set_Field;

   package UTC_Date_Impl is new GNATCOLL.JSON.Support.Integer_Generic (UTC_Date);
   function Create (Val : UTC_Date) return JSON_Value renames UTC_Date_Impl.Create;
   function Get (Val : JSON_Value) return UTC_Date renames UTC_Date_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return UTC_Date renames UTC_Date_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : UTC_Date) renames UTC_Date_Impl.Set_Field;


   function Create (Val : Trade) return JSON_Value;
   function Get (Val : JSON_Value) return Trade;
   function Get (Val : JSON_Value; Field : UTF8_String) return Trade;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Trade);

   package Vector_Trades_JSON is new GNATCOLL.JSON.Support.Ada.Containers.Vectors(Vector_Trades);
   function Create (Val : Vector_Trades.Vector) return JSON_Array renames Vector_Trades_JSON.Create;
   function Get (Val : JSON_Value) return Vector_Trades.Vector renames Vector_Trades_JSON.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Vector_Trades.Vector renames Vector_Trades_JSON.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Vector_Trades.Vector) renames Vector_Trades_JSON.Set_Field;



   function Create (Val : Reply) return JSON_Value;
   function Get (Val : JSON_Value) return Reply;
   function Get (Val : JSON_Value; Field : UTF8_String) return Reply;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Reply);


end  Data.JSON;
