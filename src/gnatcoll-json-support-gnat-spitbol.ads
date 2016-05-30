with GNAT.Spitbol;
package GNATCOLL.JSON.Support.GNAT.SPitbol is
   generic
      type Value_Type is private;
      Null_Value : Value_Type;

      with function Img (A : Value_Type) return String;

      with function "=" (A, B : Value_Type) return Boolean is <>;

      with package V is new Standard.GNAT.Spitbol.Table (Value_Type, Null_Value, Img, "=");

      with function Create (Val : Value_Type) return JSON_Value is <>;
      with function Get (Val : JSON_Value) return Value_Type is <>;

   package JSON_Table is

      function Create (Val : V.Table) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table;
      procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table);

      function Create (Val : V.Table_Entry) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Entry;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table_Entry;
      procedure Set_Field  (Val  : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table_Entry);

      function Create (Val : V.Table_Array) return JSON_Value;
      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Array;
      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table_Array;
      procedure Set_Field  (Val        : JSON_Value;  Field_Name : UTF8_String; Field  : V.Table_Array);
   end JSON_Table;
end GNATCOLL.JSON.Support.GNAT.SPitbol;
