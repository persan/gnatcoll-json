with Interfaces.C;
with GNATCOLL.JSON.Support.Integer_Generic;
with GNATCOLL.JSON.Support.Modular_Generic;
with GNATCOLL.JSON.Support.Float_Generic;
package GNATCOLL.JSON.Support.Interfaces.C is
   use Standard.Interfaces.C;

   package Int_Impl is new GNATCOLL.JSON.Support.Integer_Generic (int);
   function Create (Val : int) return JSON_Value renames Int_Impl.Create;
   function Get (Val : JSON_Value) return int renames Int_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return int renames Int_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : int) renames Int_Impl.Set_Field;

   package Short_Impl is new GNATCOLL.JSON.Support.Integer_Generic (short);
   function Create (Val : short) return JSON_Value renames Short_Impl.Create;
   function Get (Val : JSON_Value) return short renames Short_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return short renames Short_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : short) renames Short_Impl.Set_Field;

   package Long_Impl is new GNATCOLL.JSON.Support.Integer_Generic (long);
   function Create (Val : long) return JSON_Value renames Long_Impl.Create;
   function Get (Val : JSON_Value) return long renames Long_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return long renames Long_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : long) renames Long_Impl.Set_Field;

   package Signed_Char_Impl is new GNATCOLL.JSON.Support.Integer_Generic (signed_char);
   function Create (Val : signed_char) return JSON_Value renames Signed_Char_Impl.Create;
   function Get (Val : JSON_Value) return signed_char renames Signed_Char_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return signed_char renames Signed_Char_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : signed_char) renames Signed_Char_Impl.Set_Field;

   package Unsigned_Impl is new GNATCOLL.JSON.Support.Modular_Generic (unsigned);
   function Create (Val : unsigned) return JSON_Value renames Unsigned_Impl.Create;
   function Get (Val : JSON_Value) return unsigned renames Unsigned_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return unsigned renames Unsigned_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : unsigned) renames Unsigned_Impl.Set_Field;

   package Unsigned_Short_Impl is new GNATCOLL.JSON.Support.Modular_Generic (unsigned_short);
   function Create (Val : unsigned_short) return JSON_Value renames Unsigned_Short_Impl.Create;
   function Get (Val : JSON_Value) return unsigned_short renames Unsigned_Short_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return unsigned_short renames Unsigned_Short_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : unsigned_short) renames Unsigned_Short_Impl.Set_Field;

   package Ptrdiff_T_Impl is new GNATCOLL.JSON.Support.Integer_Generic (ptrdiff_t);
   function Create (Val : ptrdiff_t) return JSON_Value renames Ptrdiff_T_Impl.Create;
   function Get (Val : JSON_Value) return ptrdiff_t renames Ptrdiff_T_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return ptrdiff_t renames Ptrdiff_T_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : ptrdiff_t) renames Ptrdiff_T_Impl.Set_Field;

   package size_t_Impl is new GNATCOLL.JSON.Support.Modular_Generic (size_t);
   function Create (Val : size_t) return JSON_Value renames size_t_Impl.Create;
   function Get (Val : JSON_Value) return size_t renames size_t_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return size_t renames size_t_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : size_t) renames size_t_Impl.Set_Field;

   package C_float_Impl is new GNATCOLL.JSON.Support.Float_Generic (C_float);
   function Create (Val : C_float) return JSON_Value renames C_float_Impl.Create;
   function Get (Val : JSON_Value) return C_float renames C_float_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return C_float renames C_float_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : C_float) renames C_float_Impl.Set_Field;

   package double_Impl is new GNATCOLL.JSON.Support.Float_Generic (double);
   function Create (Val : double) return JSON_Value renames double_Impl.Create;
   function Get (Val : JSON_Value) return double renames double_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return double renames double_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : double) renames double_Impl.Set_Field;

   package Long_Double_Impl is new GNATCOLL.JSON.Support.Float_Generic (long_double);
   function Create (Val : long_double) return JSON_Value renames Long_Double_Impl.Create;
   function Get (Val : JSON_Value) return long_double renames Long_Double_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return long_double renames Long_Double_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : long_double) renames Long_Double_Impl.Set_Field;

   --     type char is new Character;
   --     type char_array is array (size_t range <>) of aliased char;
   --
   --     type wchar_t is new Wide_Character;
   --     type wchar_array is array (size_t range <>) of aliased wchar_t;
   --
   --     type char16_t is new Wide_Character;
   --     type Char16_Array is array (Size_T range <>) of aliased Char16_T;
   --
   --     type Char32_T is new Wide_Wide_Character;
   --     type char32_array is array (size_t range <>) of aliased char32_t;
end GNATCOLL.JSON.Support.Interfaces.C;
