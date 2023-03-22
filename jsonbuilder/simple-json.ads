pragma Ada_2022;
pragma Ada_2022;
--  --------------------------------------------------------------------
--   header for Simple.JSON
--  --------------------------------------------------------------------

with GNATCOLL.JSON.Support.Arrays_Generic;
with GNATCOLL.JSON.Support.Enumeration_Generic;
with GNATCOLL.JSON.Support.Integer_Generic;
with GNATCOLL.JSON.Support.Modular_Generic;
package Simple.JSON is
   pragma Elaborate_Body;
   use GNATCOLL.JSON;

   --  -------------------------------------------------------------------------
   --  AA
   --
   package AA_JSON_Impl is new GNATCOLL.JSON.Support.Integer_Generic (AA);
   function Create (Val : AA) return JSON_Value renames AA_JSON_Impl.Create;
   function Get (Val : JSON_Value) return AA renames AA_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return AA renames AA_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : AA) renames AA_JSON_Impl.Set_Field;

   --  ---------------------------------------------------------------
   --  BB
   --
   package BB_JSON_Impl is new GNATCOLL.JSON.Support.Modular_Generic (BB);
   function Create (Val : BB) return JSON_Value renames BB_JSON_Impl.Create;
   function Get (Val : JSON_Value) return BB renames BB_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return BB renames BB_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : BB) renames BB_JSON_Impl.Set_Field;

   --  ---------------------------------------------------------------
   --  Aa_Array
   --
   package Aa_Array_JSON_Impl is new GNATCOLL.JSON.Support.Arrays_Generic
      (Natural, AA, Aa_Array, Create, Get, Create, Get);
   function Create (Val : Aa_Array) return JSON_Value renames Aa_Array_JSON_Impl.Create;
   function Get (Val : JSON_Value) return Aa_Array renames Aa_Array_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Aa_Array renames Aa_Array_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Aa_Array) renames Aa_Array_JSON_Impl.Set_Field;

   --  ---------------------------------------------------------------
   --  Enum
   --
   package Enum_JSON_Impl is new GNATCOLL.JSON.Support.Enumeration_Generic (Enum);
   function Create (Val : Enum) return JSON_Value renames Enum_JSON_Impl.Create;
   function Get (Val : JSON_Value) return Enum renames Enum_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Enum renames Enum_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Enum) renames Enum_JSON_Impl.Set_Field;

   --  ---------------------------------------------------------------
   --  My_Mod
   --
   package My_Mod_JSON_Impl is new GNATCOLL.JSON.Support.Modular_Generic (My_Mod);
   function Create (Val : My_Mod) return JSON_Value renames My_Mod_JSON_Impl.Create;
   function Get (Val : JSON_Value) return My_Mod renames My_Mod_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return My_Mod renames My_Mod_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : My_Mod) renames My_Mod_JSON_Impl.Set_Field;

   --  ---------------------------------------------------------------
   --  Simple_Record record

   --  ---------------------------------------------------------------
   --  Abstract_Record record

   --  ---------------------------------------------------------------
   --  Concrete_Taggd_Record derived

   --  ---------------------------------------------------------------
   --  Concrete_Taggd_Record_With_Time derived

   --  ---------------------------------------------------------------
   --  Record_With_Discriminatns record

   --  ---------------------------------------------------------------
   --  Private_Type derived



end Simple.JSON;
