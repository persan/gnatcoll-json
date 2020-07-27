with GNATCOLL.JSON.Support.Arrays_Generic;
with GNATCOLL.JSON.Support.Enumeration_Generic;
with GNATCOLL.JSON.Support.Integer_Generic;
with GNATCOLL.JSON.Support.Modular_Generic;
with GNATColl.JSON;

package Simple.JSON_Golden is
   use GNATColl.JSON;

   --  -------------------------------------------------------------------------
   --  AA
   --
   package AA_JSON_Impl is new GNATCOLL.JSON.Support.Integer_Generic (AA);
   function Create (Val : AA) return JSON_Value renames AA_JSON_Impl.Create;
   function Get (Val : JSON_Value) return AA renames AA_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return AA renames AA_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : AA) renames AA_JSON_Impl.Set_Field;
   --  -------------------------------------------------------------------------


   --  ------------------------------------
   --  Aa_Array
   --  ------------------------------------
   package Aa_Array_JSON_Impl is new GNATCOLL.JSON.Support.Arrays_Generic
      (Natural,AA,Aa_Array, Create, Get, Create, Get);
   function Create (Val : Aa_Array) return JSON_Value renames Aa_Array_JSON_Impl.Create;
   function Get (Val : JSON_Value) return Aa_Array renames Aa_Array_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Aa_Array renames Aa_Array_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Aa_Array) renames Aa_Array_JSON_Impl.Set_Field;




   --  -------------------------------------------------------------------------
   --  Enum
   --
   package Enum_JSON_Impl is new GNATCOLL.JSON.Support.Enumeration_Generic (Enum);
   function Create (Val : Enum) return JSON_Value renames Enum_JSON_Impl.Create;
   function Get (Val : JSON_Value) return Enum renames Enum_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return Enum renames Enum_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Enum) renames Enum_JSON_Impl.Set_Field;
   --  -------------------------------------------------------------------------


   --  -------------------------------------------------------------------------
   --  My_Mod
   --
   package My_Mod_JSON_Impl is new GNATCOLL.JSON.Support.Modular_Generic (My_Mod);
   function Create (Val : My_Mod) return JSON_Value renames My_Mod_JSON_Impl.Create;
   function Get (Val : JSON_Value) return My_Mod renames My_Mod_JSON_Impl.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return My_Mod renames My_Mod_JSON_Impl.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : My_Mod) renames My_Mod_JSON_Impl.Set_Field;
   --  -------------------------------------------------------------------------


   --  -------------------------------------------------------------------------
   --  Simple_Record
   --
   function Create (Val : Simple_Record) return JSON_Value;
   function Get (Val : JSON_Value) return Simple_Record;
   function Get (Val : JSON_Value; Field : UTF8_String) return Simple_Record;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Simple_Record);
   procedure Set_Fields (Val : JSON_Value; Data : Simple_Record);

   procedure Map_JSON_Value (User_Object : in out Simple_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value);
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Simple_Record);


   --  -------------------------------------------------------------------------
   --  Abstract_Record
   --
   procedure Map_JSON_Value (User_Object : in out Abstract_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value);
   procedure Set_Fields (Val : JSON_Value; Data : Abstract_Record);


   --  -------------------------------------------------------------------------
   --  Concrete_Taggd_Record
   --
   function Create (Val : Concrete_Taggd_Record) return JSON_Value;
   function Get (Val : JSON_Value) return Concrete_Taggd_Record;
   function Get (Val : JSON_Value; Field : UTF8_String) return Concrete_Taggd_Record;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Concrete_Taggd_Record);
   procedure Map_JSON_Value (User_Object : in out Concrete_Taggd_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value);
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Concrete_Taggd_Record);

   procedure Set_Fields (Val : JSON_Value; Data : Concrete_Taggd_Record);

   --  -------------------------------------------------------------------------


   --  -------------------------------------------------------------------------
   --  Concrete_Taggd_Record_with_Time
   --
   function Create (Val : Concrete_Taggd_Record_with_Time) return JSON_Value;
   function Get (Val : JSON_Value) return Concrete_Taggd_Record_with_Time;
   function Get (Val : JSON_Value; Field : UTF8_String) return Concrete_Taggd_Record_with_Time;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Concrete_Taggd_Record_with_Time);

   procedure Map_JSON_Value (User_Object : in out Concrete_Taggd_Record_with_Time;
                             Name        : UTF8_String;
                             Value       : JSON_Value);
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Concrete_Taggd_Record_with_Time);

   procedure Set_Fields (Val : JSON_Value; Data : Concrete_Taggd_Record_with_Time);
--  -------------------------------------------------------------------------

   function Create (Val : Record_With_Discriminatns) return JSON_Value;
   function Get (Val : JSON_Value) return Record_With_Discriminatns;
   function Get (Val : JSON_Value; Field : UTF8_String) return Record_With_Discriminatns;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Record_With_Discriminatns);

   procedure Map_JSON_Value (User_Object : in out Record_With_Discriminatns;
                             Name        : UTF8_String;
                             Value       : JSON_Value);
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Record_With_Discriminatns);

   procedure Set_Fields (Val : JSON_Value; Data : Record_With_Discriminatns);

end Simple.JSON_Golden;
