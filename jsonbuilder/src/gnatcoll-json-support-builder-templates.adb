pragma Ada_2012;
package body GNATCOLL.JSON.Support.Builder.Templates is

   ---------------------
   -- Float_Type_Spec --
   ---------------------

   procedure Simple_Type_Spec
     (Type_Name       : String;
      Support_Package : String;
      S            : not null access Ada.Streams.Root_Stream_Type'Class) is

      Impl_Name       : constant String := Type_Name & "_Impl";
   begin
      String'Write (S, "   package " & Impl_Name & " is new GNATCOLL.JSON.Support." & Support_Package & " (" & Type_Name & ");" & ASCII.LF);
      String'Write (S, "   function Create (Val : " & Type_Name & ") return JSON_Value renames " & Impl_Name & ".Create;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Type_Name & ") renames " & Impl_Name & ".Set_Field;" & ASCII.LF & ASCII.LF);
   end;

   procedure Float_Type_Spec
     (Type_Name : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Simple_Type_Spec (Type_Name, "Float_Generic",Outf);
   end Float_Type_Spec;

   --------------------
   -- Enum_Type_Spec --
   --------------------

   procedure Enum_Type_Spec
     (Type_Name : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Simple_Type_Spec (Type_Name, "Enumeration_Generic",Outf);
   end Enum_Type_Spec;

   -----------------------
   -- Modulus_Type_Spec --
   -----------------------

   procedure Modulus_Type_Spec
     (Type_Name : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Simple_Type_Spec (Type_Name, "Modular_Generic",Outf);
   end Modulus_Type_Spec;

   ---------------------
   -- Fixed_Type_Spec --
   ---------------------

   procedure Fixed_Type_Spec
     (Type_Name : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Simple_Type_Spec (Type_Name, "Fixed_Generic",Outf);
   end Fixed_Type_Spec;

   ----------------------
   -- Tagged_Type_Spec --
   ----------------------

   procedure Tagged_Type_Spec
     (Type_Name     : String;
      Parent        : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin

      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Tagged_Type_Spec unimplemented");
      raise Program_Error with "Unimplemented procedure Tagged_Type_Spec";
   end Tagged_Type_Spec;

   ----------------------
   -- Tagged_Type_Body --
   ----------------------

   procedure Tagged_Type_Body
     (Type_Name : String;
      Parent        : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Tagged_Type_Body unimplemented");
      raise Program_Error with "Unimplemented procedure Tagged_Type_Body";
   end Tagged_Type_Body;

   ----------------------
   -- Record_Type_Spec --
   ----------------------

   procedure Record_Type_Spec
     (Type_Name : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Record_Type_Spec unimplemented");
      raise Program_Error with "Unimplemented procedure Record_Type_Spec";
   end Record_Type_Spec;

   ----------------------
   -- Record_Type_Body --
   ----------------------

   procedure Record_Type_Body
     (Type_Name : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Record_Type_Body unimplemented");
      raise Program_Error with "Unimplemented procedure Record_Type_Body";
   end Record_Type_Body;

   ---------------------
   -- Array_Type_Spec --
   ---------------------

   procedure Array_Type_Spec
     (Index_Type   : String;
      Element_Type : String;
      Array_Type   : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      Impl_Name       : constant String := Type_Name & "_Impl";
   begin
      String'Write (S, "   package " & Impl_Name & " is new GNATCOLL.JSON.Support.Arrays_Generic ( Index_Type => " & ", Element_Type => " & Element_Type & ", Array_Type =>" & Array_Type & ");" & ASCII.LF);
      String'Write (S, "   function Create (Val : " & Type_Name & ") return JSON_Value renames " & Impl_Name & ".Create;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Type_Name & ") renames " & Impl_Name & ".Set_Field;" & ASCII.LF & ASCII.LF);
   end Array_Type_Spec;

   ---------------------
   -- Array_Type_Spec --
   ---------------------

   procedure Simple_Array_Type_Spec
     (Index_Type   : String;
      Element_Type : String;
      Array_Type   : String;
      Outf : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      Impl_Name       : constant String := Type_Name & "_Impl";
   begin
      String'Write (S, "   package " & Impl_Name & " is new GNATCOLL.JSON.Support.Simple_Arrays_Generic ( Index_Type => " & ", Element_Type => " & Element_Type & ", Array_Type =>" & Array_Type & ");" & ASCII.LF);
      String'Write (S, "   function Create (Val : " & Type_Name & ") return JSON_Value renames " & Impl_Name & ".Create;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Type_Name & " renames " & Impl_Name & ".Get;" & ASCII.LF);
      String'Write (S, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Type_Name & ") renames " & Impl_Name & ".Set_Field;" & ASCII.LF & ASCII.LF);
   end Array_Type_Spec;

end GNATCOLL.JSON.Support.Builder.Templates;
