with Ada.Streams;
with GNATCOLL.JSON.Support.Builder.String_Vectors;
package GNATCOLL.JSON.Support.Builder.Templates is

   procedure Float_Type_Spec (Type_Name : String; Outf : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Enum_Type_Spec (Type_Name : String; Outf : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Modulus_Type_Spec (Type_Name : String; Outf : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Fixed_Type_Spec (Type_Name : String; Outf : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Tagged_Type_Spec (Type_Name     : String;
      Parent        : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
                               Outf          : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Tagged_Type_Body (Type_Name : String;
      Parent        : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
                               Outf          : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Record_Type_Spec (Type_Name : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
                               Outf          : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Record_Type_Body (Type_Name : String;
      Discriminants : String_Vectors.Vector;
      Fields        : String_Vectors.Vector;
                               Outf          : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Array_Type_Spec (Type_Name : String; Outf : not null access Ada.Streams.Root_Stream_Type'Class);

end GNATCOLL.JSON.Support.Builder.Templates;
