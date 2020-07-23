with Libadalang.Analysis;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Sets;
package GNATCOLL.JSON.Support.Generate.Writer is
   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   type JSON_Writer is tagged limited record
      Unit         : access Libadalang.Analysis.Ada_Node'Class;
      Package_Name : Ada.Strings.Unbounded.Unbounded_String;
      Buffer       : Ada.Strings.Unbounded.Unbounded_String;
      Withs        : String_Sets.Set;
      Outf         : Ada.Text_IO.File_Type;
      Out_folder   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   procedure Write (Self : in out JSON_Writer; Unit : Libadalang.Analysis.Ada_Node'Class);

   procedure Write_Spec (Self : in out JSON_Writer);
   procedure Write_Body (Self : in out JSON_Writer);


   procedure Process_ADA_ARRAY_TYPE_DEF_Spec (Self : in out JSON_Writer;
                                              Node : Libadalang.Analysis.Array_Type_Def;
                                              Name : String);
private
   type Field_Info is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Json_Name : Ada.Strings.Unbounded.Unbounded_String;
      Json_Skip : Boolean := False;
   end record;
   package Field_Vectors is new Ada.Containers.Vectors (Positive, Field_Info);
   type Record_Info is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Is_Abstract : Boolean := False;
      Is_Tagged   : Boolean := False;
      Parent_Full_Name : Ada.Strings.Unbounded.Unbounded_String;
      Fields           : Field_Vectors.Vector;
   end record;

end GNATCOLL.JSON.Support.Generate.Writer;
