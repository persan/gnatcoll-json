with Ada.Command_Line;
with Ada.Directories;
with GNAT.Source_Info;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Streams.Stream_IO;
package body GNATCOLL.JSON.Builder.Templates is
   use Ada.Directories;
   -------------------------
   -- Get_Template_Folder --
   -------------------------
   function Get_Filename (S : String := GNAT.Source_Info.Enclosing_Entity) return String is
      Lc : constant String := Ada.Strings.Fixed.Translate (S, Ada.Strings.Maps.Constants.Lower_Case_Map);
      Ix : Natural := Ada.Strings.Fixed.Index (Lc, ".", Lc'Last, Ada.Strings.Backward);
   begin
      return Ret : String := Lc (Ix + Ix .. Lc'Last) do
         Ix := Ada.Strings.Fixed.Index (Ret, "_", Ret'Last, Ada.Strings.Backward);
         Ret (Ix) := '.';
      end return;
   end Get_Filename;

   function Get_Template_Folder return String is
   begin
      return Compose (Compose (Compose (Containing_Directory (Containing_Directory (Ada.Command_Line.Command_Name)), "share"), "json-builder"), "templates");
   end Get_Template_Folder;

   function SIGNED_INT_TYPE_Template return String is
      Template_Path : constant String := Compose (Get_Template_Folder, Get_Filename);
      Inf           : Ada.Streams.Stream_IO.File_Type;

   begin
      Inf.Open (Ada.Streams.Stream_IO.In_File, Template_Path);
      return Ret : String (1 .. Natural (Inf.Size)) do
         String'Read (Inf.Stream, Ret);
         Inf.Close;
      end return;
      --  return
      --    "   --  -------------------------------------------------------------------------" & ASCII.LF &
      --    "   --  @_Name_@" & ASCII.LF &
      --    "   package @_Name_@_JSON_Impl is new GNATCOLL.JSON.Support.Integer_Generic (@_Name_@);" & ASCII.LF &
      --    "   function Create (Val : @_Name_@) return JSON_Value renames @_Name_@_JSON_Impl.Create;" & ASCII.LF &
      --    "   function Get (Val : JSON_Value) return @_Name_@ renames @_Name_@_JSON_Impl.Get;" & ASCII.LF &
      --    "   function Get (Val : JSON_Value; Field : UTF8_String) return @_Name_@ renames @_Name_@_JSON_Impl.Get;" & ASCII.LF &
      --    "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : @_Name_@) renames @_Name_@_JSON_Impl.Set_Field;" & ASCII.LF &
   end;

end GNATCOLL.JSON.Builder.Templates;
