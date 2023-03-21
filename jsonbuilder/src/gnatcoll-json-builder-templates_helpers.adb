with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.Source_Info;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
package body GNATCOLL.JSON.Builder.Templates_Helpers is

   use Ada.Directories;
   -------------------------
   -- Get_Template_Folder --
   -------------------------
   function Get_Filename (S : String := GNAT.Source_Info.Enclosing_Entity) return String is
      Lc : constant String := Ada.Strings.Fixed.Translate (S, Ada.Strings.Maps.Constants.Lower_Case_Map);
      Ix : Natural := Ada.Strings.Fixed.Index (Lc, ".", Lc'Last, Ada.Strings.Backward);
   begin
      return Ret : String := Lc (Ix + 1 .. Lc'Last) do
         Ix := Ada.Strings.Fixed.Index (Ret, "_", Ret'Last, Ada.Strings.Backward);
         Ret (Ix) := '.';
      end return;
   end Get_Filename;

   function Get_Template_Folder return String is
   begin
      return Compose (Compose (Compose (Containing_Directory (Containing_Directory (Ada.Command_Line.Command_Name)), "share"), "json-builder"), "templates");
   end Get_Template_Folder;

   procedure Create (Path : String) is
      F : Ada.Text_IO.File_Type;
   begin
      F.Create (Ada.Text_IO.Out_File, Path);
      F.Put_Line ("@@-- ----------------------------------");
      F.Put_Line ("@@-- Auto template for json-builder.");
      F.Put_Line ("@@-- To be completed.");
      F.Put_Line ("@@-- ----------------------------------");
      F.Close;
   end;
   ---------------------------
   -- Template_Path_Generic --
   ---------------------------

   function Template_Path_Generic return String is
   begin
      return Ret : constant String := Compose (Get_Template_Folder, Get_Filename) do
         if not Exists (Ret) then
            Create (Ret);
         end if;
      end return;
   end;

end GNATCOLL.JSON.Builder.Templates_Helpers;
