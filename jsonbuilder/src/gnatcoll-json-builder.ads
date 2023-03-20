with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;
with Ada.Strings.Unbounded;
with GNATCOLL.Opt_Parse;
package GNATCOLL.JSON.Builder is

   Version : constant String := "3.0";

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array);

   package App is new Libadalang.Helpers.App
     (Name         => "json-builder",
      Description  => "JSON Builder. Generates JSON serialisation/serialisation routines for datatypes in an Ada-specs.",
      App_Setup    => App_Setup,
      Process_Unit => Process_Unit);

   package Args is
      use GNATCOLL.Opt_Parse;
      Default_Output_Dir : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (".");

      package Output_Dir is new Parse_Option
        (App.Args.Parser, "-o", "--output-dir",
         Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
         Default_Val => Default_Output_Dir,
         Help        => "Project file to use");
   end Args;

end GNATCOLL.JSON.Builder;
