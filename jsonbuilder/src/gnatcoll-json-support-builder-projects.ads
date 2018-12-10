
--  This package adjusts the general project support for ASIS tools for
--  gnatstub needs.

pragma Ada_2012;

with GNAT.Command_Line; use GNAT.Command_Line;

with ASIS_UL.Projects;  use ASIS_UL.Projects;

package GNATCOLL.JSON.Support.Builder.Projects is

   type Builder_Project_Type is new Arg_Project_Type with null record;

   overriding function Compute_Project_Closure
     (My_Project  : Builder_Project_Type)
      return        Boolean is (False);
   --  gnatstub has exactly one argument and it requires it to be specified
   --  explicitly.

   overriding procedure Print_Tool_Usage (My_Project : Builder_Project_Type);

   overriding procedure Scan_Arguments
     (My_Project  : in out Builder_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False);
   --  This procedure differs from a typical parameter scanner for ASIS tools
   --  in the following aspects:
   --
   --  * '-U [main]' option is not supported;
   --  * '--subdirs' and '--no_objects_dir' options are not supported;
   --  * '-files=filename" is not supported

   overriding function Tool_Package_Name
     (My_Project : Builder_Project_Type)
      return       String;
   --  Returns "GNATCOLL.JSON.Support.Builder"

end GNATCOLL.JSON.Support.Builder.Projects;
