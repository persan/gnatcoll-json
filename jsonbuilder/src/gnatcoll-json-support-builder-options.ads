
--  This package defines variables for storing GNATCOLL.JSON.Support.Builder options and parameters,
--  as well as some internal parameters used by gnatstub

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.JSON.Support.Builder.Projects;

package GNATCOLL.JSON.Support.Builder.Options is

   Initialized : Boolean := False;
   --  set to True by Initialize, if initialization is successful

   -------------
   -- Options --
   -------------

   Alphabetical_Ordering : Boolean := False;
   --  If this flag is set ON, gnatstub orders the local bodies alphabetically.

   Indent : constant Natural := 3;
   --  Indentation in the outputted source, is not set from command line.

   No_Exception_In_Stubs : Boolean := False;
   --  '--no-exception'
   --  If this flag is ON, generated procedure stubs contain null statement
   --  instead of a raise statement.  ????

   No_Local_Comment_Headers : Boolean := False;
   --  '--no-local-header'
   --  If this flag is ON, no comment header with unit name is placed before
   --  the unit body stub.

   Overwrite_Body : Boolean := False;
   --  Should an existing body be overwritten

   type Sampler_Header is (
      No_Header,
      --  no header should be put into a body sample being created (default)
      Stand_Header,
      --  a sample comment header will be placed into a body sample
      From_Spec,
      --  a comment header from the spec will be placed into a body sampler
      From_File);
      --  a comment header is copied from a file

   Header : Sampler_Header := No_Header;

   Overwrite_Tree : Boolean := False;
   --  in case, if the current directory already contains the tree file with
   --  the name corresponding to GNATCOLL.JSON.Support.Builder parameter, indicates whether or not
   --  this file should be overwritten

   Delete_Tree : Boolean := True;
   --  indicates if GNATCOLL.JSON.Support.Builder should delete the tree file created by itself.

   Reuse_Tree : Boolean := False;
   --  indicates if the existing tree should be reused

   Indent_Level : Natural := 3;
   --  indentation level

   Max_Body_Line_Length : Natural := 79;
   --  maximum line length in the body file being generated

   Body_Form : String_Access;
   Body_O_Form : String_Access;
   --  Defines the encoding of the output file.


   ------------------------------
   -- File and Directory names --
   ------------------------------

   File_Name : String_Access;
   --  The name of file that contains processed unit. This is the only one
   --  obligatory parameter. Only one unit name may be given. The name
   --  should be the name of the source file, it has to follow the GNAT
   --  file name conventions (in particular, it has to have .ads suffix).
   --  the file name may or may not contain the path information.

   Short_File_Name : String_Access;
   --  File name without directory information

   Full_File_Name : String_Access;
   --  Fully qualified normalized name of the argument file

   Tree_Name : String_Access;
   --  we need it in more, then one routine, so we define it here
   Body_Name : String_Access;
   Spec_O_Name : String_Access;

   Full_Body_Name : String_Access;
   Full_Spec_O_Name : String_Access;
   --  Fully qualified normalized name of the body file

   Header_File_Name : String_Access;
   --  The name of the file to copy a comment header from

   Destination_Dir : String_Access;
   --  directory to put the sampler body in

   Dest_Dir_Set : Boolean := False;
   --  Is needed because of old-fashion gnatstub command line interface. Is set
   --  ON if the destination dir is set as the second argument in the command
   --  line (that is, not by "--dir=<directory_name>" option. Prevents changing
   --  destination directory by other means when analyzing parameters.

   ----------------------
   -- Status variables --
   ----------------------

   Tree_Exists : Boolean := False;
   --  if the tree file has been created or has been found as existing
   --  during the initialization

   Dir_Count : Natural := 0;
   --  the number of '-I' options in command line

   File_Name_Len         : Natural;
   File_Name_First       : Natural;
   File_Name_Last        : Natural;
   Short_File_Name_Len   : Natural;
   Short_File_Name_First : Natural;
   Short_File_Name_Last  : Natural;
   --  To simplify dealing with the spec file name

   Builder_Prj :  Projects.Builder_Project_Type;
   Num_Of_Args  : Natural := 0;

end GNATCOLL.JSON.Support.Builder.Options;
