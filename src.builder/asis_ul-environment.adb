------------------------------------------------------------------------------
--                                                                          --
--                     ASIS UTILITY LIBRARY COMPONENTS                      --
--                                                                          --
--                  A S I S _ U L . E N V I R O N M E N T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2004-2015, AdaCore                      --
--                                                                          --
-- Asis Utility Library (ASIS UL) is free software; you can redistribute it --
-- and/or  modify  it  under  terms  of  the  GNU General Public License as --
-- published by the Free Software Foundation; either version 3, or (at your --
-- option)  any later version.  ASIS UL  is distributed in the hope that it --
-- will  be  useful,  but  WITHOUT  ANY  WARRANTY; without even the implied --
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the --
-- GNU  General Public License for more details. You should have received a --
-- copy of the  GNU General Public License  distributed with GNAT; see file --
-- COPYING3. If not,  go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- ASIS UL is maintained by AdaCore (http://www.adacore.com).               --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Characters.Conversions;  use Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Directories;             use Ada.Directories;
with Ada.Text_IO;                 use Ada.Text_IO;
with System.Multiprocessors;

with GNATCOLL.Projects;           use GNATCOLL.Projects;

with Asis.Ada_Environments;       use Asis.Ada_Environments;
with Asis.Extensions;             use Asis.Extensions;
with Asis.Implementation;         use Asis.Implementation;

with A4G.A_Debug;
with A4G.GNAT_Int;

with ASIS_UL.Common;              use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;    use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;               use ASIS_UL.Debug;
with ASIS_UL.Formatted_Output;
with ASIS_UL.Options;             use ASIS_UL.Options;
with ASIS_UL.Output;              use ASIS_UL.Output;
with ASIS_UL.Projects;            use ASIS_UL.Projects;
with ASIS_UL.Source_Table;        use ASIS_UL.Source_Table;
with ASIS_UL.Utilities;           use ASIS_UL.Utilities;

package body ASIS_UL.Environment is

   Tmpdir_Needs_To_Be_Displayed : Boolean := True;

   Tmpdir    : constant String := "TMPDIR";
   No_Dir    : aliased String  := "";
   Temp_Dir  : String_Access   := No_Dir'Access;
   --  If non-empty, points to the name of the directory to create the tool's
   --  temporary directory into. If empty, the temporary directory is created
   --  in the current directory.

   Tmp_Dir_Already_Created : Boolean := False;

   -----------------------
   -- Local subprograms --
   -----------------------

   pragma Warnings (Off, More_Arguments);
   --  At least gnatstub does not use this procedure, so we have to avoid
   --  warnings about unreferenced procedure

   procedure Check_Parameters;
   --  Checks that the tool settings are compatible with each other. All
   --  possible check are tool-specific, the corresponding subunit
   --  should be provided for each tool

   --  ???Scan_Parameters and Initialize are gone; the *_New version should
   --  be renamed.
   --  These two procedures Scan_Parameters and Check_Parameters - are a part
   --  of the Initialize procedure.  The important thing is that after
   --  finishing Initialize either the source table should contain at least one
   --  name of an existing file (in case if the tool is based on
   --  ASIS_UL.Several_Files_Driver driver), or ASIS_UL.Common.Arg_File should
   --  point to an existing file (in case if the tool is based on
   --  ASIS_UL.One_Arg_Driver driver procedure). File names in both cases
   --  should be full normalized names.
   --
   --  ASIS_UL.Compiler_Options.Arg_List should collect all the needed options
   --  to call gcc for tree creation

   procedure Tool_Specific_Initialization_1;
   procedure Tool_Specific_Initialization_2;
   --  Do the initialization actions that are specific for a tool. The first
   --  subprogram is called before reading the tool command-line parameters,
   --  the second - when the command-line parameters have just been read in and
   --  analyzed. If the tool needs any specific initialization actions, the
   --  corresponding subunits should be provided for these subprograms.

   procedure Set_Tree_Name;
   --  This procedure is supposed to be called when a tree file has just been
   --  created for the Ada source which (full normalized) name is contained
   --  in ASIS_UL.Common.Arg_File. It sets into ASIS_UL.Common.Tree_File the
   --  (short) name of the corresponding tree file.

   function Builder_Command_Line return Argument_List;
   --  Called by the outer invocation of an ASIS tool when in Incremental_Mode.
   --  Returns a sequence of command-line arguments suitable for invoking the
   --  builder. See also the comments on ASIS_UL.Options.Incremental_Mode.

   --------------------------
   -- Builder_Command_Line --
   --------------------------

   function Builder_Command_Line return Argument_List is
      pragma Assert (Incremental_Mode);

      Builder_Args, Inner_Args : String_Vector;
      --  Builder_Args are passed to the builder. Inner_Args are passed to the
      --  inner invocations of the ASIS tool by passing them to the builder
      --  after "-cargs".
      Cur : Positive := 1;
      In_Gnatcheck_Rules : Boolean := False;
      --  True if the loop below is in the "-rules" section

      This_Is_Gnatcheck : constant Boolean :=
        Has_Suffix (Tool_Name.all, Suffix => "gnatcheck");
      --  Flag for special-casing gnatcheck. "Suffix" instead of "=" in case
      --  it's a cross compiler.

      use Ada.Command_Line;

   begin
      --  Tell the builder to keep quiet

      if Debug_Flag_C then
         Append (Builder_Args, "-v");
      else
         Append (Builder_Args, "-q");
      end if;

      --  Tell the builder to pretend that the ASIS tool is the compiler, and
      --  which project-file package to use.

      Append (Builder_Args,
              String'("--compiler-subst=ada," &
                        Ada.Command_Line.Command_Name));

      declare
         Pkg_Name : constant String :=
           --  Package name (in project file) corresponding to the
           --  tool. Empty string if none.
           (if Has_Suffix (Tool_Name.all, Suffix => "gnat2json")
              then "gnat2json"
            else raise Program_Error); -- other tools don't have --incremental
      begin
         Append (Builder_Args,
                 String'("--compiler-pkg-subst=" & Pkg_Name));
      end;

      --  Tell the builder to create necessary directories if they don't exist

      Append (Builder_Args, "-p");

      --  Tell the builder not to complain about missing object files. We are
      --  pretending that the ASIS tool is the compiler, but of course ASIS
      --  tools don't generate object files.

      Append (Builder_Args, "--no-object-check");

      --  Compile only

      Append (Builder_Args, "-c");

      --  Tell the builder to place ALI files in a subdirectory of the object
      --  directory -- a different subdirectory for each tool. This is
      --  necessary because otherwise the ALI files would conflict with each
      --  other. For one thing, ASIS tools run the compiler (the real one) with
      --  different switches than normal builds, so we don't want to overwrite
      --  one kind of ALI file with the other. For another thing, just because
      --  the files generated by gnat2xml are up to date doesn't mean that the
      --  files generated by gnatcheck are up to date. So if the object
      --  directory is 'obj', normal builds put ALI files in obj, "gnat2xml
      --  --incremental" puts ALI files in obj/ALI-FILES-gnat2xml, and so on.

      Append (Builder_Args,
              String'("--subdirs=" & "ALI-FILES-" & Tool_Name.all));

      --  Don't bother with code in other languages.

      Append (Builder_Args, "--restricted-to-languages=ada");

      --  Recompile if switches have changed

      Append (Builder_Args, "-s");

      --  If "-files=f" was given, append all the file names from f. It might
      --  be better to teach the builder how to use -files= directly, to avoid
      --  command-line length limitations.

      for F of Files_From_File loop
         Append (Builder_Args, F);
      end loop;

      --  Inform the inner invocation where to find input files.

      Append (Inner_Args, String'("--outer-dir=" & Tool_Current_Dir));

      --  Modify the --output-dir= switch so it uses a full pathname.
      --  Otherwise, the output files would end up in something like
      --  obj/ALI-FILES-gnat2xml.

      if Out_Dir /= null then
         Append (Inner_Args,
                 String'("--output-dir=" & Out_Dir.all));
      end if;

      --  Set the report file name for the inner invocation using a full path
      --  name, because sometimes the inner invocation has a different current
      --  directory.

      if This_Is_Gnatcheck then
         Append (Inner_Args, "-o");
         Append (Inner_Args, Get_Report_File_Name);
      end if;

      --  Now deal with command-line arguments from this invocation of an ASIS
      --  tool (the outer one). Most are copied to Builder_Args or Inner_Args.

      while Cur <= Argument_Count loop
         declare
            Arg : constant String := Argument (Cur);

            function Match
              (Check : String;
               Kind : Character := ' ';
               Builder_Arg, Inner_Arg : Boolean := False) return Boolean
              with Pre => Kind in ' ' | ':' | '=' | '!';
            --  Checks if Arg matches Check. Kind = ' ' means Arg does not have
            --  a parameter. The other possibilities for Kind mean the same
            --  thing as in GNAT.Command_Line.Getopt. If there is a match, then
            --  we move past the arg and its parameter, if any, and append them
            --  onto Builder_Args and/or Inner_Args, as indicated by the
            --  Builder_Arg and Inner_Arg flags. Return True iff there is a
            --  match. To ignore an argument, leave Builder_Arg and Inner_Arg
            --  False.

            function Match
              (Check : String;
               Kind : Character := ' ';
               Builder_Arg, Inner_Arg : Boolean := False) return Boolean is

               procedure App;
               procedure App is
               begin
                  if Builder_Arg then
                     Append (Builder_Args, Argument (Cur));
                  end if;
                  if Inner_Arg then
                     Append (Inner_Args, Argument (Cur));
                  end if;
               end App;

               Old_Cur : constant Positive := Cur;
            begin
               case Kind is
                  when ' ' =>
                     if Arg = Check then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when ':' =>
                     if Arg = Check then
                        App;
                        Cur := Cur + 1;
                        App;
                        Cur := Cur + 1;
                     elsif Has_Prefix (Arg, Prefix => Check) then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when '=' =>
                     if Arg = Check then
                        App;
                        Cur := Cur + 1;
                        App;
                        Cur := Cur + 1;
                     elsif Has_Prefix (Arg, Prefix => Check & "=") then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when '!' =>
                     if Has_Prefix (Arg, Prefix => Check) then
                        App;
                        Cur := Cur + 1;
                     end if;

                  when others => raise Program_Error;
               end case;

               return Cur /= Old_Cur;
            end Match;

         begin
            --  We shouldn't be seeing -c, -gnatea, -gnatez, -gnatc, -gnatec,
            --  -gnatem, or -gnateO here; those are passed by gnatmake or
            --  gprbuild to the inner invocations, whereas we're in the outer
            --  one. Might as well ignore them. "--incremental" needs to be
            --  ignored; the inner invocations shouldn't get here.
            --  "--output-dir" is handled specially above. Project-related
            --  arguments go in Builder_Args. Non-switches (i.e. file names) go
            --  in Builder_Args. Anything else goes in Inner_Args.
            --
            --  We ignore -files because that's covered by Files_From_File
            --  above.

            if Match ("-c", ' ')
              or else Match ("-gnatea", ' ')
              or else Match ("-gnatez", ' ')
              or else Match ("-gnatc", '!')
              or else Match ("-gnatec", '!')
              or else Match ("-gnatem", '!')
              or else Match ("-gnateO", '!')
              or else Match ("--incremental", ' ')
              or else Match ("--output-dir", '=')
              or else Match ("-files", '=')

              or else Match ("-P", ':', Builder_Arg => True)
              or else Match ("-U", ' ', Builder_Arg => True)
              or else Match ("-X", '!', Builder_Arg => True)
              or else Match ("--subdirs", '=', Builder_Arg => True)

              or else Match ("--no_objects_dir", ' ', Inner_Arg => True)
              or else Match ("-o", '=', Inner_Arg => True)
            then
               null; -- One of those Matches already Appended if appropriate
            elsif Match ("-j", '!', Builder_Arg => True) then
               --  The Match already Appended to Builder_Args, but we also want
               --  to pass --outer-parallel to the inner invocation.
               Append (Inner_Args, "--outer-parallel");
            elsif not Has_Prefix (Arg, Prefix => "-") then
               --  If it doesn't look like a switch (e.g. a source file name),
               --  we want to pass it to the builder, except that gnatcheck
               --  rules like "+Blah", should go to the inner tool invocation.
               if In_Gnatcheck_Rules then
                  Append (Inner_Args, Arg);
               else
                  Append (Builder_Args, Arg);
               end if;
               Cur := Cur + 1;
            elsif Arg = "-cargs" then
               --  We can't just pass -cargs, because it would be hi-jacked by
               --  the builder. So we pass -inner-cargs instead. The inner
               --  invocation will then use this different arg when it
               --  processes Process_cargs_Section. So for example if our
               --  (outer) args are:
               --
               --      some_file.adb -cargs -gnat2012
               --
               --  we will pass:
               --
               --      ... -cargs some_file.adb -inner-cargs -gnat2012 ...
               --
               --  to the builder, which will then pass:
               --
               --      ... some_file.adb -inner-cargs -gnat2012 ...
               --
               --  to the inner invocation.

               Append (Inner_Args, "-inner-cargs");
               Cur := Cur + 1;
            else
               pragma Assert (Has_Prefix (Arg, Prefix => "-"));
               --  Here for all switches, including -d (debug switches). Pass
               --  the switch along to the inner invocation. In addition, pass
               --  -dn (keep temp files) along to the builder.
               Append (Inner_Args, Arg);
               if Arg in "-dn" | "-debugn" then
                  Append (Builder_Args, "-dn");
               end if;
               Cur := Cur + 1;

               if This_Is_Gnatcheck and then Arg = "-rules" then
                  In_Gnatcheck_Rules := True;
               end if;
            end if;
         end;
      end loop;

      --  Include extra args specific to the ASIS tool

      Append (Inner_Args, Extra_Inner_Post_Args);
      Prepend (Inner_Args, Extra_Inner_Pre_Args);

      --  -cargs means to pass the following arguments along to the ASIS tool

      if Last_Index (Inner_Args) > 0 then
         Prepend (Inner_Args, "-cargs:Ada");
      end if;

      --  Finally, construct the result, which is basically
      --  "Builder_Args & Inner_Args".

      return Result : Argument_List
        (1 .. Last_Index (Builder_Args) + Last_Index (Inner_Args))
      do
         declare
            X : Natural := 0;
         begin
            for Arg of Builder_Args loop
               X := X + 1;
               Result (X) := new String'(Arg);
            end loop;

            for Arg of Inner_Args loop
               X := X + 1;
               Result (X) := new String'(Arg);
            end loop;

            pragma Assert (X = Result'Last);
         end;
      end return;
   end Builder_Command_Line;

   ------------------
   -- Call_Builder --
   ------------------

   procedure Call_Builder is
   begin
      if not A4G.GNAT_Int.Execute
        (ASIS_UL.Common.Gprbuild_To_Call,
         Builder_Command_Line,
         Display_Call => ASIS_UL.Debug.Debug_Flag_C)
      then
         raise ASIS_UL.Common.Fatal_Error;
         --  Presumably the builder or one of the inner invocations printed an
         --  error message.
      end if;
   end Call_Builder;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is separate;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
      Success : Boolean := False;
   begin
      Source_Table_Debug_Image;

      if not Debug_Flag_N then

         if Get_Config_File_Name /= "" then
            Delete_File (Get_Config_File_Name, Success);

            if not Success then
               Error ("cannot remove configuration pragmas file");
            end if;
         end if;

         if Get_Mapping_File_Name /= "" then
            Delete_File (Get_Mapping_File_Name, Success);

            if not Success then
               Error ("cannot remove mapping file");
            end if;
         end if;
      end if;

      Context_Clean_Up;

      --  Clean up temporary dir

      if not Debug_Flag_N and then Tool_Temp_Dir /= null then

         Change_Dir (Tool_Current_Dir);

         for J in 1 .. 10 loop
            --  On windows, there might be a slight delay between the return of
            --  the close function on a file descriptor and the actual closing
            --  done by the system. Since it's not possible to remove a
            --  directory as long as there are handles on it, this Remove_Dir
            --  may fail. So, if a call to Remove_Dir raises Directory_Error,
            --  we try several times after some delay, and only if all the
            --  attempts fail, we generate an error message and raise an
            --  exception.

            begin
               Remove_Dir (Tool_Temp_Dir.all, Recursive => True);
               Success                 := True;
               Tmp_Dir_Already_Created := False;
               exit;
            exception
               when Directory_Error =>
                  delay 0.05;
            end;

         end loop;

         if not Success then
            --  Because of some unknown reason the temporary directory cannot
            --  be removed:
            Free (Tool_Temp_Dir);  -- to avoid cycling
            Error ("cannot remove temporary directory");
            raise Fatal_Error;
         end if;

         Free (Tool_Temp_Dir);

      end if;

   end Clean_Up;

   ----------------------
   -- Context_Clean_Up --
   ----------------------

   procedure Context_Clean_Up is
   begin

      if Is_Open (The_Context) then
         Close (The_Context);
      end if;

      if Has_Associations (The_Context) then
         Dissociate (The_Context);
      end if;

   end Context_Clean_Up;

   --------------------
   -- Go_To_Temp_Dir --
   --------------------

   procedure Go_To_Temp_Dir (With_Dir_For_Comp_Output : Boolean := False) is
      FD        : File_Descriptor;
      Temp_Name : Temp_File_Name;
      Success   : Boolean;
   begin
      if Tmp_Dir_Already_Created then
         Change_Dir (Tool_Temp_Dir.all);
         return;
      end if;

      if Temp_Dir'Length /= 0 then

         --  In verbose mode, display once the value of TMPDIR, so that
         --  if temp files cannot be created, it is easier to understand
         --  where temp files are supposed to be created.

         if ASIS_UL.Options.Verbose_Mode and then
           Tmpdir_Needs_To_Be_Displayed
         then
            Info_No_EOL ("TMPDIR = """);
            Info_No_EOL (Temp_Dir.all);
            Info        ("""");
            Tmpdir_Needs_To_Be_Displayed := False;
         end if;

         Change_Dir (Temp_Dir.all);
      end if;

      --  ??? We create the temp dir by first creating the temp file, then
      --  closing and deleting it, then creating a dir with the same name.
      --  This is not atomic as another program can sneak in between file
      --  deletion and dir creation and snatch this name for itself. This is
      --  quite unlikely and anyway we don't have any other system-independent
      --  way at the moment
      Create_Temp_File (FD, Temp_Name);
      Close (FD);
      Delete_File (Temp_Name, Success);

      if not Success then
         Error ("can not delete the temporary file that was "
              & "just created");

         raise Fatal_Error;
      end if;

      Tool_Temp_Dir := new String' -- Remove NUL
        (Normalize_Pathname
           (Temp_Name (Temp_Name'First .. Temp_Name'Last - 1)));

      Parallel_Make_Dir (Tool_Temp_Dir.all);

      Change_Dir (Tool_Current_Dir);

      if Is_Regular_File ("gnat.adc") then
         Copy_File
           (Name     => "gnat.adc",
            Pathname => Tool_Temp_Dir.all & Directory_Separator &
                        "gnat.adc",
            Success  => Success,
            Mode     => Copy);
      end if;

      Change_Dir (Tool_Temp_Dir.all);

--    pragma Assert (Project_File = null, "???The following code is not used");
      if Project_File /= null
        and then
         Project_Support_Type = Use_Tmp_Project_File
      then
--         pragma Assert (False);

         declare
            Temp_Project_File : File_Type;
         begin
            --  Creating the temporary project file
            Create (Temp_Project_File, Out_File, "tmp.gpr");

            Put (Temp_Project_File, "project Tmp extends """);
            Put (Temp_Project_File, Project_File.all);
            Put (Temp_Project_File, """ is");
            New_Line (Temp_Project_File);

            Put (Temp_Project_File, "   for Object_Dir use ""."";");
            New_Line (Temp_Project_File);

            Put (Temp_Project_File, "end Tmp;");
            New_Line (Temp_Project_File);

            Close (Temp_Project_File);

            --  Storing the temporary project file as an option:

            Store_Option ("-Ptmp.gpr");
            Set_Arg_List;

         exception
            when others =>
               Error ("can not create the temporary project file");
               raise Fatal_Error;
         end;

      end if;

      if With_Dir_For_Comp_Output then
         Create_Temp_File (FD, Temp_Name);
         Close (FD);
         Delete_File (Temp_Name, Success);

         if not Success then
            Error ("can not delete the temporary file that was "
                 & "just created");

            raise Fatal_Error;
         end if;

         Compiler_Output_File_Name := new String' -- Remove NUL
           (Temp_Name (Temp_Name'First .. Temp_Name'Last - 1) &
            Directory_Separator & "compiler_diag.txt");

         Parallel_Make_Dir (Compiler_Output_File_Name.all);
      end if;

      Tmp_Dir_Already_Created := True;

   exception
      when Directory_Error =>
         Error ("cannot create the temporary directory");
         raise Fatal_Error;
   end Go_To_Temp_Dir;

   --------------------
   -- Initialize_New --
   --------------------

   procedure Initialize_New
     (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class)
   is
   begin
      Tool_Specific_Initialization_1;
      --  A tool-specific version should be provided!

      Scan_Parameters_New (Prj);

      if Incremental_Mode then
         if not Prj.Is_Specified then
            Error ("--incremental mode requires a project file, " &
                     "and cannot be used with the gnat driver");
            raise Fatal_Error;
         end if;
      end if;

      Set_Log_File;
      Check_Parameters; --  A tool-specific version should be provided!

      Tool_Specific_Initialization_2;
      --  A tool-specific version should be provided!

      if ASIS_UL.Options.Nothing_To_Do then
         return;
      end if;

      Go_To_Temp_Dir;

      if Incremental_Mode then
         Change_Dir (Tool_Current_Dir);
      else
         Store_I_Options;
      end if;

      --  Create output directory if necessary

      if Out_Dir /= null  then
         Parallel_Make_Dir
           (Out_Dir.all, Give_Message => Verbose_Mode);
      end if;

   exception
      when Parameter_Error =>
         --  The diagnosis is already generated
         Try_Help;
         raise Fatal_Error;
      when Fatal_Error =>
         --  The diagnosis is already generated
         raise;
      when others =>
         Error ("initialization failed");
         --  Exception info will be generated in main driver
         raise;
   end Initialize_New;

   --------------------
   -- More_Arguments --
   --------------------

   function More_Arguments
     (Store_Arguments : Boolean := True;
      In_Switches     : Boolean := False;
      Parser          : Opt_Parser := Command_Line_Parser)
      return            Boolean
   is
      Result : Boolean := False;
   begin
      loop
         declare
            Arg : constant String := Get_Argument
              (Do_Expansion => True,
               Parser       => Parser);
         begin
            exit when Arg = "";
            Result := True;

            if In_Switches then
               Error ("Switches attribute cannot contain argument sources");
               raise Parameter_Error;
            end if;

            if ASIS_UL.Projects.U_Option_Set then
               ASIS_UL.Projects.Store_Main_Unit (Arg, Store_Arguments);
            else
               Store_Sources_To_Process (Arg, Store_Arguments);
            end if;
         end;
      end loop;

      return Result;
   end More_Arguments;

   ---------------------
   -- Prepare_Context --
   ---------------------

   procedure Prepare_Context (Success : out Boolean) is
   begin
      Compile
        (Source_File           => ASIS_UL.Common.Arg_File,
         Args                  => ASIS_UL.Compiler_Options.Arg_List.all,
         Success               => Success,
         GCC                   => Gcc_To_Call,
         Use_GPRBUILD          => Use_Gnatmake_To_Compile,
         Result_In_Current_Dir => Project_Support_Type = Use_Tmp_Project_File,
         Display_Call          => Debug_Flag_D);

      if not Success then
         Error ("the argument source is illegal");
      else
         Set_Tree_Name;

         Asis.Implementation.Initialize ("-ws");

         Asis.Ada_Environments.Associate
           (The_Context => The_Context,
            Name        => "",
            Parameters  => "-C1 " & To_Wide_String (Tree_File.all));

         Open (The_Context);

         if Debug_Flag_T then
            Print_Tree_Sources;
         end if;

         The_CU := Asis.Extensions.Main_Unit_In_Current_Tree (The_Context);
      end if;

   end Prepare_Context;

   ------------------------
   -- Print_Command_Line --
   ------------------------

   procedure Print_Command_Line is
      use Ada.Command_Line;
   begin
      if ASIS_UL.Options.Incremental_Mode then
         Formatted_Output.Put ("(outer)\n  ");
      end if;
      if ASIS_UL.Options.Mimic_gcc then
         Formatted_Output.Put ("(inner)\n  ");
      end if;

      Formatted_Output.Put ("current directory = \1\n", Current_Directory);
      if False then -- disable for now
         A4G.A_Debug.Print_Env;
      end if;

      Formatted_Output.Put ("  \1", Command_Name);

      for X in 1 .. Argument_Count loop
         Formatted_Output.Put (" \1", Argument (X));
      end loop;
      Formatted_Output.Put ("\n");
   end Print_Command_Line;

   -------------------------
   -- Scan_Parameters_New --
   -------------------------

   procedure Scan_Parameters_New
     (Prj : in out ASIS_UL.Projects.Arg_Project_Type'Class)
   is
      procedure Pre_Scan_Arguments;
      --  Set various flags, such as the Debug_Flag_X's, based on the
      --  command-line options. This is done before the normal 2-pass argument
      --  scan, so we can use these flags during the argument scan.

      procedure Pre_Scan_Arguments is
         use Ada.Command_Line;
      begin
         for X in 1 .. Argument_Count loop
            declare
               Arg : constant String := Argument (X);
               pragma Assert (Arg'First = 1);
            begin
               --  gnatmetric uses "-debug" instead of "-d"
               if Has_Suffix (Tool_Name.all, Suffix => "gnatmetric") then
                  if Arg'Length = 7 and then Arg (1 .. 6) = "-debug" then
                     Set_Debug_Options (Arg (7 .. 7));
                  end if;
               else
                  if Arg'Length = 3 and then Arg (1 .. 2) = "-d" then
                     Set_Debug_Options (Arg (3 .. 3));
                  end if;
               end if;

               if Arg = "--incremental" then
                  Incremental_Mode := True;
               elsif Has_Prefix (Arg, Prefix => "--outer-dir=") then
                  --  We use --outer-dir to detect that we were called from
                  --  gprbuild.
                  Mimic_gcc := True;
               end if;
            end;
         end loop;

         --  We need to ignore --incremental in the inner invocation, because
         --  --incremental could be specified in package Pretty_Printer of the
         --  project file, which will cause the builder to pass it to the inner
         --  invocation.

         if Mimic_gcc then
            Incremental_Mode := False;
         end if;
      end Pre_Scan_Arguments;

   --  Start of processing for Scan_Parameters_New

   begin
      Pre_Scan_Arguments;

      if Debug_Flag_C then
         Print_Command_Line;
      end if;

      Initialize_Option_Scan
        (Stop_At_First_Non_Switch => False,
         Section_Delimiters       => Prj.Get_Section_Delimiters);

      Prj.Scan_Arguments (First_Pass => True);
      pragma Assert (not (Incremental_Mode and Mimic_gcc));

      if Print_Version then
         Print_Tool_Version (2004);
         OS_Exit (0);
      end if;

      if Print_Usage then
         Prj.Print_Tool_Usage;
         OS_Exit (0);
      end if;

      --  If we have the project file specified as a tool parameter, analyze it

      ASIS_UL.Projects.Process_Project_File (Prj);

      --  And finally - analyze the command-line parameters. We do this even in
      --  Incremental_Mode, so we get any errors on the outer call to the ASIS
      --  tool, rather than on the inner calls.

      Initialize_Option_Scan
        (Stop_At_First_Non_Switch => False,
         Section_Delimiters       => Prj.Get_Section_Delimiters);

      Prj.Scan_Arguments;
      pragma Assert (not (Incremental_Mode and Mimic_gcc));

   end Scan_Parameters_New;

   ---------------------
   -- Scan_Common_Arg --
   ---------------------

   function Scan_Common_Arg
     (My_Project  : in out ASIS_UL.Projects.Arg_Project_Type'Class;
      First_Pass : Boolean;
      Parser : Opt_Parser;
      In_Switches : Boolean;
      In_Project_File : Boolean;
      Initial_Char : Character)
     return Common_Arg_Status is

      Result : Common_Arg_Status := Arg_Not_Processed;
      --  Every switch recognized below MUST set Result!

      --  Note also that if you add processing for a switch here, you must add
      --  it to the string passed to Getopt in each ASIS tool you want to
      --  support that switch.
   begin
      case Initial_Char is
         when 'c' =>
            if Full_Switch (Parser => Parser) = "c" then
               Result := Arg_Processed;
            end if;

         when 'd' =>
            --  gnatmetric uses "-debug" instead of "-d".  Either way,
            --  Set_Debug_Options was already called by Pre_Scan_Arguments.

            if Has_Suffix (Tool_Name.all, Suffix => "gnatmetric") then
               if Full_Switch (Parser => Parser) = "debug" then -- "-debugX"
                  Result := Arg_Processed;
               end if;
            else
               if Full_Switch (Parser => Parser) = "d" then -- "-dX"
                  Result := Arg_Processed;
               end if;
            end if;

         when 'f' =>
            if Full_Switch (Parser => Parser) = "files" then
               Result              := Arg_Processed;
               File_List_Specified := True;

               if First_Pass then
                  Files_Switch_Used := True;
                  Read_Args_From_File
                    (Parameter (Parser => Parser),
                     Arg_Project         => My_Project,
                     Store_With_No_Check => True);

               elsif In_Project_File then
                  if In_Switches then
                     Error ("-files option is not allowed " &
                              "for Switches attribute");
                     raise Parameter_Error;
                  else
                     Read_Args_From_File
                       (Parameter (Parser => Parser),
                        Arg_Project       => My_Project,
                        Store_With_No_Check => True);
                  end if;
               end if;
            end if;

         when 'g' =>
            --  Various options that are passed to the inner tool invocations
            --  for incremental mode. These should mostly be passed along to
            --  the real compiler.

            --  -gnatec=configuration_pragmas_file
            --  -gnatem=mapping_file
            --  -gnateO=object_path_file_name

            if Full_Switch (Parser => Parser)
              in "gnatec" | "gnatem" | "gnateO"
            then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_GNAT_Option_With_Path
                    (Full_Switch (Parser => Parser),
                     Parameter (Parser => Parser));
               end if;

            elsif Full_Switch (Parser => Parser) in
              "gnatA" | "gnatea" | "gnatez"
            then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_Option
                    ("-" & Full_Switch (Parser => Parser));
               end if;

            elsif Full_Switch (Parser => Parser) = "gnatc" then
               Result := Arg_Processed;
               --  We can ignore -gnatc, because ASIS always calls the compiler
               --  with -gnatc.
            end if;

         when 'I' =>
            if Full_Switch (Parser => Parser) = "I" then
               Result := Arg_Processed;
               if not First_Pass then
                  Store_I_Option (Parameter (Parser => Parser));
               end if;
            end if;

         when 'j' =>
            if Full_Switch (Parser => Parser) = "j" then
               Result := Arg_Processed;

               --  We need to ignore -j in the inner invocation; otherwise we
               --  will complain about mixing -j with -rnb when not in
               --  --incremental mode.

               if not First_Pass and then not Mimic_gcc then
                  begin
                     J_Specified := True;
                     ASIS_UL.Options.Process_Num :=
                       Natural'Value (Parameter (Parser => Parser));

                     if ASIS_UL.Options.Process_Num = 0 then
                        ASIS_UL.Options.Process_Num :=
                          Positive (System.Multiprocessors.Number_Of_CPUs);
                     end if;
                  exception
                     when Constraint_Error =>
                        Error ("Wrong Parameter of '-j' option: " &
                               Parameter (Parser => Parser));
                        raise Parameter_Error;
                  end;
               end if;
            end if;

         when 'o' =>
            if Full_Switch (Parser => Parser) = "o" then
               --  See comments on Compiler_Output_Subdir for details about
               --  this horsing around. Here, we set Compiler_Output_Subdir and
               --  also pass the -o switch along to the compiler. Note that we
               --  specifically check for files ending in ".o", so we process
               --  things like "-o /some/directory/ALI-FILES-toolname/blah.o",
               --  but we do not process things like "-o blah.adb.pretty".
               --  That's important because gnatpp also has a -o switch, so we
               --  want to let gnatpp handle "-o blah.adb.pretty".
               declare
                  Param : constant String := Parameter (Parser => Parser);
               begin
                  if Has_Suffix (Param, Suffix => ".o") then
                     pragma Assert (False);
                     --  This is only for gnatmake, but we're using gprbuild
                     pragma Assert (Mimic_gcc);
                     Result := Arg_Processed;
                     if First_Pass then
                        pragma Assert (Compiler_Output_Subdir = null);
                        Compiler_Output_Subdir :=
                          new String'(Containing_Directory (Param));
                     else
                        --  Can't use Store_GNAT_Option_With_Path, because it
                        --  inserts an extra "=" that gcc doesn't like.
                        Store_Option ("-" & Full_Switch (Parser => Parser));
                        Store_Option (Parameter (Parser => Parser));
                     end if;
                  end if;
               end;
            end if;

         when 'v' =>
            if Full_Switch (Parser => Parser) = "v" then
               Result := Arg_Processed;
               if First_Pass then -- Handle verbose switch early
                  Verbose_Mode := True;
               end if;
            end if;

         when 'x' =>
            --  Ignore "-x ada". We only do this in Mimic_gcc, because
            --  gnatmetric uses the -x switch for something else.
            --  ???gnatmetric does not currently support --incremental;
            --  if it ever does, we will need to resolve this conflict.

            if Mimic_gcc and then Full_Switch (Parser => Parser) = "x" then
               Result := Arg_Processed;
            end if;

         when '-' =>
            if Full_Switch (Parser => Parser) = "-help" then
               Result := Quit;
               if In_Project_File then
                  Error ("project file should not contain '--help' option");
                  raise Parameter_Error;
               end if;

               Print_Usage := True;

            elsif Full_Switch (Parser => Parser) = "-version" then
               Result := Quit;
               if In_Project_File then
                  Error
                    ("project file should not contain '--version' option");
                  raise Parameter_Error;
               end if;

               Print_Version := True;

            elsif Full_Switch (Parser => Parser) = "-incremental" then
               Result := Arg_Processed;
               --  Incremental_Mode was already set by Pre_Scan_Arguments

            elsif Full_Switch (Parser => Parser) = "-outer-parallel" then
               Result := Arg_Processed;
               if not First_Pass then
                  Outer_Parallel := True;
                  pragma Assert (Mimic_gcc);
               end if;

            elsif Full_Switch (Parser => Parser) = "-outer-dir" then
               Result := Arg_Processed;
               if First_Pass then -- process --outer-dir early
                  Outer_Dir := new String'
                    (Full_Name (Parameter (Parser => Parser)));
                  pragma Assert
                    (Full_Name (Parameter (Parser => Parser)) =
                       Parameter (Parser => Parser));
                  pragma Assert (Mimic_gcc);
                  Change_Dir (Outer_Dir.all);
               end if;

            elsif Full_Switch (Parser => Parser) = "-output-dir" then
               Result := Arg_Processed;
               if not First_Pass then
                  Out_Dir := new String'
                    (Full_Name (Parameter (Parser => Parser)));
               end if;

            elsif Full_Switch (Parser => Parser) = "-rep-clauses" then
               Result := Arg_Processed;
               if not First_Pass then
                  Generate_Representation_Clauses := True;
               end if;
            end if;

         when others => null;
      end case;

      return Result;
   end Scan_Common_Arg;

   -------------------
   -- Set_Tree_Name --
   -------------------

   procedure Set_Tree_Name is
      Dot_Idx : Natural := 0;
      DS_Idx  : Natural;
   begin

      Free (Tree_File);

      if Arg_File = null then
         Tree_File := new String'("");
      else
         for J in reverse Arg_File'Range loop
            if Arg_File (J) = '.' then
               Dot_Idx := J - 1;
               exit;
            end if;
         end loop;

         DS_Idx := Arg_File'First;

         for J in reverse Arg_File'Range loop
            if Arg_File (J) = Directory_Separator then
               DS_Idx := J + 1;
               exit;
            end if;
         end loop;

         Tree_File := new String'(Arg_File (DS_Idx .. Dot_Idx) & ".adt");
      end if;
   end Set_Tree_Name;

   ----------------------------------
   -- Tool_Specific_Initialization --
   ----------------------------------

   procedure Tool_Specific_Initialization_1 is separate;
   procedure Tool_Specific_Initialization_2 is separate;

begin
   --  Check if we have the directory for temporary stuff specified
   declare
      Dir : String_Access := Getenv (Tmpdir);
   begin
      if Dir'Length > 0 and then
        Is_Absolute_Path (Dir.all) and then
        Is_Directory (Dir.all)
      then
         Temp_Dir := new String'(Normalize_Pathname (Dir.all));
      end if;

      Free (Dir);
   end;
end ASIS_UL.Environment;
