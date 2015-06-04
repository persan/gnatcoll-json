------------------------------------------------------------------------

pragma Ada_2012;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with ASIS_UL.Common;             use ASIS_UL.Common;
with ASIS_UL.Compiler_Options;   use ASIS_UL.Compiler_Options;
with ASIS_UL.Debug;              use ASIS_UL.Debug;
with ASIS_UL.Misc;               use ASIS_UL.Misc;
with ASIS_UL.Options;            use ASIS_UL.Options;
with ASIS_UL.Output;             use ASIS_UL.Output;

with Gnatvsn;
with Opt;                        use all type Opt.Ada_Version_Type;

with GNATCOLL.JSON.Support.Builder.Options;           use GNATCOLL.JSON.Support.Builder.Options;
with GNATCOLL.JSON.Support.Builder.Sampler;           use GNATCOLL.JSON.Support.Builder.Sampler;

package body GNATCOLL.JSON.Support.Builder.Projects is

   ----------------------
   -- Print_Tool_Usage --
   ----------------------

   overriding procedure Print_Tool_Usage
     (My_Project : Builder_Project_Type)
   is
      pragma Unreferenced (My_Project);
   begin
      GNATCOLL.JSON.Support.Builder.Sampler.Brief_Help;
   end Print_Tool_Usage;

   --------------------
   -- Scan_Arguments --
   --------------------

   overriding procedure Scan_Arguments
     (My_Project  : in out Builder_Project_Type;
      First_Pass  :        Boolean    := False;
      Parser      :        Opt_Parser := Command_Line_Parser;
      In_Switches :        Boolean    := False)
   is
      pragma Unreferenced (In_Switches);
      Nat             : Natural;
      Sources_Found   : Boolean;
      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;

      Dest_Dir_Tmp : String_Access;
   begin
      --  For some parameters, we have two options to set the same thing.
      --  When gnatstub was initially developed, it introduced its
      --  own options for everything, including controlling the layout of
      --  the generated code. Then it was realized that it would make sense
      --  to use for setting the layout of the gnatstub-generated code the
      --  same options as used to set the GNAT style checking mode (see
      --  C307-004). And the old gnatstub versions are kept for back
      --  compatibility

      Process_Parameters : loop
         case
            GNAT.Command_Line.Getopt
              ("P:  X! eL "             & --  project-specific options
               "-RTS= "                 &
               "f hs hg k o= q r t v "  & --  general gnatstub
               "-header-file= d? "      & --  options
               "-dir= "                 &
               "-no-exception "         & --  no raise stmt in procedure stubs
               "-no-local-header "      & --  no local comment headers
               "i! l! "                 & --  old layout control options
               "gnatyo gnaty! gnatyM! " & --  GNAT-style layout control
               "I! gnatec! "            &
               "Wh Wu Ws We W8 Wb "     & --  encoding of the result file(s)
               "-version -help "        & --  print version and usage
               "-test "                 & --  test mode for Q4A
               "gnat83 "                & --  process Ada 83 constructs
               "gnat95 "                & --  process Ada 95 constructs
               "gnat05 gnat2005 "       & --  process Ada 2005 constructs
                 "gnat12 gnat2012",       --  process Ada 2012 constructs
               Parser => Parser)
         is
            when ASCII.NUL =>     --  ???
               --  gnatstub does not use the Source Table from ASIS UL

               if First_Pass then
                  Sources_Found := False;

                  Scan_Sources : loop
                     case Num_Of_Args is
                        when 0 =>
                           File_Name :=
                             new String'(Get_Argument (Parser => Parser));

                           if File_Name.all = "" then
                              Error ("file name is missing");
                              Brief_Help;
                              raise Parameter_Error;
                           else
                              Sources_Found := True;
                           end if;
                        when 1 =>

                           Dest_Dir_Tmp := new String'(Get_Argument);

                           if Dest_Dir_Tmp.all = "" then
                              if Destination_Dir = null then
                                 Destination_Dir := new String'("");
                              end if;

                              Free (Dest_Dir_Tmp);
                              exit Scan_Sources;
                           else
                              Free (Destination_Dir);
                              Destination_Dir := new String'(Dest_Dir_Tmp.all);
                              Free (Dest_Dir_Tmp);
                              Sources_Found := True;
                              Dest_Dir_Set  := True;
                           end if;

                        when 2 =>
                           if Get_Argument  (Parser => Parser) = "" then
                              exit Scan_Sources;
                           else
                              Error ("only one file name and at most " &
                                     "one destination directory are allowed");
                              Brief_Help;
                              raise Parameter_Error;
                           end if;
                        when others =>
                           pragma Assert (False);
                           null;
                     end case;

                     Num_Of_Args := Num_Of_Args + 1;
                  end loop Scan_Sources;

                  if not Sources_Found then
                     exit Process_Parameters;
                  end if;

               elsif In_Project_File then  --  ???
                  exit Process_Parameters;
--                  Error ("Reading argument files from project " &
--                         "not implemented yet");
--                  raise Parameter_Error;
               else
                  exit Process_Parameters;
               end if;

            when 'd' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "d" then
                     Set_Debug_Options (Parameter (Parser => Parser));
                  end if;
               end if;

            when 'e' =>
               if Full_Switch (Parser => Parser) = "eL" then
                  if First_Pass then
                     ASIS_UL.Projects.Follow_Symbolic_Links := True;
                  elsif In_Project_File then
                     Error ("-eL option cannot be set in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'f' =>
               if not First_Pass then
                  Overwrite_Body := True;
               end if;

            when 'g' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "gnatec" then
                     Store_GNAT_Option_With_Path
                       (Full_Switch (Parser => Parser),
                        Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "gnatyo" then
                     Alphabetical_Ordering := True;

                  elsif Full_Switch (Parser => Parser) = "gnatyM" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                     if Nat = 0 then
                        Error
                          ("body line length cannot be 0");
                        raise Parameter_Error;
                     else
                        Max_Body_Line_Length := Nat;
                     end if;

                  elsif Full_Switch (Parser => Parser) = "gnaty" then
                     Nat :=
                       Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                     if Nat not in 1 .. 9 then
                        Error ("indentation level should be from 1 .. 9");
                        raise Parameter_Error;
                     else
                        Indent_Level := Nat;
                     end if;

                  elsif Full_Switch (Parser => Parser) = "gnat83" then
                     Opt.Ada_Version     := Ada_83;
                     Ada_Version_Changed := True;
                  elsif Full_Switch (Parser => Parser) = "gnat95" then
                     Opt.Ada_Version     := Ada_95;
                     Ada_Version_Changed := True;
                  elsif Full_Switch (Parser => Parser) in
                    "gnat05" | "gnat2005"
                  then
                     Opt.Ada_Version     := Ada_2005;
                     Ada_Version_Changed := True;
                  elsif Full_Switch (Parser => Parser) in
                    "gnat12" | "gnat2012"
                  then
                     Opt.Ada_Version     := Ada_2012;
                     Ada_Version_Changed := True;
                  end if;
               end if;

            when 'h' =>

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "hg" then
                     Header := Stand_Header;
                  elsif Full_Switch (Parser => Parser) = "hs"then
                     Header := From_Spec;
                  elsif Full_Switch (Parser => Parser) = "header_file" then
                     Header_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     Header           := From_File;
   --               elsif Full_Switch (Parser => Parser) = "-header_file" then
   --                  Header_File_Name := new String'(Parameter);
   --                  Header           := From_File;
                  end if;
               end if;

            when 'i' =>
               --  Old-style option for setting the indentation level,
               --  the GNAT-style option is -gnaty

               if not First_Pass then
                  Nat :=
                    Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                  if Nat not in 1 .. 9 then
                     Error (" indentation level should be from 1 .. 9");
                     raise Parameter_Error;
                  else
                     Indent_Level := Nat;
                  end if;
               end if;

            when 'I' =>
               if not First_Pass then
                  Store_I_Option (Parameter (Parser => Parser));
               end if;

            when 'k' =>
               if not First_Pass then
                  Delete_Tree := False;
               end if;

            when 'l' =>
               --  Old-style option for setting the indentation level,
               --  the GNAT-style option is -gnaty

               if not First_Pass then
                  Nat :=
                    Get_Nat_Switch_Parameter (Parameter (Parser => Parser));

                  if Nat = 0 then
                     Error ("body line length cannot be 0");
                     raise Parameter_Error;
                  else
                     Max_Body_Line_Length := Nat;
                  end if;
               end if;

            when 'o' =>
               if not First_Pass then
                  Body_Name := new String'(Parameter (Parser => Parser));
               end if;

            when 'P' =>
               if Full_Switch (Parser => Parser) = "P" then
                  if First_Pass then
                     My_Project.Store_Project_Source
                       (Parameter (Parser => Parser));
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'q' =>
               if not First_Pass then
                  Quiet_Mode := True;
               end if;

            when 'r' =>
               if not First_Pass then
                  Reuse_Tree := True;
               end if;

            when 't' =>
               if not First_Pass then
                  Overwrite_Tree := True;
               end if;

            when 'v' =>
               if not First_Pass then
                  Verbose_Mode := True;
                  Print_Version_Info (1996);
                  Info ("");
                  Info ("Copyright 1997-" & Gnatvsn.Current_Year &
                        ", Free Software Foundation, Inc.");
               end if;

            when 'W' =>

               if Full_Switch = "Wh" then
                  Body_Form := new String'("WCEM=h");
               elsif Full_Switch = "Wu" then
                  Body_Form := new String'("WCEM=u");
               elsif Full_Switch = "Ws" then
                  Body_Form := new String'("WCEM=s");
               elsif Full_Switch = "We" then
                  Body_Form := new String'("WCEM=e");
               elsif Full_Switch = "W8" then
                  Body_Form := new String'("WCEM=8");
               elsif Full_Switch = "Wb" then
                  Body_Form := new String'("WCEM=b");
               end if;

            when 'X' =>
               if Full_Switch (Parser => Parser) = "X" then
                  if First_Pass then
                     ASIS_UL.Projects.Store_External_Variable
                       (Var => Parameter (Parser => Parser));
                  elsif In_Project_File then
                     Error ("external references cannot be set in " &
                            "a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when '-' =>

               if Full_Switch (Parser => Parser) = "-help" then
                  if In_Project_File then
                     Error ("project file should not contain '--help' option");
                     raise Parameter_Error;
                  end if;

                  Print_Usage := True;
                  return;
               elsif Full_Switch (Parser => Parser) = "-version" then
                  if In_Project_File then
                     Error
                       ("project file should not contain '--version' option");
                     raise Parameter_Error;
                  end if;

                  Print_Version := True;
                  return;
               end if;

               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-header-file" then
                     Header_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     Header           := From_File;
                  elsif Full_Switch (Parser => Parser) = "-no-exception" then
                     No_Exception_In_Stubs := True;
                  elsif Full_Switch (Parser => Parser) =
                    "-no-local-header"
                  then
                     No_Local_Comment_Headers := True;
                  elsif Full_Switch (Parser => Parser) = "-test" then
                     Test_Mode := True;
                  elsif Full_Switch (Parser => Parser) = "-dir" then
                     if not Dest_Dir_Set then
                        Free (Destination_Dir);
                        Destination_Dir :=
                          new String'(Parameter (Parser => Parser));
                     end if;
                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     ASIS_UL.Compiler_Options.Store_Option
                       ("--RTS=" & Parameter (Parser => Parser));
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-RTS" then
                     Store_RTS_Path (Parameter (Parser => Parser));
                  end if;
               end if;

            when others =>
               Brief_Help;
               raise Parameter_Error;
         end case;

      end loop Process_Parameters;

      if not First_Pass then
         Process_cargs_Section (Parser => Parser);
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch : " &
                  GNAT.Command_Line.Full_Switch (Parser => Parser));
         Brief_Help;

         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error ("missing parameter for: " &
                  GNAT.Command_Line.Full_Switch (Parser => Parser));
         Brief_Help;

         raise Parameter_Error;
   end Scan_Arguments;

   -----------------------
   -- Tool_Package_Name --
   -----------------------

   overriding function Tool_Package_Name
     (My_Project : Builder_Project_Type)
      return       String
   is
      pragma Unreferenced (My_Project);
   begin
      return "ada2json";
   end Tool_Package_Name;

end GNATCOLL.JSON.Support.Builder.Projects;
