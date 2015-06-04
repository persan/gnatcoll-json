------------------------------------------------------------------------------
--                                                                          --
--                           GNATSTUB COMPONENTS                            --
--                                                                          --
--                       G N A T S T U B  . D R I V E R                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--           Copyright (C) 1997-2014, Free Software Foundation, Inc.        --
--                                                                          --
-- GNATCOLL.JSON.Support.Builder is free software;  you  can  redistribute it  and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version.  GNATCOLL.JSON.Support.Builder is  distributed  in the hope  that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;  without  even the implied warranty of MER- --
-- CHANTABILITY or  FITNESS  FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details. You  should have received a copy of the --
-- GNU General Public License distributed with GNAT;  see file COPYING. If  --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCOLL.JSON.Support.Builder  is  distributed as a part of the  ASIS implementation for GNAT --
-- (ASIS-for-GNAT).                                                         --
--                                                                          --
-- GNATCOLL.JSON.Support.Builder  was  originally  developed  by  Alexei Kuchumov  as a  part of --
-- collaboration  between  Software  Engineering  Laboratory  of  the Swiss --
-- Federal  Institute  of  Technology  in  Lausanne, Switzerland,  and  the --
-- Scientific  Research  Computer  Center  of the  Moscow State University, --
-- Russia.  This  work  was  supported  by  a grant from the Swiss National --
-- Science Foundation,  no  7SUPJ048247, funding a project  "Development of --
-- ASIS for GNAT with industry quality".                                    --
--                                                                          --
-- GNATCOLL.JSON.Support.Builder is now maintained by AdaCore (http://www.adacore.com).          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;

with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with ASIS_UL.Common;          use ASIS_UL.Common;

with GNATCOLL.JSON.Support.Builder.Sampler;        use GNATCOLL.JSON.Support.Builder.Sampler;
with GNATCOLL.JSON.Support.Builder.Options;        use GNATCOLL.JSON.Support.Builder.Options;
with GNAT.Traceback.Symbolic;
with Gnat.Exception_Traces;
procedure GNATCOLL.JSON.Support.Builder.Driver is
   use type Asis.Errors.Error_Kinds;
begin
   Gnat.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Gnat.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
   Initialize;

   if not Initialized then
      return;
   end if;

   Create_Sample;
   Clean_Up;
exception
   when Fatal_Error | Parameter_Error =>
      --  Everything has already been reported
      Clean_Up;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when Ex : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      declare
         Tmp_Output : constant File_Access := Current_Output;
      begin
         Set_Output (Standard_Error);
         New_Line;

         if Exception_Identity (Ex) = Asis.Exceptions.ASIS_Failed'Identity
           and then
            Asis.Implementation.Status = Asis.Errors.Use_Error
           and then
            Asis.Implementation.Diagnosis =
            "Cannot process Ada sources compiled with -gnat05"
         then
            --  EC12-013: This path should be removed when ASIS 2005 is
            --  implemented
            Put_Line ("ada2json: Ada 2005 not supported yet, exiting");

         elsif Exception_Identity (Ex) = Program_Error'Identity and then
            Exception_Message (Ex) = "Inconsistent versions of GNAT and ASIS"
         then
            Put ("ada2json is inconsistent with the GNAT version");
            New_Line;
            Put ("Check your installation of GNAT, ASIS and the GNAT toolset");
            New_Line;
         else
            Put ("Unexpected bug in ada2json - ");
            Put (Exception_Name (Ex));
            Put (" was raised: ");

            if Exception_Message (Ex)'Length = 0 then
               Put_Line ("(no exception message)");
            else
               Put_Line (Exception_Message (Ex));
            end if;
            Set_Output (Tmp_Output.all);
         end if;

      end;

      Clean_Up;
end GNATCOLL.JSON.Support.Builder.Driver;
