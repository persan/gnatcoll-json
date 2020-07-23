with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Libadalang.Common;
with GNATCOLL.JSON.Support.Generate.Writer;
with GNAT.Traceback.Symbolic;
procedure GNATCOLL.JSON.Support.Generate.Driver.Main is
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package Slocs renames Langkit_Support.Slocs;
   Context : constant LAL.Analysis_Context := LAL.Create_Context;
begin
   --  Try to parse all source file given as arguments
   -- for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         -- Filename : constant String := Ada.Command_Line.Argument (I);
         Unit     : constant LAL.Analysis_Unit :=  Context.Get_From_File ("tests.codegen/simple.ads");
      begin
         --  Report parsing errors, if any
         if Unit.Has_Diagnostics then
            for D of Unit.Diagnostics loop
               Put_Line (Unit.Format_GNU_Diagnostic (D));
            end loop;

            --  Otherwise, look for object declarations
         else
            declare
               W : GNATCOLL.JSON.Support.Generate.Writer.JSON_Writer;
            begin
               W.Write (Unit.Root);
            end;

         end if;
         New_Line;
      end;
   --   end loop;
exception
   when E : others  =>
      Put_Line (Ada.Exceptions.Exception_Information (E) &
                  GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex (E));


end GNATCOLL.JSON.Support.Generate.Driver.Main;
