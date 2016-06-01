------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.Calendar;
with GNATCOLL.JSON.Support;

procedure Check_Version is
   pragma Compile_Time_Error (GNATCOLL.JSON.Support.VERSION /= $VERSION,
                              "Version missmatch between source and project");
   --  Check that the version number i gnatcoll-json.gpr and
   --  gnatcoll-json-support matches.
   --  -------------------------------------------------------------------------

   use Ada.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Text_IO;

   Exit_Status : Ada.Command_Line.Exit_Status := Ada.Command_Line.Success;

   pragma Warnings (Off, "condition is always False");
   pragma Warnings (Off, "condition is always True");
   F           : File_Type;
   VERSION_OK  : Boolean := False;
   DATE_OK     : Boolean := False;
   Date        : constant String := GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, GNAT.Calendar.Time_IO.ISO_Date);
   procedure Check (Line : String) is
   begin
      if Index (Line, GNATCOLL.JSON.Support.VERSION) > 0 then
         VERSION_OK := True;
         if Index (Line, Date) > 0 then
            DATE_OK := True;
         end if;
      end if;
   end Check;
begin

   Open (F, In_File, "README.md");
   while not End_Of_File (F) loop
      Check (Get_Line (F));
   end loop;
   if (not VERSION_OK) or (not DATE_OK) then
      Exit_Status := Ada.Command_Line.Failure;
      if not VERSION_OK then
         Put_Line (Standard_Error, "Version missmatch in README.MD:"  & GNATCOLL.JSON.Support.VERSION & " not found");
      end if;
      if not DATE_OK then
         Put_Line (Standard_Error, "Date missmatch in README.MD:"  & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, GNAT.Calendar.Time_IO.ISO_Date ) & " not Found");
      end if;
   end if;
   Close (F);

   if Exit_Status = Success then
      Put_Line (GNATCOLL.JSON.Support.VERSION);
   end if;

   Ada.Command_Line.Set_Exit_Status (Exit_Status);
end  Check_Version;
