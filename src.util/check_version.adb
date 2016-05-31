with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.Calendar;
with GNATCOLL.JSON.Support;

procedure Check_Version is
   use Ada.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Text_IO;

   Exit_Status : ADa.Command_Line.Exit_Status := ADa.Command_Line.Success;

   pragma Warnings (Off, "condition is always False");
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
   if GNATCOLL.JSON.Support.VERSION /= $VERSION then
      Put_Line (Standard_Error, "Version missmatch : source=>"  & GNATCOLL.JSON.Support.VERSION & " /= project =>" & $VERSION);
      Exit_Status := Ada.Command_Line.Failure;

   end if;
   Open (F, In_File, "README.md");
   while not End_Of_File (F) loop
      Check (Get_Line (F));
   end loop;
   if (not VERSION_OK) or (not DATE_OK) then
      Exit_Status := Ada.Command_Line.Failure;
      if not VERSION_OK then
         Put_Line (Standard_Error, "Version missmatch in README.MD:"  & GNATCOLL.JSON.Support.VERSION & " not found");
      end if;
      if not VERSION_OK then
         Put_Line (Standard_Error, "Data missmatch in README.MD:"  & GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, GNAT.Calendar.Time_IO.ISO_Date ) & " not Found");
      end if;
   end if;
   Close (F);
   if Exit_Status = Success then
      Put_Line (GNATCOLL.JSON.Support.VERSION);
   end if;
   Ada.Command_Line.Set_Exit_Status (Exit_Status);
end  Check_Version;
