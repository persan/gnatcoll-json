with Ada.Command_Line;
with GNAT.Strings;
with GNAT.Expect;
with GNAT.String_Split;
with Ada.Text_IO;
procedure Check_Tags is
   use GNAT.String_Split;
   use GNAT.Strings;
   use Ada.Text_IO;
   Args   : String_List_Access := new
     String_List'(new String'("tag"),
                  new String'("-l"));
   Status : aliased Integer;
   S      : GNAT.String_Split.Slice_Set;
begin
   Create (S => S,
           From => GNAT.Expect.Get_Command_Output
             (Command    => "git",
              Arguments  => Args.all,
              Status     => Status'Access,
              Input      => "",
              Err_To_Out => True),
           Separators => ASCII.LF & ASCII.CR);
   Free (Args);
   for I in 1 .. Slice_Count (S) loop
      if Ada.Command_Line.Argument (1) = Slice (S, I) then
         Put_Line (Standard_Error, "Tag: '" & Slice (S, I) & "' Already exists.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end loop;
end Check_Tags;
