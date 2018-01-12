with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   type S is access constant Integer;
   T : aliased Integer := 3;
   A : constant S := T'Access;
begin
   Put_Line (A.all'Img);
   A.all := 2;

end Main;
