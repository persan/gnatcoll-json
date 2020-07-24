with Ada.Text_IO; use Ada.Text_IO;
with Simple.JSON_Golden;
with GNATCOLL.JSON;
procedure Simple.Main is
   T : Concrete_Taggd_Record_With_Time;
begin
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Simple.JSON_Golden.Create (T)));
end simple.Main;
