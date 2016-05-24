with Data.JSON;
with GNATCOLL.JSON;
with Ada.Text_IO;
procedure Testa.Main is
   D : Data.Rec;
begin
   D.Add ("11");
   D.Add (11);
   D.Add (11.0);
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Data.JSON.Create (D), False));
end;
