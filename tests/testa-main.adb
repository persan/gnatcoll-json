with Data.JSON;
with GNATCOLL.JSON;
with ada.Text_IO;
procedure Testa.Main is
   D : Data.Rec;
begin
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Data.JSON.Create (D), False));
end;
