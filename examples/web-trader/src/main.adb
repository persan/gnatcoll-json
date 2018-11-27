with AWS.Response;
with AWS.Client;
WITH GNATCOLL.JSON;
with Ada.Text_IO;
with Data.JSON;
with Data.Images;
procedure Main is
   use Ada.Text_IO;
   URL : constant string := "http://api.exmo.com/v1/trades/?pair=BTC_USD&limit=10";
   J : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Read (STRING'(AWS.Response.Message_Body (AWS.Client.Get (URL => URL))));
   R : Data.Reply;
begin
   Put_Line (GNATCOLL.JSON.Write (J, FALSE));
   R := Data.JSON.Get (J);
   Put_Line (Data.Images.Image (R));
end Main;
