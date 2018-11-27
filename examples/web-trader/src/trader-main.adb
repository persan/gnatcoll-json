with AWS.Response;
with AWS.Client;
with GNATCOLL.JSON;
with Ada.Text_IO;
with Trader.Data.JSON;
with Trader.Data.Images;
procedure Trader.Main is
   use Ada.Text_IO;
   URL : constant String := "http://api.exmo.com/v1/trades/?pair=BTC_USD&limit=100";
   J   : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Read (STRING'(AWS.Response.Message_Body (AWS.Client.Get (URL => URL))));
   R   : Data.Reply;
begin
   Put_Line (GNATCOLL.JSON.Write (J, FALSE));
   R := Data.JSON.Get (J);
   Put_Line (Data.Images.Image (R));
end Trader.Main;
