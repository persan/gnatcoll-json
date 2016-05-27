with JSONSample.InterfaceTypes.JSON;
with Ada.Text_IO;
with GNATCOLL.JSON;
with JSONSample.String_Maps;
with Ada.Strings.Unbounded;
procedure JSONSample.Main is
   use JSONSample.InterfaceTypes.JSON;
   use JSONSample.InterfaceTypes;
   use Ada.Strings.Unbounded;
   Request  : JSONSample.InterfaceTypes.Request;
   Response : JSONSample.InterfaceTypes.Response;
begin
   Request.Args := new String_Maps.Map;
   Request.Args.Include ("Foo", "Fax");
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Create (Request), False));

   Response.Completed := new Posix_Time'(555);
   Response.Interupt_Message := new Interupt_Message_Type'(To_Unbounded_String ("Interupt_Message"), Error);
   Response.Messages.Append ("Line 1");
   Response.Messages.Append ("Line 2");
   Response.Messages.Append ("Line 3");
   Response.Messages.Append ("Line 4");
   Response.Progress := new Progress_Type'(10.0, 30.0, 20.0);
   Ada.Text_IO.Put_Line (GNATCOLL.JSON.Write (Create (Response), False));
end JSONSample.Main;
