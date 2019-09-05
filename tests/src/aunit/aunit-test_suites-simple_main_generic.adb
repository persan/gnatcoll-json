
with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Reporter.GNATtest;
with AUnit.Reporter.XML;
with AUnit.Reporter.Stream_XML;
with AUnit.Test_Suites;
with Ada.Command_Line;

procedure AUnit.Test_Suites.Simple_Main_Generic is

   function Run is new AUnit.Run.Test_Runner_With_Status (Suite);

   Text_Reporter           : AUnit.Reporter.Text.Text_Reporter;
   GNATtest_Reporter       : AUnit.Reporter.GNATtest.GNATtest_Reporter;
   XML_Reporter            : AUnit.Reporter.XML.XML_Reporter;
   XML_Stream_Reporter     : AUnit.Reporter.Stream_XML.XML_Reporter;

   Use_Text_Reporter       : Boolean := True;
   Use_GNATTest_Reporter   : Boolean := False;
   Use_XML_Reporter        : Boolean := False;
   Use_XML_Stream_Reporter : Boolean := False;

begin

   Ada.Command_Line.Set_Exit_Status
     (case Run
        (if Use_Text_Reporter then
              Text_Reporter
         elsif Use_GNATTest_Reporter then
            GNATtest_Reporter
         elsif Use_XML_Reporter then
            XML_Reporter
         elsif Use_XML_Stream_Reporter then
            XML_Stream_Reporter
         else Text_Reporter) is
      when Failure => Ada.Command_Line.Failure,
        when Success => Ada.Command_Line.Success
     );

end AUnit.Test_Suites.Simple_Main_Generic;
