
with AUnit.Run;
with AUnit.Reporter.Text;
with AUnit.Reporter.GNATtest;
with AUnit.Reporter.XML;
with AUnit.Test_Suites;
with Ada.Command_Line;
with GNAT.Command_Line;
procedure AUnit.Test_Suites.Simple_Main_Generic is

   function Run is new AUnit.Run.Test_Runner_With_Status (Suite);

   Text_Reporter           : AUnit.Reporter.Text.Text_Reporter;
   GNATtest_Reporter       : AUnit.Reporter.GNATtest.GNATtest_Reporter;
   XML_Reporter            : AUnit.Reporter.XML.XML_Reporter;
   type Reporter_Selection_Type is (Use_Text_Reporter,
                                    Use_GNATTest_Reporter,
                                    Use_XML_Reporter);

   Reporter_Selection      : Reporter_Selection_Type := Use_Text_Reporter;
   Command_Line_Parser     : GNAT.Command_Line.Command_Line_Configuration;
   use GNAT.Command_Line;
   Use_Text                : aliased Boolean;
   Use_GANTTest            : aliased Boolean;
   Use_XML                 : aliased Boolean;
begin

   Define_Switch (Command_Line_Parser, Use_XML'Access, "-x", "--xml",
                  Help => "Use xml as output format.");
   Define_Switch (Command_Line_Parser, Use_Text'Access, "-t", "--text",
                  Help => "Use Text as output format.");
   Define_Switch (Command_Line_Parser, Use_GANTTest'Access, "-T", "--test",
                  Help => "Use GNATtest as output format.");
--     Define_Switch (Command_Line_Parser, Output'Access, "-o=",
--                    Help => "Save results to a file, deault is standard output.");

   if Use_Text then
      Reporter_Selection := Use_Text_Reporter;
   elsif  Use_XML then
      Reporter_Selection := Use_XML_Reporter;
   elsif Use_GANTTest then
      Reporter_Selection := Use_GNATTest_Reporter;
   end if;

   Ada.Command_Line.Set_Exit_Status
     (case Run
        (case Reporter_Selection is
            when Use_Text_Reporter       => Text_Reporter,
            when Use_GNATTest_Reporter   => GNATtest_Reporter,
            when Use_XML_Reporter        => XML_Reporter) is
         when Failure => Ada.Command_Line.Failure,
         when Success => Ada.Command_Line.Success
     );

end AUnit.Test_Suites.Simple_Main_Generic;
