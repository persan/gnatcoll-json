with AUnit.Run;
with AUnit.Reporter.Text;

procedure GNATCOLL.JSON.Support.Test.Tree_Suites.Main is
   procedure Run is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end GNATCOLL.JSON.Support.Test.Tree_Suites.Main;
