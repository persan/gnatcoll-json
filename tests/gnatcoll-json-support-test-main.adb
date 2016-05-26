with AUnit.Run;
with AUnit.Reporter.Text;
with GNATCOLL.JSON.Support.Test.Suits.Test_Vectors;
procedure GNATCOLL.JSON.Support.Test.Main is
   procedure Run is new AUnit.Run.Test_Runner (GNATCOLL.JSON.Support.Test.Suits.Test_Vectors.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end GNATCOLL.JSON.Support.Test.Main;
