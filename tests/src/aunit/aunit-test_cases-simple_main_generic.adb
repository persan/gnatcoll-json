
with AUnit.Test_Suites;
with AUnit.Test_Suites.Simple_Main_Generic;

procedure AUnit.Test_Cases.Simple_Main_Generic is
   use AUnit.Test_Suites;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : Access_Test_Suite;
      pragma Warnings (Off);
   begin
      Result := new Test_Suite;
      Add_Test (Result, new Test_Case);
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Suites.Simple_Main_Generic (Suite);
begin
   Run;
end AUnit.Test_Cases.Simple_Main_Generic;
