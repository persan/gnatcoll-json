--  ----------------------------------------------------------------------------
--
--  The intent of this unit is to provide a simple main program that runs
--  one Test_Case.
--  The procedure is intended to be instansiated as a childern to
--- the package containing the test_case.
--
--  ----------------------------------------------------------------------------
with AUnit.Test_Cases;
generic
   type Test_Case is new AUnit.Test_Cases.Test_Case with private;
procedure AUnit.Test_Cases.Simple_Main_Generic;
