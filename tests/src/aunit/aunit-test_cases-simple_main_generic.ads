--  ----------------------------------------------------------------------------
--
--  The intent of this unit is to provide a simple main program that runs
--  one Test_Case.
--  The procedure is intended to be instansiated as a childern to
--  the package containing the test_case.
--
--  with AUnit.Test_Cases.Simple_Main_Generic;
--  procedure component.children.tests.testcase1.main is
--     new AUnit.Test_Cases.Simple_Main_Generic(Test_Case);
--
-- Thus providing a simple main for One_Testcase to be used during development.
--
--  ----------------------------------------------------------------------------
with AUnit.Test_Cases;
generic AUnit.Test_Cases.Simple_Main_Generic;
   type Test_Case is new AUnit.Test_Cases.Test_Case with private;
procedure AUnit.Test_Cases.Simple_Main_Generic;
