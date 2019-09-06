--  ----------------------------------------------------------------------------
--
--  The intent of this unit is to provide a simple main program that runs
--  one Test_Suite.
--  The procedure is intended to be instansiated as a childern to
--  the package containing the function Suite.
--
--  with AUnit.Test_Suites.Simple_Main_Generic;
--  procedure component.children.Tests.Suit1.main is
--     new AUnit.Test_Suites.Simple_Main_Generic(Suit);
--
--  Thus providing a simple main for One_Testcase to be used during development.
--
--  ----------------------------------------------------------------------------
with AUnit.Test_Suites;
generic
   with function Suite return AUnit.Test_Suites.Access_Test_Suite is <>;
procedure AUnit.Test_Suites.Simple_Main_Generic;
