--  ----------------------------------------------------------------------------
--
--  The intent of this unit is to provide a simple main program that runs
--  one Test_Case.
--  The procedure is intended to be instansiated as a childern to
--- the package containing the Test_Suite.
--
--  ----------------------------------------------------------------------------
with AUnit.Test_Suites;
generic
   with function Suite return AUnit.Test_Suites.Access_Test_Suite is <>;
procedure AUnit.Test_Suites.Simple_Main_Generic;
