with GNATCOLL.JSON.Support.GNAT.Spitbol;
with AUnit.Test_Cases;

package GNATCOLL.JSON.Support.Test.GNAT.Spitbol is

   generic
      with package T is new GNATCOLL.JSON.Support.GNAT.Spitbol.Table (<>);
      with function Initialize return  T.V.Table;
   package Generic_Test is
      type Table_Access is access all T.V.Table;
      type Test_Case is new AUnit.Test_Cases.Test_Case with  record
         Test_Data : Table_Access;
      end record;

      overriding procedure Set_Up_Case (Test : in out Test_Case);
      overriding procedure Register_Tests (Test : in out Test_Case);
      overriding function Name (Test : Test_Case) return AUnit.Message_String;

   end Generic_Test;
end GNATCOLL.JSON.Support.Test.GNAT.Spitbol;
