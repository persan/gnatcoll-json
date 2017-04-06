with System.Storage_Elements; use System.Storage_Elements;
with AUnit.Test_Cases;
package GNATCOLL.JSON.Support.Test.System.Storage_Elements is
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      T_0 : Storage_Offset;
      T_1 : Storage_Element;
      T_2 : Storage_Array (1 .. 10);
      T_3 : Integer_Address;
   end record;

   overriding procedure Set_Up_Case (Test : in out Test_Case);
   overriding procedure Register_Tests (Test : in out Test_Case);
   overriding function Name (Test : Test_Case) return AUnit.Message_String;

end GNATCOLL.JSON.Support.Test.System.Storage_Elements;
