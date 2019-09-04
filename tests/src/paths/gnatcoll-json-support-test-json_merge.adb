pragma Ada_2012;
with GNAT;
with GNAT.Source_Info;
with AUnit; use AUnit;
with GNATCOLL.JSON.Support;
with AUnit.Assertions;
with GNATCOLL.JSON.Support.Test.Utilities;
package body GNATCOLL.JSON.Support.Test.JSON_Merge is
   use AUnit.Assertions;
   use GNATCOLL.JSON.Support.Test.Utilities;
   use GNAT.Source_Info;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      null;
   end Set_Up_Case;

   procedure Test_Add_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Add_1;

   procedure Test_Add_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Add_2;

   procedure Test_Add_3 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Add_3;

   procedure Test_Or_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Or_1;

   procedure Test_Or_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Or_2;

   procedure Test_Or_3 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Left        : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Right       : constant JSON_Value := Read_Json_Value (Name & "-right.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Left + Right;
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Or_3;

   procedure Test_Normalize_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Source      : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Normalize_Field_Names (Source);
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Normalize_1;

   procedure Test_Normalize_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Name        : constant String := "data/" & Ada2file_Simple (Enclosing_Entity);
      Source      : constant JSON_Value := Read_Json_Value (Name & "-left.json");
      Expected    : constant JSON_Value := Read_Json_Value (Name & "-expected.json");
      Result_Name : constant String := Name & "-result.out.json";
      Result      : JSON_Value;
   begin
      Result := Normalize_Field_Names (Source);
      Write (Result_Name, Write (Result, False));
      Assert (Result = Expected, "Not expected result");
   end Test_Normalize_2;
   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (Test : in out Test_Case) is separate;

   ----------
   -- Name --
   ----------

   overriding function Name (Test : Test_Case) return AUnit.Message_String is
      pragma Unreferenced (Test);
   begin
      return Format (Unit_Name);
   end Name;

end GNATCOLL.JSON.Support.Test.JSON_Merge;
