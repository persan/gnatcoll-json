pragma Ada_2012;
with GNAT;
with GNAT.Source_Info;
with AUnit; use AUnit;
with GNATCOLL.JSON.Support.JSON_Paths;
with AUnit.Assertions;
package body GNATCOLL.JSON.Support.Test.JSON_Paths is
   use AUnit.Assertions;
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   -----------------
   -- Set_Up_Case --
   -----------------
   Src : constant String :=
           "{" &
           "   ""float""   : 2.0 " & "," &
           "   ""integer"" : 1 " & "," &
           "   ""string""  : ""one"" " & "," &
           "   ""struct""  : {""f1"": ""f1""} " & "," &
           "   ""deep1""   : {""deep2"": " &
           "                    {"  &
           "                        ""deep3""    : ""rock""," &
           "                        ""deep31""   : ""sss""" &
           "                    }"  &
           "                 }," &
           "   ""array""   : [ 1,2,3, [1,2,3,4], { ""False"": false, " &
           "                                       ""True"" : true" &
           "                                     }" &
           "                 ]" &
           "}";

   overriding procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      Test.Data := Read (Src);
   end Set_Up_Case;

   procedure Test_Get_deep (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant String := "rock";
      Result   : String (Expected'Range);
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "deep1.deep2.deep3"));
      Assert (Result = Expected, "Got :" & Result & " Expected:" & Expected);
   end Test_Get_deep;

   procedure Test_Get_Float (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Result   : Float;
      Expected : constant := 2.0;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "float"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Float;

   procedure Test_Get_Integer (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Result   : Integer;
      Expected : constant := 1;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "integer"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Integer;

   procedure Test_Get_String (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant String := "one";
      Result   : String (Expected'Range);
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "string"));
      Assert (Result = Expected, "Got :" & Result & " Expected:" & Expected);
   end Test_Get_String;

   procedure Test_Get_Array_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Integer := 2;
      Result   : Integer;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array(2)"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_1;

   procedure Test_Get_Array_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Integer := 3;
      Result   : Integer;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array(4)(3)"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_2;

   procedure Test_Get_Array_3 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Boolean := True;
      Result   : Boolean;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array(5).True"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_3;

   procedure Test_Get_Array_Hidden_Alternate_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Integer := 2;
      Result   : Integer;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array.2"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_Hidden_Alternate_1;

   procedure Test_Get_Array_Hidden_Alternate_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Integer := 3;
      Result   : Integer;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array.4.3"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_Hidden_Alternate_2;

   procedure Test_Get_Array_Hidden_Alternate_3 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T        : Test_Case renames Test_Case (Test);
      Expected : constant Boolean := True;
      Result   : Boolean;
   begin
      Result := Get (GNATCOLL.JSON.Support.JSON_Paths.Get (T.Data, "array.5.True"));
      Assert (Result = Expected, "Got :" & Result'Img & " Expected:" & Expected'Img);
   end Test_Get_Array_Hidden_Alternate_3;
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

end GNATCOLL.JSON.Support.Test.JSON_Paths;
