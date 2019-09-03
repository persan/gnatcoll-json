package GNATCOLL.JSON.Support.JSON_Paths with Obsolescent => "Use corresponding functions in GNATCOLL.JSON.Support" is
   function Get (Val : JSON_Value; Path : UTF8_String) return JSON_Value renames Get_Path;
   function Has_Value (Value : JSON_Value; Path : UTF8_String) return Boolean renames Path_Has_Value;
end GNATCOLL.JSON.Support.JSON_Paths;
