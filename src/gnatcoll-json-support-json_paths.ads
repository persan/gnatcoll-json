package GNATCOLL.JSON.Support.JSON_Paths is

   ------------------------------------------------------------------------------------------
   --  This suport package gives suport to read nodes in a JSON value
   --  by providing a textual path.
   --
   --  "data.text.values(1).fix" will return a JSON_Object of JSON_Int_Type containing 1
   --  "data.text.active" will return a JSON_Object of JSON_Boolean_Type containing False
   --
   --    { "data"
   --        { "text" :
   --            { "values" :
   --                [ {"fix"  : 1},
   --                  {"fool" : 2}
   --                ],
   --
   --              "description" : "fix",
   --              "speed"       : 1.2,
   --              "active"      : False
   --            }
   --        }
   --    }
   ------------------------------------------------------------------------------------------

   Path_Delimiter          : constant String := ".";
   Start_Indexed_Delimiter : constant String := "(";
   End_Indexed_Delimiter   : constant String := ")";

   function Get (Val : JSON_Value; Path : UTF8_String) return JSON_Value;
   --
   --  Return the JSON_Value on path
   --  JSON_Null  will be returned if the Path dont exist.
   -------------------------------------------------------------------

   function Has_Value (Value : JSON_Value; Path : UTF8_String) return Boolean is
     (Get (Value, Path).Kind in JSON_Elementary_Value_Type);

end GNATCOLL.JSON.Support.JSON_Paths;
