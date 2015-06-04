package body GNATCOLL.JSON.Support.GNAT.SPitbol is

   ----------------
   -- JSON_Table --
   ----------------

   package body Table is

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table) return JSON_Value is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
         raise Program_Error with "Unimplemented function Create";
         return Create (Val);
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val);
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val, Field);
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field  : V.Table)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Set_Field unimplemented");
         raise Program_Error with "Unimplemented procedure Set_Field";
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table_Entry) return JSON_Value is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
         raise Program_Error with "Unimplemented function Create";
         return Create (Val);
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Entry is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val);
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val : JSON_Value;
         Field : UTF8_String)
         return V.Table_Entry
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val, Field);
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val  : JSON_Value;
         Field_Name : UTF8_String;
         Field  : V.Table_Entry)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Set_Field unimplemented");
         raise Program_Error with "Unimplemented procedure Set_Field";
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table_Array) return JSON_Value is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
         raise Program_Error with "Unimplemented function Create";
         return Create (Val);
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Array is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val);
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val : JSON_Value;
         Field : UTF8_String)
         return V.Table_Array
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
         raise Program_Error with "Unimplemented function Get";
         return Get (Val, Field);
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field  : V.Table_Array)
      is
      begin
         --  Generated stub: replace with real body!
         pragma Compile_Time_Warning (Standard.True, "Set_Field unimplemented");
         raise Program_Error with "Unimplemented procedure Set_Field";
      end Set_Field;

   end Table;

end GNATCOLL.JSON.Support.GNAT.SPitbol;
