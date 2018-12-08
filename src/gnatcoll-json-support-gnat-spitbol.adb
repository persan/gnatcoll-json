with Interfaces;
package body GNATCOLL.JSON.Support.GNAT.Spitbol is

   -----------
   -- Table --
   -----------

   package body Table is

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table) return JSON_Value is
      begin
         return Create (V.Convert_To_Array (Val));
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table is
         Data : constant JSON_Array := Get (Val);
      begin
         return Ret : V.Table (Interfaces.Unsigned_32 (Length (Data))) do
            for E of V.Table_Array'(Get (Val)) loop
               V.Set (Ret, E.Name, E.Value);
            end loop;
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Val : JSON_Value; Field : UTF8_String) return V.Table is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : V.Table)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table_Entry) return JSON_Value is
      begin
         return Ret : constant JSON_Value := Create_Object do
            Set_Field (Ret, "Key", Create (Val.Name));
            Set_Field (Ret, "Element", Create (Val.Value));
         end return;
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Entry is
      begin
         return Ret : V.Table_Entry do
            Ret.Name := Get (Val, "Key");
            Ret.Value := Get (Get (Val, "Element"));
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val   : JSON_Value;
         Field : UTF8_String)
         return V.Table_Entry
      is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : V.Table_Entry)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : V.Table_Array) return JSON_Value is
         Data : JSON_Array;
      begin
         for D of   Val loop
            Append (Data, Create (D));
         end loop;
         return Create (Data);
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return V.Table_Array is
         D : constant JSON_Array := Get (Val);
      begin
         return Ret : V.Table_Array (1 .. Length (D)) do
            for I in Ret'Range loop
               Ret (I) := Get (Get (D, I));
            end loop;
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val   : JSON_Value;
         Field : UTF8_String)
         return V.Table_Array
      is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : V.Table_Array)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;
   end Table;

end GNATCOLL.JSON.Support.GNAT.Spitbol;
