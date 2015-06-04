package body GNATCOLL.JSON.Support.System.Storage_Elements.JSON is
   function Image (Item : Storage_Element) return String is
      Map : constant array (Storage_Element'(0) .. Storage_Element'(15)) of Character := "0123456789ABCDEF";
   begin
      return Ret : String (1 .. 2) do
         Ret (1) := Map (Item / Storage_Element'(16));
         Ret (2) := Map (Item mod Storage_Element (16));
      end return;
   end;

   function Image (Item : Storage_Array) return String is
      Cursor : Natural := 1;
   begin
      return Ret : String (1 .. Item'Length * 2 ) do
         for I of Item loop
            Ret (Cursor .. Cursor + 1) := Image (I);
            Cursor := Cursor + 2;
         end loop;
      end return;
   end;

   function Image (Base : Standard.System.Address; Length : Storage_Offset ) return String is
      Buffer : Storage_Array (1 .. Length) with
        Import => True,
        Address => Base;
   begin
      return Image (Buffer);
   end;

   function Value (Item : String) return Storage_Element is
      Map : constant array (Character'('0') .. Character'('F')) of Storage_Element :=
              ('0' => 0,
               '1' => 1,
               '2' => 2,
               '3' => 3,
               '4' => 4,
               '5' => 5,
               '6' => 6,
               '7' => 7,
               '8' => 8,
               '9' => 9,
               'A' => 10,
               'B' => 11,
               'C' => 12,
               'D' => 13,
               'E' => 14,
               'F' => 15,
               others => 0);
   begin
      return Map (Item (ITEM'First)) * 16 + Map (Item (ITEM'Last));
   end;
   function Value (Item : String) return Storage_Array is
      Cursor : Natural := Item'First;
   begin
      return Ret : Storage_Array (1 .. Item'Length / 2) do
         for I in Ret'Range loop
            Ret (I) := value(Item (Cursor .. Cursor + 1));
            Cursor := Cursor + 2;
         end loop;
      end return;
   end;

   ------------
   -- Create --
   ------------

   function Create (Val : Storage_Offset) return JSON_Value is
   begin
      return Create (Image (Val'Address, Val'Size / 8));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Storage_Offset is
   begin
      return Storage_Offset(integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val : JSON_Value;
      Field : UTF8_String)
      return Storage_Offset
   is
   begin
      return Storage_Offset(integer'(Get (Val, Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Storage_Offset)
   is
   begin
      Set_Field (Val, Field_Name, Integer (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create (Val : Storage_Element) return JSON_Value is
   begin
      return Create (Image (Val'Address, Val'Size / 8));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Storage_Element is
   begin
      return Storage_Element(integer'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val : JSON_Value;
      Field : UTF8_String)
      return Storage_Element
   is
   begin
      return Storage_Element(integer'(Get (Val, Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Storage_Element)
   is
   begin
      Set_Field (Val, Field_Name, Integer (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create (Val : Storage_Array) return JSON_Value is
   begin
      return Create (Image (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Storage_Array is
   begin
      return value(String'(Get (Val)));
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val : JSON_Value;
      Field : UTF8_String)
      return Storage_Array
   is
   begin
      return value(String'(Get (Val, Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------Inline_Always

   procedure Set_Field
     (Val : JSON_Value;
      Field_Name : UTF8_String;
      Field  : Storage_Array)
   is
   begin
      Set_Field (Val, Field_Name, Image (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.System.Storage_Elements.JSON;
