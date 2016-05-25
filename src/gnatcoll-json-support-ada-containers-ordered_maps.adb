package body GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps is

   function Create (Val : Map_Entry) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("Key", Create (Val.Key));
         Ret.Set_Field ("Element", Create (Val.Element));
      end return;
   end Create;

   function Get (Val : JSON_Value) return Map_Entry is
   begin
      return Ret : Map_Entry do
         Ret.Key := Get_Name (Val, "Key");
         Ret.Element := Get_Name (Val, "Element");
      end return;
   end Get;

   ------------
   -- Create --
   ------------

   function Create (Val : Map) return JSON_Value is
      V : JSON_Array;
      procedure Process (Position : Cursor) is
      begin
         Append (V, Create (Map_Entry'((Key (Position), Element (Position)))));
      end Process;
   begin
      Val.Iterate (Process'Access);
      return Create (V);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Map is
      L : constant JSON_Array := Val.Get;
   begin
      return Ret : Map do
         for I in 1 .. Length (L) loop
            declare
               Value : constant Map_Entry := Get (Get (L, I));
            begin
               Ret.Include (Value.Key, Value.Element);
            end;
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Map is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Map)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps;
