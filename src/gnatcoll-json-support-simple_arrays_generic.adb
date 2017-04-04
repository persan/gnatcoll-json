package body GNATCOLL.JSON.Support.Simple_Arrays_Generic is

   ------------
   -- Create --
   ------------

   function Create (Val : Array_Type) return JSON_Array is
   begin
      return Data : JSON_Array do
         for I of Val loop
            Append (Data, Create (I));
         end loop;
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Array) return Array_Type is
      Cursor : Index_Type;
      First   : constant Index_Type := Index_Type'First;
      Len     : constant Integer := Length (Val);
      Last    : constant Index_Type := First+Index_Type(Len);
   begin
      return Ret : Array_Type (First .. Last) do
         Cursor :=    Ret'First;
         for I in 1 .. Length (Val) loop
            Ret (Cursor) :=  (Element_Type'(Get (Get (Val, I))));
            Cursor := Index_Type'Succ (Cursor);
         end loop;
      end return;
   end Get;

end GNATCOLL.JSON.Support.Simple_Arrays_Generic;
