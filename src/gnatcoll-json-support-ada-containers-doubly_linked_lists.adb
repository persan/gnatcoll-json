package body GNATCOLL.JSON.Support.Ada.Containers.Doubly_Linked_Lists is
  ------------
   -- Create --
   ------------

   function Create (Val : List) return JSON_Value is
      Data : JSON_Array;
   begin
      for I of Val loop
         Append (Data, Create (I));
      end loop;
      return Create (Data);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return List is
      L : JSON_Array := Val.Get;
   begin
      return Ret : List do
         for I in 1 .. Length (L) loop
            Ret.Append (Element_Type'(Get (Get (L, I))));
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return List is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end ;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : List) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end ;

end GNATCOLL.JSON.Support.Ada.Containers.Doubly_Linked_Lists;
