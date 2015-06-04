package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees is

   ------------
   -- Create --
   ------------

   function Create (Val : Tree) return JSON_Value is
      Ret : JSON_Array;
   begin
      for I of Val loop
            Append (Ret, Create (I));
      end loop;
      return R : JSON_Value := Create_Object do
         Set_Field (R, "Capacity", Val.Capacity);
         Set_Field (R, "Data", Ret);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Tree is
   begin
      return Ret : Tree (Capacity =>  Get (Val, "Capacity")) do
         null;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Tree is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end ;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Tree) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end ;

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
