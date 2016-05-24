package body Data is

   ---------
   -- Add --
   ---------

   procedure Add (Self : in out Rec; Data : String) is
   begin
      Self.Name  := Ada.Strings.Unbounded.To_Unbounded_String (Data);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Self : in out Rec; Data : Integer) is
   begin
      Self.Fox.Append (Data);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Self : in out Rec; Data : Float) is
   begin
      Self.Count := Data;
   end Add;

end Data;
