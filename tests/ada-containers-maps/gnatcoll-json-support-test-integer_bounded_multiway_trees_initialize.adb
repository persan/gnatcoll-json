function GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees_Initialize
   return GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees.Tree
is
   use GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees;
   C : Cursor;
begin
   return Ret : Tree (10) do
      C := Ret.Root;
      Ret.Append_Child (C, 100);
      Ret.Append_Child (C, 200);
      Ret.Append_Child (C, 300);
      C := First_Child (Ret.Root);
      Ret.Append_Child (C, 110);
      Ret.Append_Child (C, 120);
      Ret.Append_Child (C, 130);
      C := Next_Sibling (C);
      Ret.Append_Child (C, 210);
      Ret.Append_Child (C, 220);
      Ret.Append_Child (C, 230);
      C := First_Child (C);
      Ret.Append_Child (C, 211);
   end return;
end GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees_Initialize;
