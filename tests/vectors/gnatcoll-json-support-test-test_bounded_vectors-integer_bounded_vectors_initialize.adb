function GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_Initialize return
  GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.Vector is

begin
   return Item : GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors.Vector (20) do
      for I in 1 .. 20 loop
         Item.Append (I);
      end loop;
   end return;
end GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_Initialize;
