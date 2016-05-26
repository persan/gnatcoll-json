procedure GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_Initialize
  (Item : in out GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors.Vector)
is
begin
   for I in 1 .. 20 loop
      Item.Append (I);
   end loop;
end GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_Initialize;
