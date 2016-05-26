procedure GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_Initialize
  (Item : in out GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets.Vector)
is
begin
   for I in 1 .. 20 loop
      Item.Append (I);
   end loop;
end GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_Initialize;
