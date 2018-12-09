function GNATCOLL.JSON.Support.Test.Table_VString_Initialize
  return GNAT.Spitbol.Table_VString.Table
is
   use GNAT.Spitbol;
begin
   return Ret : GNAT.Spitbol.Table_VString.Table (10) do
      for I in 1 .. 10 loop
         GNAT.Spitbol.Table_VString.Set (Ret, I'Img, V (I'Img));
      end loop;
   end return;
end GNATCOLL.JSON.Support.Test.Table_VString_Initialize;
