with GNAT.Spitbol.Table_VString;
function Table_VString_Initialize
  return GNAT.Spitbol.Table_VString.Table
is
begin
   return Ret : GNAT.Spitbol.Table_VString.Table (10) do
      for I in 1 .. 10 loop
      end loop;
   end return; -- Causes the compiler to blowup. with Storage_Error
end Table_VString_Initialize;
