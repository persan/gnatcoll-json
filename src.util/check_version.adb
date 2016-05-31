with GNATCOLL.JSON.Support;
with GNAT.IO;
procedure Check_Version is
   pragma Warnings (Off, "condition is always False");
begin
   if GNATCOLL.JSON.Support.VERSION /= $VERSION then
      raise Program_Error with "Version missmatch:"  & GNATCOLL.JSON.Support.VERSION & " /= " & $VERSION;
   else
      GNAT.IO.Put_Line ($VERSION);
   end if;
end  Check_Version;
