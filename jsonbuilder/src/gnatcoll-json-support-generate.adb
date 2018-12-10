package body  GNATCOLL.JSON.Support.Generate is

   procedure Set_Assert_Enabled;
   --  This is to get around the syntactic restrictions of pragma Debug, which
   --  takes a procedure call as parameter (pretending that's an expression).
   --  The call below will not happen in assertions-off mode, thus leaving
   --  Assert_Enabled = False.

   procedure Set_Assert_Enabled is
   begin
      Assert_Enabled := True;
   end Set_Assert_Enabled;

begin
   pragma Debug (Set_Assert_Enabled);

end  GNATCOLL.JSON.Support.Generate;
