pragma Warnings (Off); -- imported for children
with ASIS_UL.Debug;
with ASIS_UL.Dbg_Out; use ASIS_UL;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
use Ada;
pragma Warnings (On);

package  GNATCOLL.JSON.Support.Generate is

   pragma Elaborate_Body;

   Debug_Mode : Boolean renames ASIS_UL.Debug.Debug_Flag_9;

   Assert_Enabled : Boolean := False;
   --  Set to True in body if assertions are enabled. This should really be a
   --  constant, but there's no easy mechanism for that.

end  GNATCOLL.JSON.Support.Generate;
