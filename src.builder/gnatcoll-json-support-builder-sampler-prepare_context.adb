-----------------------------------------------------------------------

--  This version of the body is for non-GNSA version

with ASIS_UL.Common;
with ASIS_UL.Compiler_Options;

separate (GNATCOLL.JSON.Support.Builder.Sampler)
procedure Prepare_Context is
   Success : Boolean := False;
begin
   if Tree_Exists and then Reuse_Tree then
      return;
   end if;

   Compile
     (File_Name,
      Arg_List.all,
      Success,
      GCC          => ASIS_UL.Common.Gcc_To_Call,
      Display_Call => Debug_Flag_C);

   if not Success then
      Error ("cannot create the tree file for " & File_Name.all);
      raise Parameter_Error;
   else
      Tree_Exists := True;
   end if;

   Asis.Implementation.Initialize ("-ws -sv");

   Associate
     (My_Context,
     "My_Context",
     "-C1 " & To_Wide_String (Tree_Name.all));

   Open (My_Context);

   if Debug_Flag_T then
      Print_Tree_Sources;
   end if;
end Prepare_Context;
