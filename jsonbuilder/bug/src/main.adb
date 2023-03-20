with Ada.Text_IO;
with GNAT.MBBS_Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with GNAT.Formatted_String;
procedure Main is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   subtype Rt is Natural range 8 .. 64;
   package Random is new GNAT.MBBS_Discrete_Random (Rt);

   use Ada.Strings.Fixed;

   procedure Setup is
      use GNAT.Formatted_String;
      G      : Random.Generator;
      F      : File_Type;
   begin
      F.Create (Out_File, "data.txt");
      for I in Integer'(1) .. 2000 loop
         F.Put_Line ((-(+("%d") & I)) & G.Random * '.');
      end loop;
      F.Close;
   end Setup;

   procedure Test is
      F      : File_Type;
   begin
      F.OPen (In_File, "data.txt");
      while not F.End_Of_File loop
         Put_Line (F.Get_Line);
      end loop;
      F.Close;
   end;
begin
   -- Setup;
   Test;
end Main;
