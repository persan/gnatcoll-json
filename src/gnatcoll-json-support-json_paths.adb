pragma Ada_2012;
with GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;
package body GNATCOLL.JSON.Support.JSON_Paths is
      use GNAT.Regpat;

   Matcher : constant Pattern_Matcher := Compile
   --         12                  3                                       4                                             56                                7
     ("^" &  "(([a-z]+[a-z0-9_]*)|(" & Quote (Start_Indexed_Delimiter) & "(\d+)" & Quote (Start_Indexed_Delimiter) & "))((" & Quote (Path_Delimiter) & "|)(.*)|)");

   procedure Debug (Matches : GNAT.Regpat.Match_Array; Path : String) with Ghost => True;
   procedure Debug (Matches : GNAT.Regpat.Match_Array; Path : String) is
   begin
      for I in Matches'Range loop
         Put_Line (I'Img & "(" & Matches (I).First'Img & "," & Matches (I).Last'Img & ")=> " &
                   (if Matches (I) = No_Match
                      then
                         "<NO_MATCH>"
                      else
                         Path (Matches (I).First .. Matches (I).Last))
                  );
      end loop;
      New_Line;
   end Debug;

   function Get (Val : JSON_Value; Path : UTF8_String) return JSON_Value is
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
   begin
      pragma Debug (Put_Line ("Value_Kind => " & Val.Kind'Img & ", Path => '" & Path & "'"));
      if Val.Kind = JSON_Object_Type then
         Match (Matcher, Path, Matches);
         pragma Debug (Debug (Matches, Path));
         if Matches (7) = No_Match or else (Matches (7).Last < Matches (7).First) then
            return Val.Get (Field => Path (Matches (2).First .. Matches (2).Last));
         else
            return Get (Val.Get (Field => Path (Matches (2).First .. Matches (2).Last)), Path (Matches (7).First .. Matches (7).Last));
         end if;
      elsif Val.Kind = JSON_Array_Type then
         return Get (Val => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (4).First .. Matches (4).Last))),
                     Path => Path (Matches (7).First .. Matches (7).Last));
      else
         return JSON_Null;
      end if;
      --     exception
      --        when others =>
      --           return JSON_Null;
   end Get;

end GNATCOLL.JSON.Support.JSON_Paths;
