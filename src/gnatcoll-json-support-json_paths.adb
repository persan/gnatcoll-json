pragma Ada_2012;
with GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;

package body GNATCOLL.JSON.Support.JSON_Paths is
   use GNAT.Regpat;

   Match_Regexp : constant String
   --    12                  3                                       4                                           5     6                                7
     := "(([a-z]+[a-z0-9_]*)|(" & Quote (Start_Indexed_Delimiter) & "(\d+)" & Quote (End_Indexed_Delimiter) & ")|(\d+))(" & Quote (Path_Delimiter) & "|)(.*)";

   TAIL : constant := 7;
   Matcher : constant Pattern_Matcher := Compile (Match_Regexp, GNAT.Regpat.Case_Insensitive);

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
   end Debug;

   function Get (Val : JSON_Value; Path : UTF8_String) return JSON_Value is
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
   begin
      pragma Debug (Put_Line ("Value_Kind => " & Val.Kind'Img & ", Path => '" & Path & "'"));
      Match (Matcher, Path, Matches);
      pragma Debug (Debug (Matches, Path));
      if Val.Kind in JSON_Elementary_Value_Type then
         return Val;
      elsif Val.Kind = JSON_Object_Type then
         if Matches (TAIL) = No_Match or else (Matches (TAIL).Last < Matches (TAIL).First) then
            return Val.Get (Field => Path (Matches (2).First .. Matches (2).Last));
         else
            pragma Debug (Put_Line
                          ("Field =>" & Path (Matches (2).First .. Matches (2).Last) &
                               ", Path =>" & Path (Matches (TAIL).First .. Matches (TAIL).Last)));
            return Get (Val  => Val.Get (Field => Path (Matches (2).First .. Matches (2).Last)),
                        Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         end if;
      elsif Val.Kind = JSON_Array_Type then
         if Matches (5) /= No_Match then
            return Get (Val  => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (5).First .. Matches (5).Last))),
                        Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         else
            return Get (Val  => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (4).First .. Matches (4).Last))),
                        Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         end if;
      else
         return JSON_Null;
      end if;
   end Get;

end GNATCOLL.JSON.Support.JSON_Paths;
