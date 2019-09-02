pragma Ada_2012;
with GNAT.Regpat;

package body GNATCOLL.JSON.Support.JSON_Paths is
   use GNAT.Regpat;

   Match_Regexp : constant String
   --    12                  3                                       4                                           5                                6
     := "(([a-z]+[a-z0-9_]*)|(" & Quote (Start_Indexed_Delimiter) & "(\d+)" & Quote (End_Indexed_Delimiter) & "))(" & Quote (Path_Delimiter) & "|)(.*)";

   TAIL : constant := 6;
   Matcher : constant Pattern_Matcher := Compile (Match_Regexp, GNAT.Regpat.Case_Insensitive);

   function Get (Val : JSON_Value; Path : UTF8_String) return JSON_Value is
      Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Matcher));
   begin
      Match (Matcher, Path, Matches);
      if Val.Kind in JSON_Elementary_Value_Type then
         return Val;
      elsif Val.Kind = JSON_Object_Type then
         if Matches (TAIL) = No_Match or else (Matches (TAIL).Last < Matches (TAIL).First) then
            return Val.Get (Field => Path (Matches (2).First .. Matches (2).Last));
         else
            return Get (Val  => Val.Get (Field => Path (Matches (2).First .. Matches (2).Last)),
                        Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         end if;
      elsif Val.Kind = JSON_Array_Type then
         return Get (Val  => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (4).First .. Matches (4).Last))),
                     Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
      else
         return JSON_Null;
      end if;
   end Get;

end GNATCOLL.JSON.Support.JSON_Paths;
