pragma Ada_2012;
with GNAT.Regpat;
with Ada.Text_IO;
package body GNATCOLL.JSON.Support is
   use GNAT.Regpat;
   use Ada.Text_IO;

   Match_Regexp : constant String
   --    12                  3                                       4                                           5     6                                7
     := "(([a-z]+[a-z0-9_]*)|(" & Quote (Start_Indexed_Delimiter) & "(\d+)" & Quote (End_Indexed_Delimiter) & ")|(\d+))(" & Quote (Path_Delimiter) & "|)(.*)";

   TAIL    : constant := 7;
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

   function Get_Path (Val : JSON_Value; Path : UTF8_String) return JSON_Value is
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
            return Get_Path (Val  => Val.Get (Field => Path (Matches (2).First .. Matches (2).Last)),
                             Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         end if;
      elsif Val.Kind = JSON_Array_Type then
         if Matches (5) /= No_Match then
            return Get_Path (Val  => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (5).First .. Matches (5).Last))),
                             Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         else
            return Get_Path (Val  => Get (Arr => Get (Val), Index => Positive'Value (Path (Matches (4).First .. Matches (4).Last))),
                             Path => Path (Matches (TAIL).First .. Matches (TAIL).Last));
         end if;
      else
         return JSON_Null;
      end if;
   end Get_Path;

   procedure Mapper is new Gen_Map_JSON_Object (JSON_Value);
   procedure Append (Path : String; L : in out JSON_Value; R : JSON_Value; Raise_On_Kind_Missmatch : Boolean)  is
      procedure Map
        (User_Object : in out JSON_Value;
         Name        : UTF8_String;
         Value       : JSON_Value) is
         Temp        : JSON_Value;
      begin
         if User_Object.Has_Field (Name) then
            Temp := User_Object.Get (Name);
            if Temp.Kind in JSON_Elementary_Value_Type and then Value.Kind = Temp.Kind then
               Set_Field (User_Object, Name, Value);
            end if;
--                 when JSON_Array_Type => null;
--                 when JSON_Object_Type => null;
         else
            User_Object.Set_Field (Name, Value);
         end if;
      end Map;

   begin
      if R.Kind /= L.Kind and then Raise_On_Kind_Missmatch then
         raise Constraint_Error with Path & ":" & R.Kind'Img & "/=" & L.Kind'Img;
      end if;
      if L.Kind = JSON_Object_Type then
         Mapper (R, Map'Access, L);
      end if;
   end Append;

   function "or" (L, R : JSON_Value) return JSON_Value is
   begin
      return Ret : JSON_Value := L do
         Append ("", Ret, R, False);
      end return;
   end "or";

   function "+" (L, R : JSON_Value) return JSON_Value is
   begin
      return Ret : JSON_Value := L do
         Append ("", Ret, R, True);
      end return;
   end "+";

   function Normalize_Field_Names (Src       : JSON_Value;
                                   Normalize : not null access function (Src : String) return String := Default_Normalize'Access) return JSON_Value is
      pragma Unreferenced (Normalize);
   begin
      return Src;
   end Normalize_Field_Names;

end GNATCOLL.JSON.Support;
