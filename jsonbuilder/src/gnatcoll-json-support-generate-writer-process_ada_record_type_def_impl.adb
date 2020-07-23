separate (GNATCOLL.JSON.Support.Generate.Writer)
procedure Process_ADA_Record_TYPE_DEF_Impl (Self : in out JSON_Writer; Node : Libadalang.Analysis.Ada_Node'Class; Name : Unbounded_String) is
   Components : GNATCOLL.JSON.Support.Generate.String_Vectors.Vector;
   F_NAME       : ADA.Strings.Unbounded.Unbounded_String;
begin
   for C1 of  Node.Children loop
      if not C1.Is_Null then
         case C1.Kind is
            when ADA_RECORD_DEF =>
               for C2 of  C1.Children loop
                  if not C2.Is_Null then
                     case C2.Kind is
                        when ADA_COMPONENT_LIST =>
                           for C3 of  C2.Children loop
                              if not C3.Is_Null then
                                 case C3.Kind is
                                    when ADA_ADA_NODE_LIST =>
                                       for C4 of  C3.Children loop
                                          if not C4.Is_Null then

                                             Append (Self.Buffer, "-- +4" & ASCII.LF);
                                             case C4.Kind is
                                                when ADA_COMPONENT_DECL =>
                                                   for C5 of C4.Children loop
                                                      if not C5.Is_Null then
                                                         case C5.Kind is
                                                            when ADA_DEFINING_NAME_LIST =>
                                                               for C6 of C5.Children loop
                                                                  if not C6.Is_Null then
                                                                     case C6.Kind is
                                                                        when ADA_DEFINING_NAME =>
                                                                           F_NAME := To_Unbounded_String (Image (As_Defining_Name (C6).F_Name.Text));
                                                                           Append (Self.Buffer , "   -- +F_NAME: " & F_NAME);
                                                                        when others =>
                                                                           Append (Self.Buffer , "   -- -6- ADA_DEFINING_NAME_LIST  " & C6.Kind'Img & ASCII.LF);
                                                                     end case;
                                                                  end if;
                                                               end loop;
                                                            when ADA_ASPECT_SPEC =>
                                                               for C6 of C5.Children loop
                                                                  if not C6.Is_Null then
                                                                     case C6.Kind is
                                                                     when others =>
                                                                        Append (Self.Buffer , "   -- -6- ADA_ASPECT_SPEC  " & C6.Kind'Img & ASCII.LF);
                                                                        Print (C6);
                                                                     end case;
                                                                  end if;
                                                               end loop;
                                                            when others =>
                                                               Append (Self.Buffer , "   -- -5- ADA_DEFINING_NAME_LIST " & C5.Kind'Img & ASCII.LF);
                                                         end case;
                                                      end if;
                                                   end loop;

                                             when others =>
                                                Append (Self.Buffer , "   -- -4- ADA_COMPONENT_DECL " & C4.Kind'Img & ASCII.LF);
                                             end case;
                                          end if;
                                       end loop;
                                    when others =>
                                       Append (Self.Buffer , "   -- -3- " & C3.Kind'Img & ASCII.LF);
                                 end case;
                              end if;
                           end loop;
                        when others =>
                           Append (Self.Buffer , "   --  -2- " & C2.Kind'Img & ASCII.LF);
                     end case;

                  end if;
               end loop;

            when others =>
               Append (Self.Buffer , "   --  -1- " & C1.Kind'Img & ASCII.LF);
         end case;
      end if;
   end loop;
   Append (Self.Buffer, "   --  --------------------------------------------------------------------" & Ascii.Lf);
   Append (Self.Buffer, "   --  ADA_RECORD_TYPE_DEF:" & Name & " " & Ascii.Lf);
   Append (Self.Buffer, "   --  --------------------------------------------------------------------" & Ascii.Lf);
   Append (Self.Buffer,
           "   function Create (Val : " & Name & ") return JSON_Value is" & Ascii.Lf &
             "   begin" & ASCII.LF &
             "      return ret : JSON_Value := Create_Object do" & ASCII.LF &
             "         null;" & ASCII.LF &
             "      end return;" & ASCII.LF &
             "   end Create;" & ASCII.LF & ASCII.LF);

   Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " is" & Ascii.Lf &
             "   begin" & ASCII.LF &
             "      return ret : " & Name & " do" & ASCII.LF &
             "         null;" & ASCII.LF &
             "      end return;" & ASCII.LF &
             "   end Get;" & ASCII.LF & ASCII.LF);
   Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " is" & Ascii.Lf &
             "   begin" & ASCII.LF &
             "      return ret : " & Name & " do" & ASCII.LF &
             "         null;" & ASCII.LF &
             "      end return;" & ASCII.LF &
             "   end Get;" & ASCII.LF & ASCII.LF);
   Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") is" & ASCII.LF &
             "   begin" & ASCII.LF &
             "      Set_Field (Val, Field_Name, Create (Field));" & ASCII.LF &
             "   end Set_Field;" &  Ascii.Lf &  Ascii.Lf);
   Append (Self.Buffer, "   procedure Map_JSON_Value (User_Object : in out " & Name & ";" & ASCII.LF &
             "                             Name        : UTF8_String;" & ASCII.LF &
             "                             Value       : JSON_Value) is " & ASCII.LF &
             "   begin" & ASCII.LF &
             "      null;" & ASCII.LF &
             "   end Map_JSON_Value;" &  Ascii.Lf);

   Append (Self.Buffer, Ascii.Lf & Ascii.Lf);
end;
