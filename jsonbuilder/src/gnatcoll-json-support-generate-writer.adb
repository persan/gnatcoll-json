pragma Ada_2012;
with Libadalang.Common;
with Libadalang.Analysis; use Libadalang.Analysis;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Ada.Text_IO; use Ada.Text_IO;
with Gnat.Case_Util;
with GNAT.Source_Info;
with Ada.Tags;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Characters.Conversions;
with GNATCOLL.JSON.Support.Generate.String_Vectors;
with GNAT.Source_Info;
package body GNATCOLL.JSON.Support.Generate.Writer is
   use Libadalang.Common;
   use Langkit_Support.Text;
   function Ada2file (Name : String ) return String is
   begin
      return Ret : String := Name do
         Gnat.Case_Util.To_Lower (Ret);
         for I of Ret loop
            if I = '.' then
               I := '-';
            end if;
         end loop;
      end return;
   end;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self : in out JSON_Writer;
      Unit :         Libadalang.Analysis.Ada_Node'Class)
   is
   begin
      Self.Unit := Unit'Unrestricted_Access;
      Self.Write_Spec;
      Self.Write_Body;
   end Write;


   procedure Process_ADA_ARRAY_TYPE_DEF_Spec (Self : in out JSON_Writer;
                                              Node : Libadalang.Analysis.Array_Type_Def;
                                              Name : String) is
      Index_Name   : Unbounded_String;
      Element_Name : Unbounded_String;
   begin

      Append (Self.Buffer, "   --  ------------------------------------" & Ascii.Lf);
      Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
      Append (Self.Buffer, "   --  ------------------------------------" & Ascii.Lf);
      for C of Node.Children loop
         if not C.Is_Null then
            case C.Kind is
               when ADA_UNCONSTRAINED_ARRAY_INDICES =>
                  for CC of C.Children loop
                     if not CC.Is_Null then
                        case CC.Kind is
                           when ADA_UNCONSTRAINED_ARRAY_INDEX_LIST =>
                              for CCc of Cc.Children loop
                                 if not CCc.Is_Null then
                                    case CcC.Kind is
                                       when ADA_UNCONSTRAINED_ARRAY_INDEX =>
                                          for CCcC of Ccc.Children loop
                                             if not CCcc.Is_Null then
                                                case CcCc.Kind is
                                                   when ADA_SUBTYPE_INDICATION =>
                                                      for Ccccc of Cccc.Children loop
                                                         if not CCccC.Is_Null then
                                                            case CCCCC.Kind is
                                                            when ADA_NOT_NULL_ABSENT | ADA_ALIASED_ABSENT | ADA_CONSTANT_ABSENT =>
                                                               null;
                                                               when ADA_IDENTIFIER =>
                                                                  Index_Name := To_Unbounded_String (Ada.Characters.Conversions.To_String (As_Identifier (CCCCC).Text));
                                                               when others =>
                                                                  Append (Self.Buffer , "   --5      " & CCCCC.Kind'Img & ASCII.LF);
                                                            end case;
                                                         end if;
                                                      end loop;
                                                   when others =>
                                                      Append (Self.Buffer , "   --4      " & CCCC.Kind'Img & ASCII.LF);
                                                end case;
                                             end if;
                                          end loop;
                                       when others =>
                                          Append (Self.Buffer , "   --3      " & CCC.Kind'Img & ASCII.LF);
                                    end case;
                                 end if;
                              end loop;
                           when others =>
                              Append (Self.Buffer , "   --2     " & CC.Kind'Img & ASCII.LF);
                        end case;
                     end if;
                  end loop;
               when ADA_CONSTRAINED_ARRAY_INDICES =>
                  null;
               when ADA_COMPONENT_DEF =>

                  for C_1 of C.Children loop
                     if not C_1.Is_Null then
                        case C_1.Kind is
                           when ADA_SUBTYPE_INDICATION =>
                              for C_2 of C_1.Children loop
                                 if not C_2.Is_Null then
                                    case C_2.Kind is
                                       when ADA_NOT_NULL_ABSENT | ADA_ALIASED_ABSENT | ADA_CONSTANT_ABSENT =>
                                          null;
                                       when ADA_IDENTIFIER =>
                                          Element_Name := To_Unbounded_String (Ada.Characters.Conversions.To_String (As_Identifier (C_2).Text));
                                       when others =>
                                          Append (Self.Buffer , "   -- " & GNAT.Source_Info.Source_Location & "      " & C_2.Kind'Img & ASCII.LF);
                                    end case;
                                 end if;
                              end loop;

                           when ADA_NOT_NULL_ABSENT | ADA_ALIASED_ABSENT | ADA_CONSTANT_ABSENT =>
                              null;
                           when others =>
                              Append (Self.Buffer , "   -- " & GNAT.Source_Info.Source_Location & "      " & C_1.Kind'Img & ASCII.LF);
                              Print (C_1);

                        end case;
                     end if;
                  end loop;
               when others =>
                  Append (Self.Buffer , "   --1  " & C.Kind'Img & ASCII.LF);
            end case;
         end if;
      end loop;
      Append (Self.Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Arrays_Generic" & ASCII.LF &
                "      (" & Index_Name & "," & Element_Name & "," &  Name & ", Create, Get, Create, Get); " & ASCII.LF );

      Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & Ascii.Lf);
      Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
      Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
      Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & Ascii.Lf & Ascii.Lf & Ascii.Lf);
      Self.Withs.Include ("GNATCOLL.JSON.Support.Arrays_Generic");

      Append (Self.Buffer, Ascii.Lf & Ascii.Lf);
   end;
   procedure Process_ADA_Record_TYPE_DEF_Impl (Self : in out JSON_Writer; Node : Libadalang.Analysis.Ada_Node'Class; Name : Unbounded_String) is separate;

   procedure Write_Spec (Self : in out JSON_Writer) is separate;

   procedure Write_Body (Self : in out JSON_Writer) is separate;

end GNATCOLL.JSON.Support.Generate.Writer;
