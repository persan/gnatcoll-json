separate (GNATCOLL.JSON.Support.Generate.Writer)
procedure Write_Body (Self : in out JSON_Writer) is
   use type Libadalang.Common.Ada_Node_Kind_Type;



   procedure Process_ADA_DERIVED_TYPE_DEF (Node : Derived_Type_Def; Name : Unbounded_String) is
      ABSTRACT_ABSENT     : Boolean := False;
      LIMITED_ABSENT      : Boolean := False;
      SYNCHRONIZED_ABSENT : Boolean := False;
      SUBTYPE_INDICATION  : Boolean := False;
      PARENT_LIST         : Boolean := False;
      WITH_PRIVATE_ABSENT : Boolean := False;
      Is_Record           : Boolean := False;
      Done                : Boolean := False;
   begin
      Append (Self.Buffer, "   --  --------------------------------------------------------------------" & Ascii.Lf);
      Append (Self.Buffer, "   --  ADA_DERIVED_TYPE_DEF: " & Name & " " & Ascii.Lf);
      Append (Self.Buffer, "   --  --------------------------------------------------------------------" & Ascii.Lf);

      for Ix in Node.First_Child_Index .. Node.Last_Child_Index loop
         declare
            CC    : Libadalang.Analysis.Ada_Node := Node.Child (Ix);
         begin
            if not CC.Is_Null then
               case CC.Kind is
                  when ADA_ABSTRACT_ABSENT =>
                     ABSTRACT_ABSENT := True;
                  when ADA_LIMITED_ABSENT =>
                     LIMITED_ABSENT := True;
                  when ADA_SYNCHRONIZED_ABSENT =>
                     SYNCHRONIZED_ABSENT := True;
                  when ADA_SUBTYPE_INDICATION =>
                     null;
                  when ADA_PARENT_LIST =>
                     null;
                  when ADA_WITH_PRIVATE_ABSENT =>
                     WITH_PRIVATE_ABSENT := True;
                  when ADA_RECORD_DEF =>
                     Is_Record := True;
                  when others =>
                     Append (Self.Buffer , "   --  " & CC.Kind'Img & ASCII.LF);
               end case;
            end if;
         end;
      end loop;
      if not Done then
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
                   "       return " & Name & "'(Get (JSON_Value'(Get (Val, Field))));" & ASCII.LF &
                   "   end Get;" & ASCII.LF & ASCII.LF);

         Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") is" & ASCII.LF &
                   "   begin" & ASCII.LF &
                   "      Set_Field (Val, Field_Name, Create (Field));" & ASCII.LF &
                   "   end Set_Field;" &  Ascii.Lf & Ascii.Lf & Ascii.Lf);
      end if;
   end;

   function Process_ADA_TYPE_DECL (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
      Name : Unbounded_String;
   begin
      for I in Node.First_Child_Index .. Node.Last_Child_Index loop
         declare
            C    : Libadalang.Analysis.Ada_Node := Node.Child (I);
         begin

            if not C.Is_Null then
               case C.Kind is
                  when ADA_DEFINING_NAME =>
                     Name := To_Unbounded_String (Image (As_Defining_Name (C).F_Name.Text));
                  when ADA_SIGNED_INT_TYPE_DEF =>
                     null;
                  when ADA_ARRAY_TYPE_DEF =>
                     null;
                  when ADA_ENUM_TYPE_DEF =>
                     null;

                  when ADA_MOD_INT_TYPE_DEF =>
                     null;

                  when ADA_FLOATING_POINT_DEF =>
                     null;

                  when ADA_DERIVED_TYPE_DEF =>
                     Process_ADA_DERIVED_TYPE_DEF (As_Derived_Type_Def (C), Name);

                  when ADA_RECORD_TYPE_DEF =>
                     Self.Process_ADA_Record_TYPE_DEF_Impl (As_RECORD_TYPE_DEF (C), Name);

                  when ADA_INTERFACE_TYPE_DEF | ADA_ASPECT_SPEC =>
                     null;
                  when others =>
                     Append (Self.Buffer, "   --!!  " & C.Kind'Img & "  " &  Ascii.Lf);
               end case;
            end if;
         end;
      end loop;
      return Libadalang.Common.Over;
   end Process_ADA_TYPE_DECL;

   function Process_ADA_PUBLIC_PART (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
   begin
      for I in Node.First_Child_Index .. Node.Last_Child_Index loop
         declare
            C : Libadalang.Analysis.Ada_Node := Node.Child (I);
         begin

            if not C.Is_Null then
               --                Ada.Text_IO.Put_Line ("3   -> " & C.Kind'Img);
               case C.Kind is
                  when ADA_ADA_NODE_LIST =>
                     for N of As_Ada_Node_List (C) loop
                        --                             Ada.Text_IO.Put_Line ("3.   -> " & N.Kind'Img);
                        case N.Kind is
                           when ADA_TYPE_DECL =>
                              N.Traverse (Process_ADA_TYPE_DECL'Access);
                           when others =>
                              null;
                        end case;
                     end loop;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
      return Libadalang.Common.Over;
   end Process_ADA_PUBLIC_PART;

   function Process_ADA_PACKAGE_DECL (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
   begin

      for I in Node.First_Child_Index .. Node.Last_Child_Index loop
         declare
            C : Libadalang.Analysis.Ada_Node := Node.Child (I);
         begin

            if not C.Is_Null then
               case C.Kind is
                  when ADA_DEFINING_NAME =>
                     Self.Package_Name := To_Unbounded_String (Image (As_Defining_Name (C).F_Name.Text) & ".JSON");
                     Append (Self.Buffer, "package body " & Self.Package_Name & " is" & ASCII.LF & ASCII.LF);
                  when ADA_PUBLIC_PART =>
                     C.Traverse (Process_ADA_PUBLIC_PART'Access);
                  when ADA_END_NAME =>
                     Append (Self.Buffer, "end " & Self.Package_Name & ";");
                  when others =>
                     null;
               end case;
            end if;
         end;
      end loop;
      return Libadalang.Common.Over;
   end Process_ADA_PACKAGE_DECL;

   function Process_ADA_LIBRARY_ITEM (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
   begin
      for I in Node.First_Child_Index .. Node.Last_Child_Index loop
         declare
            C : Libadalang.Analysis.Ada_Node := Node.Child (I);
         begin
            if C.Kind = ADA_PACKAGE_DECL then
               C.Traverse (Process_ADA_PACKAGE_DECL'Access);
            end if;
         end;
      end loop;
      return Libadalang.Common.Over;
   end Process_ADA_LIBRARY_ITEM;


   function Process_PackageDecl (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
   begin
      for I in Node.First_Child_Index .. Node.Last_Child_Index loop
         if Node.Child (I).Kind = ADA_LIBRARY_ITEM then
            Node.Child (I).Traverse (Process_ADA_LIBRARY_ITEM'Access);
         end if;
      end loop;
      return Libadalang.Common.Over;
   end Process_PackageDecl;


   function Process_Spec (Node : Libadalang.Analysis.Ada_Node'Class) return Libadalang.Common.Visit_Status
   is
   begin
      if Node.Kind = Libadalang.Common.Ada_Package_Decl then
         Self.Unit.Traverse (Process_PackageDecl'Access);
      end if;
      return Libadalang.Common.Into;
   end Process_Spec;

begin
   Self.Buffer := Null_Unbounded_String;
   Self.Unit.Traverse (Process_Spec'Access);
   Create (Self.Outf, Out_File, "tests.codegen/" & Ada2file (To_String (Self.Package_Name)) & ".adb");
   Put_Line (Self.Outf, Self.Buffer);
   Close (Self.Outf); --  Self.Unit.Traverse (Process'Access);
end Write_Body;
