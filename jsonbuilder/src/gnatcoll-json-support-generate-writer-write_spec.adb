separate (GNATCOLL.JSON.Support.Generate.Writer)
procedure Write_Spec (Self : in out JSON_Writer) is
   use type Libadalang.Common.Ada_Node_Kind_Type;

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
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Integer_Generic (" & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & Ascii.Lf);
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);
                     Self.Withs.Include ("GNATCOLL.JSON.Support.Integer_Generic");

                  when ADA_ENUM_TYPE_DEF =>
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Enumeration_Generic (" & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & Ascii.Lf);
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);

                     Self.Withs.Include ("GNATCOLL.JSON.Support.Enumeration_Generic");

                  when ADA_MOD_INT_TYPE_DEF =>
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Modular_Generic (" & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & Ascii.Lf);
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);
                     Self.Withs.Include ("GNATCOLL.JSON.Support.Modular_Generic");

                  when ADA_FLOATING_POINT_DEF =>
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Float_Generic (" & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & Ascii.Lf);
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);
                     Self.Withs.Include ("GNATCOLL.JSON.Support.Float_Generic");

                  when ADA_DERIVED_TYPE_DEF =>
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);

                  when ADA_RECORD_TYPE_DEF =>
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, "   --  " & Name & " " & Ascii.Lf);
                     Append (Self.Buffer, "   --" & Ascii.Lf);
                     Append (Self.Buffer, "   function Create (Val : " & Name & ") return JSON_Value;" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value) return " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ");" & Ascii.Lf );
                     Append (Self.Buffer, "   procedure Map_JSON_Value (User_Object : in out " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "                             Name        : UTF8_String;" & Ascii.Lf);
                     Append (Self.Buffer, "                             Value       : JSON_Value);" & Ascii.Lf);
                     Append (Self.Buffer, "   procedure Map_JSON_Object is new Gen_Map_JSON_Object (" & Name & ");" & Ascii.Lf);
                     Append (Self.Buffer, "   -- Map_JSON_Object( Val         : JSON_Value;" & Ascii.Lf);
                     Append (Self.Buffer, "   --                  CB          : access procedure (User_Object : in out " & Name & ";" & Ascii.Lf);
                     Append (Self.Buffer, "   --                                                  Name        : UTF8_String;" & Ascii.Lf);
                     Append (Self.Buffer, "   --                                                  Value       : JSON_Value);" & Ascii.Lf);
                     Append (Self.Buffer, "   --                  User_Object : in out Abstract_Record)" & Ascii.Lf );
                     Append (Self.Buffer, "   --  -------------------------------------------------------------------------" & Ascii.Lf);
                     Append (Self.Buffer, Ascii.Lf & Ascii.Lf);

                  when ADA_ARRAY_TYPE_DEF =>
                     Process_ADA_ARRAY_TYPE_DEF_Spec (Self, As_Array_Type_Def (C), To_String (Name));

                  when ADA_INTERFACE_TYPE_DEF | ADA_ASPECT_SPEC =>
                     null;
                  when others =>
                     Append (Self.Buffer, "   --  " & C.Kind'Img & "  " &  Ascii.Lf);
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
                     Append (Self.Buffer, "with GNATColl.JSON;" & ASCII.LF & ASCII.LF);
                     Append (Self.Buffer, "package " & Self.Package_Name & " is" & ASCII.LF & ASCII.LF);
                     Append (Self.Buffer, "   use GNATColl.JSON;" & ASCII.LF & ASCII.LF);
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
   Create (Self.Outf, Out_File, "tests.codegen/" & Ada2file (To_String (Self.Package_Name)) & ".ads");
   for I of Self.Withs loop
      Insert (Self.Buffer, 1, "with " & I & ";" & ASCII.LF);
   end loop;
   Put_Line (Self.Outf, Self.Buffer);
   Close (Self.Outf);

end Write_Spec;
