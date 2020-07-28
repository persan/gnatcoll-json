with Ada.Characters;
with Ada.Directories;
with GNAT.OS_Lib;
with Libadalang.Common;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Source_Info;
with Ada.Strings.Maps;
with Ada.Characters.Conversions;
with GNATCOLL.VFS;
package body GNATCOLL.Json.Builder is

   use Libadalang.Common;
   use Ada.Text_IO;
   use GNAT.Source_Info;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO.Unbounded_IO;
   Ada2file : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ.",
                                                                                          "abcdefgjijklmnopqrstuvwxyzåäö-");
   procedure App_Setup (Context : Libadalang.Helpers.App_Context; Jobs : Libadalang.Helpers.App_Job_Context_Array) is
      pragma Unreferenced (Jobs, Context);
   begin
      if Args.Version.Get then
         Put_Line (VERSION);
         GNAT.OS_Lib.OS_Exit (0);
      end if;
   end;

   procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context; Unit : Analysis_Unit) is
      Self : Analyzser (Context'Unrestricted_Access, Unit'Unrestricted_Access);
      root : GNATCOLL.VFS.Virtual_File;
      use all type Libadalang.Helpers.Source_Provider_Kind;
      use all type GNATCOLL.VFS.Filesystem_String;
      Output_folder  : Ada.Strings.Unbounded.Unbounded_String;

   begin
      if Context.App_Ctx.Provider.Kind = Libadalang.Helpers.Project_File then
         root := Context.App_Ctx.Provider.Project.Root_Project.Project_Path.Get_Parent;
      else
         root := GNATCOLL.VFS.Create (".");
      end if;

      Output_folder := To_Unbounded_String ((+root.Full_Name.all) & To_String (Args.Output_folder.Get));
      if not Ada.Directories.Exists (To_String (Output_folder)) then
         Ada.Directories.Create_Path (To_String (Output_folder));
      end if;
      Append (Output_folder, "/");

      for Node of Unit.Root.Children loop
         case Node.Kind is
            when Ada_Library_Item =>
               Self.On_Ada_Library_Item (Node);
            when Ada_Pragma_Node_List =>
               null;
            when others =>
               Put_Line (Enclosing_Entity & " : " & Node.Kind'Img & " : " & Node.Image);
         end case;
      end loop;

      Self.Create_File (To_String (Output_folder) & Translate (Self.Name, Ada2file) & ".ads");
      for I of Self.Withs loop
         Self.Put_Line ("with " & I & ";");
      end loop;
      Self.Put_Line (Self.Spec_Buffer);
      Self.Close_File;
      Self.Create_File (To_String (Output_folder) & Translate (Self.Name, Ada2file) & ".adb");

      Self.Put_Line (Self.Body_Buffer);
      Self.Close_File;

   end Process_Unit;

   procedure Create_File (Self : in out Analyzser; Name : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      pragma Debug (Put_Line (GNAT.Source_Info.Enclosing_Entity & "( " & Name & ") --------------------------"));
      Ada.Text_IO.Create (Self.Outf, Out_File, To_String (Name));
   end;
   procedure Put_Line (Self : in out Analyzser; Item : String) is
   begin
      pragma Debug (Put_Line (Item));
      Put_Line (Self.Outf, Item);
   end;

   procedure Put_Line (Self : in out Analyzser; Item : Ada.Strings.Unbounded.Unbounded_String)  is
   begin
      pragma Debug (Put_Line (Item));
      Put_Line (Self.Outf, Item);
   end;

   procedure Close_File (Self : in out Analyzser) is
   begin
      pragma Debug (Put_Line (GNAT.Source_Info.Enclosing_Entity & "--------------------------"));
      Ada.Text_IO.Close (Self.Outf);
   end;

   --  ---------------------------------------------------------------------------------------------
   --  ---------------------------------------------------------------------------------------------

   procedure On_Ada_Abort_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Abort_Absent;

   procedure On_Ada_Abort_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Abort_Present;

   procedure On_Ada_Abstract_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abstract_Absent =>
                  Self.On_Ada_Abstract_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abstract_Absent;

   procedure On_Ada_Abstract_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abstract_Present =>
                  Self.On_Ada_Abstract_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abstract_Present;

   procedure On_Ada_Ada_Node_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      pragma Debug (Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image));
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Type_Decl =>
                  Self.On_Ada_Type_Decl (N);
               when Ada_Subtype_Decl =>
                  null;
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Ada_Node_List;

   procedure On_Ada_Alternatives_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Alternatives_List =>
                  Self.On_Ada_Alternatives_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Alternatives_List;

   procedure On_Ada_Constraint_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Constraint_List =>
                  Self.On_Ada_Constraint_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Constraint_List;

   procedure On_Ada_Decl_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Decl_List =>
                  Self.On_Ada_Decl_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Decl_List;

   procedure On_Ada_Stmt_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Stmt_List;

   procedure On_Ada_Aspect_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aspect_Assoc_List =>
                  Self.On_Ada_Aspect_Assoc_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aspect_Assoc_List;

   procedure On_Ada_Base_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Base_Assoc_List =>
                  Self.On_Ada_Base_Assoc_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Base_Assoc_List;

   procedure On_Ada_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Assoc_List =>
                  Self.On_Ada_Assoc_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Assoc_List;

   procedure On_Ada_Case_Expr_Alternative_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Expr_Alternative_List =>
                  Self.On_Ada_Case_Expr_Alternative_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Expr_Alternative_List;

   procedure On_Ada_Case_Stmt_Alternative_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Stmt_Alternative_List =>
                  Self.On_Ada_Case_Stmt_Alternative_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Stmt_Alternative_List;

   procedure On_Ada_Compilation_Unit_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Compilation_Unit_List =>
                  Self.On_Ada_Compilation_Unit_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Compilation_Unit_List;

   procedure On_Ada_Contract_Case_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Contract_Case_Assoc_List =>
                  Self.On_Ada_Contract_Case_Assoc_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Contract_Case_Assoc_List;

   procedure On_Ada_Defining_Name_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Defining_Name_List =>
                  Self.On_Ada_Defining_Name_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Defining_Name_List;

   procedure On_Ada_Discriminant_Spec_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discriminant_Spec_List =>
                  Self.On_Ada_Discriminant_Spec_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discriminant_Spec_List;

   procedure On_Ada_Elsif_Expr_Part_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Elsif_Expr_Part_List;

   procedure On_Ada_Elsif_Stmt_Part_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Elsif_Stmt_Part_List;

   procedure On_Ada_Enum_Literal_Decl_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Literal_Decl_List =>
                  Self.On_Ada_Enum_Literal_Decl_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Enum_Literal_Decl_List;

   procedure On_Ada_Expr_Alternatives_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Expr_Alternatives_List =>
                  Self.On_Ada_Expr_Alternatives_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Expr_Alternatives_List;

   procedure On_Ada_Discriminant_Choice_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discriminant_Choice_List =>
                  Self.On_Ada_Discriminant_Choice_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discriminant_Choice_List;

   procedure On_Ada_Name_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Name_List =>
                  Self.On_Ada_Name_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Name_List;

   procedure On_Ada_Parent_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Parent_List =>
                  Self.On_Ada_Parent_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Parent_List;

   procedure On_Ada_Param_Spec_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Param_Spec_List =>
                  Self.On_Ada_Param_Spec_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Param_Spec_List;

   procedure On_Ada_Pragma_Node_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Pragma_Node_List;

   procedure On_Ada_Select_When_Part_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Select_When_Part_List;

   procedure On_Ada_Unconstrained_Array_Index_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Unconstrained_Array_Index_List =>
                  Self.On_Ada_Unconstrained_Array_Index_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Unconstrained_Array_Index_List;

   procedure On_Ada_Variant_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Variant_List =>
                  Self.On_Ada_Variant_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Variant_List;

   procedure On_Ada_Aliased_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aliased_Absent =>
                  Self.On_Ada_Aliased_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aliased_Absent;

   procedure On_Ada_Aliased_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aliased_Present =>
                  Self.On_Ada_Aliased_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aliased_Present;

   procedure On_Ada_All_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_All_Absent =>
                  Self.On_Ada_All_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_All_Absent;

   procedure On_Ada_All_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_All_Present =>
                  Self.On_Ada_All_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_All_Present;

   procedure On_Ada_Constrained_Array_Indices (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Constrained_Array_Indices =>
                  Self.On_Ada_Constrained_Array_Indices (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Constrained_Array_Indices;

   procedure On_Ada_Unconstrained_Array_Indices (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Unconstrained_Array_Indices =>
                  Self.On_Ada_Unconstrained_Array_Indices (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Unconstrained_Array_Indices;

   procedure On_Ada_Aspect_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aspect_Assoc =>
                  Self.On_Ada_Aspect_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aspect_Assoc;

   procedure On_Ada_At_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_At_Clause;

   procedure On_Ada_Attribute_Def_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Attribute_Def_Clause =>
                  Self.On_Ada_Attribute_Def_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Attribute_Def_Clause;

   procedure On_Ada_Enum_Rep_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Enum_Rep_Clause;

   procedure On_Ada_Record_Rep_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Record_Rep_Clause;

   procedure On_Ada_Aspect_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aspect_Spec =>
                  Self.On_Ada_Aspect_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aspect_Spec;

   procedure On_Ada_Contract_Case_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Contract_Case_Assoc =>
                  Self.On_Ada_Contract_Case_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Contract_Case_Assoc;

   procedure On_Ada_Pragma_Argument_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Pragma_Argument_Assoc;

   procedure On_Ada_Entry_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Entry_Spec =>
                  Self.On_Ada_Entry_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Entry_Spec;

   procedure On_Ada_Enum_Subp_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Subp_Spec =>
                  Self.On_Ada_Enum_Subp_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Enum_Subp_Spec;

   procedure On_Ada_Subp_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Spec =>
                  Self.On_Ada_Subp_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Spec;

   procedure On_Ada_Component_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Component_List =>
                  Self.On_Ada_Component_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Component_List;

   procedure On_Ada_Known_Discriminant_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Known_Discriminant_Part =>
                  Self.On_Ada_Known_Discriminant_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Known_Discriminant_Part;

   procedure On_Ada_Unknown_Discriminant_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Unknown_Discriminant_Part =>
                  Self.On_Ada_Unknown_Discriminant_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Unknown_Discriminant_Part;

   procedure On_Ada_Entry_Completion_Formal_Params (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Entry_Completion_Formal_Params =>
                  Self.On_Ada_Entry_Completion_Formal_Params (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Entry_Completion_Formal_Params;

   procedure On_Ada_Generic_Formal_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Formal_Part =>
                  Self.On_Ada_Generic_Formal_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Formal_Part;

   procedure On_Ada_Null_Record_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Record_Def =>
                  Self.On_Ada_Null_Record_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Record_Def;

   procedure On_Ada_Record_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Record_Def =>
                  Self.On_Ada_Record_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Record_Def;

   procedure On_Ada_Aggregate_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aggregate_Assoc =>
                  Self.On_Ada_Aggregate_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aggregate_Assoc;

   procedure On_Ada_Multi_Dim_Array_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Multi_Dim_Array_Assoc =>
                  Self.On_Ada_Multi_Dim_Array_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Multi_Dim_Array_Assoc;

   procedure On_Ada_Discriminant_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discriminant_Assoc =>
                  Self.On_Ada_Discriminant_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discriminant_Assoc;

   procedure On_Ada_Param_Assoc (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Param_Assoc =>
                  Self.On_Ada_Param_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Param_Assoc;

   procedure On_Ada_Component_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Component_Decl =>
                  Self.On_Ada_Component_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Component_Decl;

   procedure On_Ada_Discriminant_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discriminant_Spec =>
                  Self.On_Ada_Discriminant_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discriminant_Spec;

   procedure On_Ada_Generic_Formal_Obj_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Formal_Obj_Decl =>
                  Self.On_Ada_Generic_Formal_Obj_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Formal_Obj_Decl;

   procedure On_Ada_Generic_Formal_Package (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Formal_Package =>
                  Self.On_Ada_Generic_Formal_Package (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Formal_Package;

   procedure On_Ada_Generic_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Formal_Subp_Decl =>
                  Self.On_Ada_Generic_Formal_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Formal_Subp_Decl;

   procedure On_Ada_Generic_Formal_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Formal_Type_Decl =>
                  Self.On_Ada_Generic_Formal_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Formal_Type_Decl;

   procedure On_Ada_Param_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Param_Spec =>
                  Self.On_Ada_Param_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Param_Spec;

   procedure On_Ada_Generic_Package_Internal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Package_Internal =>
                  Self.On_Ada_Generic_Package_Internal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Package_Internal;

   procedure On_Ada_Package_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : Unbounded_String;
   begin
      --  Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Defining_Name =>
                  Name := To_Unbounded_String (Debug_Text (N.As_Defining_Name));
                  if Self.Name = Null_Unbounded_String then
                     Append (Name, ".JSON");
                     Self.Name := Name;
                  end if;
                  Append (Self.Body_Buffer, "package body " & Name & " is" & ASCII.LF);

                  Append (Self.Spec_Buffer, "with GNATColl.JSON;" & ASCII.LF & ASCII.LF);
                  Append (Self.Spec_Buffer, "package " & Name & " is" & ASCII.LF);
                  Append (Self.Spec_Buffer, "   use GNATColl.JSON;" & ASCII.LF & ASCII.LF);
               when Ada_Public_Part =>
                  Self.On_Ada_Public_Part (N);
               when Ada_End_Name =>
                  Append (Self.Body_Buffer, "end " & Name & ";" & ASCII.LF);
                  Append (Self.Spec_Buffer, "end " & Name & ";" & ASCII.LF);

               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Package_Decl;

   procedure On_Ada_Discrete_Base_Subtype_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discrete_Base_Subtype_Decl =>
                  Self.On_Ada_Discrete_Base_Subtype_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discrete_Base_Subtype_Decl;

   procedure On_Ada_Subtype_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subtype_Decl =>
                  Self.On_Ada_Subtype_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subtype_Decl;

   procedure On_Ada_Classwide_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Classwide_Type_Decl =>
                  Self.On_Ada_Classwide_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Classwide_Type_Decl;

   procedure On_Ada_Incomplete_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Incomplete_Type_Decl =>
                  Self.On_Ada_Incomplete_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Incomplete_Type_Decl;

   procedure On_Ada_Incomplete_Tagged_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Incomplete_Tagged_Type_Decl =>
                  Self.On_Ada_Incomplete_Tagged_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Incomplete_Tagged_Type_Decl;

   procedure On_Ada_Protected_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Type_Decl =>
                  Self.On_Ada_Protected_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Type_Decl;

   procedure On_Ada_Task_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Task_Type_Decl =>
                  Self.On_Ada_Task_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Task_Type_Decl;

   procedure On_Ada_Single_Task_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Single_Task_Type_Decl =>
                  Self.On_Ada_Single_Task_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Single_Task_Type_Decl;

   procedure On_Ada_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      --  Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Defining_Name =>
                  Self.Current.Type_Name := To_Unbounded_String (Debug_Text (N.As_Defining_Name));
               when Ada_Signed_Int_Type_Def =>
                  Self.On_Ada_Signed_Int_Type_Def (N);
               when Ada_Enum_Type_Def =>
                  Self.On_Ada_Enum_Type_Def (N);
               when Ada_Mod_Int_Type_Def =>
                  Self.On_Ada_Mod_Int_Type_Def (N);
               when Ada_Floating_Point_Def =>
                  Self.On_Ada_Floating_Point_Def (N);
               when Ada_Derived_Type_Def =>
                  Self.On_Ada_Derived_Type_Def (N);
               when Ada_Record_Type_Def =>
                  Self.On_Ada_Record_Type_Def (N);
               when Ada_Array_Type_Def =>
                  Self.On_Ada_Array_Type_Def (N);

               when Ada_Interface_Type_Def | Ada_Aspect_Spec =>
                  null;

               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Type_Decl;

   procedure On_Ada_Anonymous_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Anonymous_Type_Decl =>
                  Self.On_Ada_Anonymous_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Anonymous_Type_Decl;

   procedure On_Ada_Synth_Anonymous_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Synth_Anonymous_Type_Decl =>
                  Self.On_Ada_Synth_Anonymous_Type_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Synth_Anonymous_Type_Decl;

   procedure On_Ada_Abstract_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abstract_Subp_Decl =>
                  Self.On_Ada_Abstract_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abstract_Subp_Decl;

   procedure On_Ada_Abstract_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abstract_Formal_Subp_Decl =>
                  Self.On_Ada_Abstract_Formal_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abstract_Formal_Subp_Decl;

   procedure On_Ada_Concrete_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Concrete_Formal_Subp_Decl =>
                  Self.On_Ada_Concrete_Formal_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Concrete_Formal_Subp_Decl;

   procedure On_Ada_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Decl =>
                  Self.On_Ada_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Decl;

   procedure On_Ada_Entry_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Entry_Decl =>
                  Self.On_Ada_Entry_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Entry_Decl;

   procedure On_Ada_Enum_Literal_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Literal_Decl =>
                  Self.On_Ada_Enum_Literal_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Enum_Literal_Decl;

   procedure On_Ada_Generic_Subp_Internal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Subp_Internal =>
                  Self.On_Ada_Generic_Subp_Internal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Subp_Internal;

   procedure On_Ada_Expr_Function (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Expr_Function =>
                  Self.On_Ada_Expr_Function (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Expr_Function;

   procedure On_Ada_Null_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Subp_Decl =>
                  Self.On_Ada_Null_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Subp_Decl;

   procedure On_Ada_Subp_Body (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Body =>
                  Self.On_Ada_Subp_Body (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Body;

   procedure On_Ada_Subp_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Renaming_Decl =>
                  Self.On_Ada_Subp_Renaming_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Renaming_Decl;

   procedure On_Ada_Package_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Package_Body_Stub =>
                  Self.On_Ada_Package_Body_Stub (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Package_Body_Stub;

   procedure On_Ada_Protected_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Body_Stub =>
                  Self.On_Ada_Protected_Body_Stub (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Body_Stub;

   procedure On_Ada_Subp_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Body_Stub =>
                  Self.On_Ada_Subp_Body_Stub (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Body_Stub;

   procedure On_Ada_Task_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Task_Body_Stub =>
                  Self.On_Ada_Task_Body_Stub (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Task_Body_Stub;

   procedure On_Ada_Entry_Body (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Entry_Body =>
                  Self.On_Ada_Entry_Body (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Entry_Body;

   procedure On_Ada_Package_Body (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Package_Body =>
                  Self.On_Ada_Package_Body (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Package_Body;

   procedure On_Ada_Protected_Body (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Body =>
                  Self.On_Ada_Protected_Body (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Body;

   procedure On_Ada_Task_Body (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Task_Body =>
                  Self.On_Ada_Task_Body (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Task_Body;

   procedure On_Ada_Entry_Index_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Entry_Index_Spec =>
                  Self.On_Ada_Entry_Index_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Entry_Index_Spec;

   procedure On_Ada_Error_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Error_Decl =>
                  Self.On_Ada_Error_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Error_Decl;

   procedure On_Ada_Exception_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Exception_Decl =>
                  Self.On_Ada_Exception_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Exception_Decl;

   procedure On_Ada_Exception_Handler (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Exception_Handler =>
                  Self.On_Ada_Exception_Handler (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Exception_Handler;

   procedure On_Ada_For_Loop_Var_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_For_Loop_Var_Decl =>
                  Self.On_Ada_For_Loop_Var_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_For_Loop_Var_Decl;

   procedure On_Ada_Generic_Package_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Package_Decl =>
                  Self.On_Ada_Generic_Package_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Package_Decl;

   procedure On_Ada_Generic_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Subp_Decl =>
                  Self.On_Ada_Generic_Subp_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Subp_Decl;

   procedure On_Ada_Generic_Package_Instantiation (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Package_Instantiation =>
                  Self.On_Ada_Generic_Package_Instantiation (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Package_Instantiation;

   procedure On_Ada_Generic_Subp_Instantiation (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Subp_Instantiation =>
                  Self.On_Ada_Generic_Subp_Instantiation (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Subp_Instantiation;

   procedure On_Ada_Generic_Package_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Package_Renaming_Decl =>
                  Self.On_Ada_Generic_Package_Renaming_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Package_Renaming_Decl;

   procedure On_Ada_Generic_Subp_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Generic_Subp_Renaming_Decl =>
                  Self.On_Ada_Generic_Subp_Renaming_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Generic_Subp_Renaming_Decl;

   procedure On_Ada_Label_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Label_Decl =>
                  Self.On_Ada_Label_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Label_Decl;

   procedure On_Ada_Named_Stmt_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Named_Stmt_Decl =>
                  Self.On_Ada_Named_Stmt_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Named_Stmt_Decl;

   procedure On_Ada_Number_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Number_Decl =>
                  Self.On_Ada_Number_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Number_Decl;

   procedure On_Ada_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Object_Decl =>
                  Self.On_Ada_Object_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Object_Decl;

   procedure On_Ada_Anonymous_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Anonymous_Object_Decl =>
                  Self.On_Ada_Anonymous_Object_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Anonymous_Object_Decl;

   procedure On_Ada_Extended_Return_Stmt_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Extended_Return_Stmt_Object_Decl =>
                  Self.On_Ada_Extended_Return_Stmt_Object_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Extended_Return_Stmt_Object_Decl;

   procedure On_Ada_Package_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Package_Renaming_Decl =>
                  Self.On_Ada_Package_Renaming_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Package_Renaming_Decl;

   procedure On_Ada_Single_Protected_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Single_Protected_Decl =>
                  Self.On_Ada_Single_Protected_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Single_Protected_Decl;

   procedure On_Ada_Single_Task_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Single_Task_Decl =>
                  Self.On_Ada_Single_Task_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Single_Task_Decl;

   procedure On_Ada_Case_Stmt_Alternative (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Stmt_Alternative =>
                  Self.On_Ada_Case_Stmt_Alternative (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Stmt_Alternative;

   procedure On_Ada_Compilation_Unit (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Compilation_Unit =>
                  Self.On_Ada_Compilation_Unit (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Compilation_Unit;

   procedure On_Ada_Component_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Component_Clause =>
                  Self.On_Ada_Component_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Component_Clause;

   procedure On_Ada_Component_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Component_Def =>
                  Self.On_Ada_Component_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Component_Def;

   procedure On_Ada_Constant_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Constant_Absent =>
                  Self.On_Ada_Constant_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Constant_Absent;

   procedure On_Ada_Constant_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Constant_Present =>
                  Self.On_Ada_Constant_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Constant_Present;

   procedure On_Ada_Delta_Constraint (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Delta_Constraint =>
                  Self.On_Ada_Delta_Constraint (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Delta_Constraint;

   procedure On_Ada_Digits_Constraint (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Digits_Constraint =>
                  Self.On_Ada_Digits_Constraint (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Digits_Constraint;

   procedure On_Ada_Discriminant_Constraint (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discriminant_Constraint =>
                  Self.On_Ada_Discriminant_Constraint (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discriminant_Constraint;

   procedure On_Ada_Index_Constraint (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Index_Constraint =>
                  Self.On_Ada_Index_Constraint (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Index_Constraint;

   procedure On_Ada_Range_Constraint (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Range_Constraint =>
                  Self.On_Ada_Range_Constraint (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Range_Constraint;

   procedure On_Ada_Declarative_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Declarative_Part =>
                  Self.On_Ada_Declarative_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Declarative_Part;

   procedure On_Ada_Private_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Private_Part =>
                  Self.On_Ada_Private_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Private_Part;

   procedure On_Ada_Public_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      --  Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Ada_Node_List  =>
                  Self.On_Ada_Ada_Node_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Public_Part;

   procedure On_Ada_Elsif_Expr_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Elsif_Expr_Part =>
                  Self.On_Ada_Elsif_Expr_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Elsif_Expr_Part;

   procedure On_Ada_Elsif_Stmt_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Elsif_Stmt_Part =>
                  Self.On_Ada_Elsif_Stmt_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Elsif_Stmt_Part;

   procedure On_Ada_Allocator (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Allocator =>
                  Self.On_Ada_Allocator (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Allocator;

   procedure On_Ada_Aggregate (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Aggregate =>
                  Self.On_Ada_Aggregate (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Aggregate;

   procedure On_Ada_Null_Record_Aggregate (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Record_Aggregate =>
                  Self.On_Ada_Null_Record_Aggregate (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Record_Aggregate;

   procedure On_Ada_Bin_Op (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Bin_Op =>
                  Self.On_Ada_Bin_Op (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Bin_Op;

   procedure On_Ada_Relation_Op (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Relation_Op =>
                  Self.On_Ada_Relation_Op (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Relation_Op;

   procedure On_Ada_Box_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Box_Expr =>
                  Self.On_Ada_Box_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Box_Expr;

   procedure On_Ada_Case_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Expr =>
                  Self.On_Ada_Case_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Expr;

   procedure On_Ada_Case_Expr_Alternative (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Expr_Alternative =>
                  Self.On_Ada_Case_Expr_Alternative (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Expr_Alternative;

   procedure On_Ada_Contract_Cases (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Contract_Cases =>
                  Self.On_Ada_Contract_Cases (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Contract_Cases;

   procedure On_Ada_If_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_If_Expr =>
                  Self.On_Ada_If_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_If_Expr;

   procedure On_Ada_Membership_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Membership_Expr =>
                  Self.On_Ada_Membership_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Membership_Expr;

   procedure On_Ada_Attribute_Ref (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Attribute_Ref =>
                  Self.On_Ada_Attribute_Ref (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Attribute_Ref;

   procedure On_Ada_Update_Attribute_Ref (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Update_Attribute_Ref =>
                  Self.On_Ada_Update_Attribute_Ref (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Update_Attribute_Ref;

   procedure On_Ada_Call_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Call_Expr =>
                  Self.On_Ada_Call_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Call_Expr;

   procedure On_Ada_Defining_Name (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Defining_Name =>
                  Self.On_Ada_Defining_Name (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Defining_Name;

   procedure On_Ada_Discrete_Subtype_Name (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discrete_Subtype_Name =>
                  Self.On_Ada_Discrete_Subtype_Name (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discrete_Subtype_Name;

   procedure On_Ada_Dotted_Name (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Dotted_Name =>
                  Self.On_Ada_Dotted_Name (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Dotted_Name;

   procedure On_Ada_End_Name (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_End_Name =>
                  Self.On_Ada_End_Name (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_End_Name;

   procedure On_Ada_Explicit_Deref (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Explicit_Deref =>
                  Self.On_Ada_Explicit_Deref (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Explicit_Deref;

   procedure On_Ada_Qual_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Qual_Expr =>
                  Self.On_Ada_Qual_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Qual_Expr;

   procedure On_Ada_Char_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Char_Literal =>
                  Self.On_Ada_Char_Literal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Char_Literal;

   procedure On_Ada_Identifier (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Identifier =>
                  Self.On_Ada_Identifier (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Identifier;

   procedure On_Ada_Op_Abs (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Abs =>
                  Self.On_Ada_Op_Abs (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Abs;

   procedure On_Ada_Op_And (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_And =>
                  Self.On_Ada_Op_And (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_And;

   procedure On_Ada_Op_And_Then (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_And_Then =>
                  Self.On_Ada_Op_And_Then (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_And_Then;

   procedure On_Ada_Op_Concat (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Concat =>
                  Self.On_Ada_Op_Concat (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Concat;

   procedure On_Ada_Op_Div (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Div;

   procedure On_Ada_Op_Double_Dot (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Double_Dot;

   procedure On_Ada_Op_Eq (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Eq;

   procedure On_Ada_Op_Gt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Gt;

   procedure On_Ada_Op_Gte (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Gte;

   procedure On_Ada_Op_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_In;

   procedure On_Ada_Op_Lt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Lt;

   procedure On_Ada_Op_Lte (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Lte;

   procedure On_Ada_Op_Minus (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Minus;

   procedure On_Ada_Op_Mod (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Mod;

   procedure On_Ada_Op_Mult (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Mult;

   procedure On_Ada_Op_Neq (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Neq;

   procedure On_Ada_Op_Not (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Not;

   procedure On_Ada_Op_Not_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Not_In;

   procedure On_Ada_Op_Or (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Or;

   procedure On_Ada_Op_Or_Else (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Or_Else;

   procedure On_Ada_Op_Plus (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Plus;

   procedure On_Ada_Op_Pow (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Pow;

   procedure On_Ada_Op_Rem (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Rem;

   procedure On_Ada_Op_Xor (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Op_Xor;

   procedure On_Ada_String_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_String_Literal;

   procedure On_Ada_Null_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Null_Literal;

   procedure On_Ada_Int_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Int_Literal;

   procedure On_Ada_Real_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Real_Literal;

   procedure On_Ada_Target_Name (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Target_Name =>
                  Self.On_Ada_Target_Name (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Target_Name;

   procedure On_Ada_Paren_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Paren_Expr =>
                  Self.On_Ada_Paren_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Paren_Expr;

   procedure On_Ada_Quantified_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Quantified_Expr =>
                  Self.On_Ada_Quantified_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Quantified_Expr;

   procedure On_Ada_Raise_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Raise_Expr =>
                  Self.On_Ada_Raise_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Raise_Expr;

   procedure On_Ada_Un_Op (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Un_Op =>
                  Self.On_Ada_Un_Op (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Un_Op;

   procedure On_Ada_Handled_Stmts (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Handled_Stmts =>
                  Self.On_Ada_Handled_Stmts (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Handled_Stmts;

   procedure On_Ada_Interface_Kind_Limited (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Interface_Kind_Limited =>
                  Self.On_Ada_Interface_Kind_Limited (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Interface_Kind_Limited;

   procedure On_Ada_Interface_Kind_Protected (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Interface_Kind_Protected =>
                  Self.On_Ada_Interface_Kind_Protected (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Interface_Kind_Protected;

   procedure On_Ada_Interface_Kind_Synchronized (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Interface_Kind_Synchronized =>
                  Self.On_Ada_Interface_Kind_Synchronized (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Interface_Kind_Synchronized;

   procedure On_Ada_Interface_Kind_Task (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Interface_Kind_Task =>
                  Self.On_Ada_Interface_Kind_Task (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Interface_Kind_Task;

   procedure On_Ada_Iter_Type_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Iter_Type_In =>
                  Self.On_Ada_Iter_Type_In (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Iter_Type_In;

   procedure On_Ada_Iter_Type_Of (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Iter_Type_Of =>
                  Self.On_Ada_Iter_Type_Of (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Iter_Type_Of;

   procedure On_Ada_Library_Item (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin

      --  Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Package_Decl =>
                  Self.Spec_Buffer := Null_Unbounded_String;
                  Self.Body_Buffer := Null_Unbounded_String;
                  Self.Name        := Null_Unbounded_String;
                  Self.On_Ada_Package_Decl (N);
               when Ada_Private_Absent =>
                  null;
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Library_Item;

   procedure On_Ada_Limited_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Limited_Absent =>
                  Self.On_Ada_Limited_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Limited_Absent;

   procedure On_Ada_Limited_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Limited_Present =>
                  Self.On_Ada_Limited_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Limited_Present;

   procedure On_Ada_For_Loop_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_For_Loop_Spec =>
                  Self.On_Ada_For_Loop_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_For_Loop_Spec;

   procedure On_Ada_While_Loop_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_While_Loop_Spec =>
                  Self.On_Ada_While_Loop_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_While_Loop_Spec;

   procedure On_Ada_Mode_Default (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Mode_Default;

   procedure On_Ada_Mode_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Mode_In;

   procedure On_Ada_Mode_In_Out (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Mode_In_Out;

   procedure On_Ada_Mode_Out (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Mode_Out;

   procedure On_Ada_Not_Null_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Not_Null_Absent;

   procedure On_Ada_Not_Null_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Not_Null_Present;

   procedure On_Ada_Null_Component_Decl (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Component_Decl =>
                  Self.On_Ada_Null_Component_Decl (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Component_Decl;

   procedure On_Ada_Others_Designator (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Others_Designator;

   procedure On_Ada_Overriding_Not_Overriding (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Overriding_Not_Overriding;

   procedure On_Ada_Overriding_Overriding (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Overriding_Overriding;

   procedure On_Ada_Overriding_Unspecified (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Overriding_Unspecified;

   procedure On_Ada_Params (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Params;

   procedure On_Ada_Pragma_Node (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Pragma_Node =>
                  Self.On_Ada_Pragma_Node (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Pragma_Node;

   procedure On_Ada_Prim_Type_Accessor (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Prim_Type_Accessor =>
                  Self.On_Ada_Prim_Type_Accessor (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Prim_Type_Accessor;

   procedure On_Ada_Private_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Private_Absent =>
                  Self.On_Ada_Private_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Private_Absent;

   procedure On_Ada_Private_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Private_Present =>
                  Self.On_Ada_Private_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Private_Present;

   procedure On_Ada_Protected_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Def =>
                  Self.On_Ada_Protected_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Def;

   procedure On_Ada_Protected_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Absent =>
                  Self.On_Ada_Protected_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Absent;

   procedure On_Ada_Protected_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Protected_Present =>
                  Self.On_Ada_Protected_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Protected_Present;

   procedure On_Ada_Quantifier_All (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Quantifier_All;

   procedure On_Ada_Quantifier_Some (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Quantifier_Some;

   procedure On_Ada_Range_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Range_Spec;

   procedure On_Ada_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Renaming_Clause;

   procedure On_Ada_Synthetic_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Synthetic_Renaming_Clause;

   procedure On_Ada_Reverse_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Reverse_Absent;

   procedure On_Ada_Reverse_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Reverse_Present;

   procedure On_Ada_Select_When_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Select_When_Part;

   procedure On_Ada_Accept_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Accept_Stmt;

   procedure On_Ada_Accept_Stmt_With_Stmts (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Accept_Stmt_With_Stmts;

   procedure On_Ada_For_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_For_Loop_Stmt;

   procedure On_Ada_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Loop_Stmt;

   procedure On_Ada_While_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_While_Loop_Stmt;

   procedure On_Ada_Begin_Block (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Begin_Block;

   procedure On_Ada_Decl_Block (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Decl_Block;

   procedure On_Ada_Case_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Case_Stmt;

   procedure On_Ada_Extended_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Extended_Return_Stmt;

   procedure On_Ada_If_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_If_Stmt;

   procedure On_Ada_Named_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Named_Stmt;

   procedure On_Ada_Select_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Select_Stmt;

   procedure On_Ada_Error_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Error_Stmt;

   procedure On_Ada_Abort_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Abort_Stmt;

   procedure On_Ada_Assign_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Assign_Stmt;

   procedure On_Ada_Call_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Call_Stmt;

   procedure On_Ada_Delay_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Delay_Stmt;

   procedure On_Ada_Exit_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Exit_Stmt;

   procedure On_Ada_Goto_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Goto_Stmt;

   procedure On_Ada_Label (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Label;

   procedure On_Ada_Null_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Null_Stmt;

   procedure On_Ada_Raise_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Raise_Stmt;

   procedure On_Ada_Requeue_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Requeue_Stmt;

   procedure On_Ada_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Return_Stmt;

   procedure On_Ada_Terminate_Alternative (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Terminate_Alternative;

   procedure On_Ada_Subp_Kind_Function (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Subp_Kind_Function;

   procedure On_Ada_Subp_Kind_Procedure (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Subp_Kind_Procedure;

   procedure On_Ada_Subunit (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subunit =>
                  Self.On_Ada_Subunit (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subunit;

   procedure On_Ada_Synchronized_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Synchronized_Absent;

   procedure On_Ada_Synchronized_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Synchronized_Present;

   procedure On_Ada_Tagged_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Tagged_Absent =>
                  Self.On_Ada_Tagged_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Tagged_Absent;

   procedure On_Ada_Tagged_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Tagged_Present =>
                  Self.On_Ada_Tagged_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Tagged_Present;

   procedure On_Ada_Task_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Task_Def;

   procedure On_Ada_Access_To_Subp_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Access_To_Subp_Def;

   procedure On_Ada_Anonymous_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Anonymous_Type_Access_Def;

   procedure On_Ada_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Type_Access_Def;

   procedure On_Ada_Array_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name         : constant String := To_String (Self.Current.Type_Name);
      Index_Name   : Unbounded_String;
      Element_Name : Unbounded_String;

   begin

      Append (Self.Spec_Buffer, "   --  ------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  ------------------------------------" & ASCII.LF);
      for C of Node.Children loop
         if not C.Is_Null then
            case C.Kind is
               when Ada_Unconstrained_Array_Indices =>
                  for CC of C.Children loop
                     if not CC.Is_Null then
                        case CC.Kind is
                           when Ada_Unconstrained_Array_Index_List =>
                              for CCc of CC.Children loop
                                 if not CCc.Is_Null then
                                    case CCc.Kind is
                                       when Ada_Unconstrained_Array_Index =>
                                          for CCcC of CCc.Children loop
                                             if not CCcC.Is_Null then
                                                case CCcC.Kind is
                                                   when Ada_Subtype_Indication =>
                                                      for Ccccc of CCcC.Children loop
                                                         if not Ccccc.Is_Null then
                                                            case Ccccc.Kind is
                                                            when Ada_Not_Null_Absent | Ada_Aliased_Absent | Ada_Constant_Absent =>
                                                               null;
                                                               when Ada_Identifier =>
                                                                  Index_Name := To_Unbounded_String (Ada.Characters.Conversions.To_String (As_Identifier (Ccccc).Text));
                                                               when others =>
                                                                  Append (Self.Spec_Buffer, "   --5      " & Ccccc.Kind'Img & ASCII.LF);
                                                            end case;
                                                         end if;
                                                      end loop;
                                                   when others =>
                                                      Append (Self.Spec_Buffer, "   --4      " & CCcC.Kind'Img & ASCII.LF);
                                                end case;
                                             end if;
                                          end loop;
                                       when others =>
                                          Append (Self.Spec_Buffer, "   --3      " & CCc.Kind'Img & ASCII.LF);
                                    end case;
                                 end if;
                              end loop;
                           when others =>
                              Append (Self.Spec_Buffer, "   --2     " & CC.Kind'Img & ASCII.LF);
                        end case;
                     end if;
                  end loop;
               when Ada_Constrained_Array_Indices =>
                  null;
               when Ada_Component_Def =>

                  for C_1 of C.Children loop
                     if not C_1.Is_Null then
                        case C_1.Kind is
                           when Ada_Subtype_Indication =>
                              for C_2 of C_1.Children loop
                                 if not C_2.Is_Null then
                                    case C_2.Kind is
                                       when Ada_Not_Null_Absent | Ada_Aliased_Absent | Ada_Constant_Absent =>
                                          null;
                                       when Ada_Identifier =>
                                          Element_Name := To_Unbounded_String (Ada.Characters.Conversions.To_String (As_Identifier (C_2).Text));
                                       when others =>
                                          Append (Self.Spec_Buffer, "   -- " & GNAT.Source_Info.Source_Location & "      " & C_2.Kind'Img & ASCII.LF);
                                    end case;
                                 end if;
                              end loop;

                           when Ada_Not_Null_Absent | Ada_Aliased_Absent | Ada_Constant_Absent =>
                              null;
                           when others =>
                              Append (Self.Spec_Buffer, "   -- " & GNAT.Source_Info.Source_Location & "      " & C_1.Kind'Img & ASCII.LF);
                              Print (C_1);

                        end case;
                     end if;
                  end loop;
               when others =>
                  Append (Self.Spec_Buffer, "   --1  " & C.Kind'Img & ASCII.LF);
            end case;
         end if;
      end loop;
      Append (Self.Spec_Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Arrays_Generic" & ASCII.LF &
                "      (" & Index_Name & "," & Element_Name & "," &  Name & ", Create, Get, Create, Get); " & ASCII.LF);

      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & ASCII.LF & ASCII.LF & ASCII.LF);
      Self.Withs.Include ("GNATCOLL.JSON.Support.Arrays_Generic");

      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);
   end On_Ada_Array_Type_Def;

   procedure On_Ada_Derived_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name                : constant String := To_String (Self.Current.Type_Name);
      ABSTRACT_ABSENT     : Boolean := False with Warnings => Off;
      LIMITED_ABSENT      : Boolean := False with Warnings => Off;
      SYNCHRONIZED_ABSENT : Boolean := False with Warnings => Off;
      SUBTYPE_INDICATION  : Boolean := False with Warnings => Off;
      PARENT_LIST         : Boolean := False with Warnings => Off;
      WITH_PRIVATE_ABSENT : Boolean := False with Warnings => Off;
      Is_Record           : Boolean := False with Warnings => Off;
      Done                : Boolean := False with Warnings => Off;

   begin
      for CC of Node.Children loop
         if not CC.Is_Null then
            case CC.Kind is
               when Ada_Limited_Absent =>
                  LIMITED_ABSENT := True;
               when Ada_Synchronized_Absent =>
                  SYNCHRONIZED_ABSENT := True;
               when Ada_Subtype_Indication =>
                  null;
               when Ada_Parent_List =>
                  null;
               when Ada_With_Private_Absent =>
                  WITH_PRIVATE_ABSENT := True;
               when Ada_Record_Def =>
                  Is_Record := True;
               when others =>
                  Append (Self.Body_Buffer, "   --  " & CC.Kind'Img & ASCII.LF);
            end case;
         end if;
      end loop;
      --  --------------------------------------------------------------------------------------
      --  Specefication
      --  --------------------------------------------------------------------------------------
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & ";" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & ";" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ");" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);

      Append (Self.Body_Buffer, "   --  --------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Body_Buffer, "   --  ADA_DERIVED_TYPE_DEF: " & Name & " " & ASCII.LF);
      Append (Self.Body_Buffer, "   --  --------------------------------------------------------------------" & ASCII.LF);

      --  --------------------------------------------------------------------------------------
      --  Implementation
      --  --------------------------------------------------------------------------------------

      if not Done then
         --  for Field of Fields loop
         --    Append (Self.Body_Buffer,
         --            Name & "_" & Field & "_Name : constant aliased String := "" & Field &"";" & ASCII.LF);
         --  end loop;

         Append (Self.Body_Buffer, "   function Create (Val : " & Name & ") return JSON_Value is" & ASCII.LF);
         Append (Self.Body_Buffer, "   begin" & ASCII.LF);
         Append (Self.Body_Buffer, "      return ret : JSON_Value := Create_Object do" & ASCII.LF);
         Append (Self.Body_Buffer, "         null;" & ASCII.LF);
         --  for Field of Fields loop
         --    Append (Self.Body_Buffer,"      Set_Field (ret, Field, Create (Val." & Field & "));");
         --  end loop;
         Append (Self.Body_Buffer, "      end return;" & ASCII.LF);
         Append (Self.Body_Buffer, "   end Create;" & ASCII.LF & ASCII.LF);

         Append (Self.Body_Buffer, "   function Get (Val : JSON_Value) return " & Name & " is" & ASCII.LF &
                   "   begin" & ASCII.LF &
                   "      return ret : " & Name & " do" & ASCII.LF &
                   "         null;" & ASCII.LF &
                   "      end return;" & ASCII.LF &
                   "   end Get;" & ASCII.LF & ASCII.LF);

         Append (Self.Body_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " is" & ASCII.LF &
                   "   begin" & ASCII.LF &
                   "       return " & Name & "'(Get (JSON_Value'(Get (Val, Field))));" & ASCII.LF &
                   "   end Get;" & ASCII.LF & ASCII.LF);

         Append (Self.Body_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") is" & ASCII.LF &
                   "   begin" & ASCII.LF &
                   "      Set_Field (Val, Field_Name, Create (Field));" & ASCII.LF &
                   "   end Set_Field;" &  ASCII.LF & ASCII.LF & ASCII.LF);
      end if;

   end On_Ada_Derived_Type_Def;

   procedure On_Ada_Enum_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : constant String := To_String (Self.Current.Type_Name);
   begin
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      Append (Self.Spec_Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Enumeration_Generic (" & Name & ");" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);

      Self.Withs.Include ("GNATCOLL.JSON.Support.Enumeration_Generic");
   end On_Ada_Enum_Type_Def;

   procedure On_Ada_Formal_Discrete_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Formal_Discrete_Type_Def =>
                  Self.On_Ada_Formal_Discrete_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Formal_Discrete_Type_Def;

   procedure On_Ada_Interface_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Interface_Type_Def;

   procedure On_Ada_Mod_Int_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : constant String := To_String (Self.Current.Type_Name);
   begin
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      Append (Self.Spec_Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Modular_Generic (" & Name & ");" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);
      Self.Withs.Include ("GNATCOLL.JSON.Support.Modular_Generic");
   end On_Ada_Mod_Int_Type_Def;

   procedure On_Ada_Private_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Private_Type_Def =>
                  Self.On_Ada_Private_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Private_Type_Def;

   procedure On_Ada_Decimal_Fixed_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Decimal_Fixed_Point_Def =>
                  Self.On_Ada_Decimal_Fixed_Point_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Decimal_Fixed_Point_Def;

   procedure On_Ada_Floating_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : constant String := To_String (Self.Current.Type_Name);
   begin
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      Append (Self.Spec_Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Float_Generic (" & Name & ");" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);
      Self.Withs.Include ("GNATCOLL.JSON.Support.Float_Generic");
   end On_Ada_Floating_Point_Def;

   procedure On_Ada_Ordinary_Fixed_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Ordinary_Fixed_Point_Def =>
                  Self.On_Ada_Ordinary_Fixed_Point_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Ordinary_Fixed_Point_Def;

   procedure On_Ada_Record_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : constant String := To_String (Self.Current.Type_Name);
      ABSTRACT_ABSENT     : Boolean := False with Warnings => Off;
      LIMITED_ABSENT      : Boolean := False with Warnings => Off;
      SYNCHRONIZED_ABSENT : Boolean := False with Warnings => Off;
      SUBTYPE_INDICATION  : Boolean := False with Warnings => Off;
      PARENT_LIST         : Boolean := False with Warnings => Off;
      WITH_PRIVATE_ABSENT : Boolean := False with Warnings => Off;
      Is_Record           : Boolean := False with Warnings => Off;

   begin
      for CC of Node.Children loop
         if not CC.Is_Null then
            case CC.Kind is
               when Ada_Abstract_Absent =>
                  ABSTRACT_ABSENT := True;
               when Ada_Limited_Absent =>
                  LIMITED_ABSENT := True;
               when Ada_Synchronized_Absent =>
                  SYNCHRONIZED_ABSENT := True;
               when Ada_Subtype_Indication =>
                  null;
               when Ada_Parent_List =>
                  null;
               when Ada_With_Private_Absent =>
                  WITH_PRIVATE_ABSENT := True;
               when Ada_Record_Def =>
                  Is_Record := True;
               when others =>
                  Append (Self.Body_Buffer, "   --  " & CC.Kind'Img & ASCII.LF);
            end case;
         end if;
      end loop;

      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      if ABSTRACT_ABSENT then
         Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value;" & ASCII.LF);
         Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & ";" & ASCII.LF);
         Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & ";" & ASCII.LF);
         Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ");" & ASCII.LF);
      end if;
      Append (Self.Spec_Buffer, "   procedure Map_JSON_Value (User_Object : in out " & Name & ";" & ASCII.LF);
      Append (Self.Spec_Buffer, "                             Name        : UTF8_String;" & ASCII.LF);
      Append (Self.Spec_Buffer, "                             Value       : JSON_Value);" & ASCII.LF);
      if ABSTRACT_ABSENT then
         Append (Self.Spec_Buffer, "   procedure Map_JSON_Object is new Gen_Map_JSON_Object (" & Name & ");" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --  Map_JSON_Object(Val         : JSON_Value;" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --                  CB          : access procedure (User_Object : in out " & Name & ";" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --                                                  Name        : UTF8_String;" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --                                                  Value       : JSON_Value);" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --                   User_Object : in out Abstract_Record)" & ASCII.LF);
         Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      end if;
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);

      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      Print (Node, Show_Slocs => True, Line_Prefix => "   >");

      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Record_Type_Def =>
                  Self.On_Ada_Record_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Record_Type_Def;

   procedure On_Ada_Signed_Int_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
      Name : constant String := To_String (Self.Current.Type_Name);
   begin
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  " & Name & " " & ASCII.LF);
      Append (Self.Spec_Buffer, "   --" & ASCII.LF);
      Append (Self.Spec_Buffer, "   package " & Name & "_JSON_Impl is new GNATCOLL.JSON.Support.Integer_Generic (" & Name & ");" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Create (Val : " & Name & ") return JSON_Value renames " & Name & "_JSON_Impl.Create;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & " renames " & Name & "_JSON_Impl.Get;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ") renames " & Name & "_JSON_Impl.Set_Field;" & ASCII.LF);
      Append (Self.Spec_Buffer, "   --  -------------------------------------------------------------------------" & ASCII.LF);
      Append (Self.Spec_Buffer, ASCII.LF & ASCII.LF);
      Self.Withs.Include ("GNATCOLL.JSON.Support.Integer_Generic");
   end On_Ada_Signed_Int_Type_Def;

   procedure On_Ada_Anonymous_Type (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Anonymous_Type =>
                  Self.On_Ada_Anonymous_Type (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Anonymous_Type;

   procedure On_Ada_Enum_Lit_Synth_Type_Expr (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Lit_Synth_Type_Expr =>
                  Self.On_Ada_Enum_Lit_Synth_Type_Expr (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Enum_Lit_Synth_Type_Expr;

   procedure On_Ada_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subtype_Indication =>
                  Self.On_Ada_Subtype_Indication (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subtype_Indication;

   procedure On_Ada_Constrained_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Constrained_Subtype_Indication =>
                  Self.On_Ada_Constrained_Subtype_Indication (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Constrained_Subtype_Indication;

   procedure On_Ada_Discrete_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Discrete_Subtype_Indication =>
                  Self.On_Ada_Discrete_Subtype_Indication (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Discrete_Subtype_Indication;

   procedure On_Ada_Unconstrained_Array_Index (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Unconstrained_Array_Index =>
                  Self.On_Ada_Unconstrained_Array_Index (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Unconstrained_Array_Index;

   procedure On_Ada_Until_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Until_Absent;

   procedure On_Ada_Until_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Until_Present;

   procedure On_Ada_Use_Package_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Use_Package_Clause;

   procedure On_Ada_Use_Type_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Use_Type_Clause;

   procedure On_Ada_Variant (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
   end On_Ada_Variant;

   procedure On_Ada_Variant_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Variant_Part =>
                  Self.On_Ada_Variant_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Variant_Part;

   procedure On_Ada_With_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_With_Clause =>
                  Self.On_Ada_With_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_With_Clause;

   procedure On_Ada_With_Private_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_With_Private_Absent =>
                  Self.On_Ada_With_Private_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_With_Private_Absent;

   procedure On_Ada_With_Private_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_With_Private_Present =>
                  Self.On_Ada_With_Private_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_With_Private_Present;

end GNATCOLL.JSON.Builder;
