with Libadalang.Common;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Source_Info;
with Ada.Strings.Fixed;
package body GNATCOLL.Json.Builder is

   use Libadalang.Common;
   use Ada.Text_IO;
   use GNAT.Source_Info;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use Ada.Text_IO.Unbounded_IO;

   procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context; Unit : Analysis_Unit) is
      Self : Analyzser (Context'Unrestricted_Access, Unit'Unrestricted_Access);
   begin
      for Node of Unit.Root.Children loop
         case Node.Kind is
            when Ada_Library_Item =>
               Self.On_Ada_Library_Item (Node);
            when others =>
               Put_Line (Enclosing_Entity & " : " & Node.Kind'Img & " : " & Node.Image);
         end case;
      end loop;
      Put_Line (String'(80 * '-'));
      Put_Line (Self.Name);
      Put_Line (String'(80 * '-'));
      Put_Line (Self.Spec_Buffer);
      Put_Line (String'(80 * '-'));
      Put_Line (Self.Body_Buffer);
      Put_Line (String'(80 * '-'));
   end Process_Unit;

   procedure On_Ada_Abort_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abort_Absent =>
                  Self.On_Ada_Abort_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abort_Absent;

   procedure On_Ada_Abort_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abort_Present =>
                  Self.On_Ada_Abort_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Stmt_List =>
                  Self.On_Ada_Stmt_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Elsif_Expr_Part_List =>
                  Self.On_Ada_Elsif_Expr_Part_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Elsif_Expr_Part_List;

   procedure On_Ada_Elsif_Stmt_Part_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Elsif_Stmt_Part_List =>
                  Self.On_Ada_Elsif_Stmt_Part_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Pragma_Node_List =>
                  Self.On_Ada_Pragma_Node_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Pragma_Node_List;

   procedure On_Ada_Select_When_Part_List (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Select_When_Part_List =>
                  Self.On_Ada_Select_When_Part_List (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_At_Clause =>
                  Self.On_Ada_At_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Rep_Clause =>
                  Self.On_Ada_Enum_Rep_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Enum_Rep_Clause;

   procedure On_Ada_Record_Rep_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Record_Rep_Clause =>
                  Self.On_Ada_Record_Rep_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Pragma_Argument_Assoc =>
                  Self.On_Ada_Pragma_Argument_Assoc (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
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
                  Append (Self.Spec_Buffer, "package " & Name & " is" & ASCII.LF);
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
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Type_Decl =>
                  Self.On_Ada_Type_Decl (N);
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
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Div =>
                  Self.On_Ada_Op_Div (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Div;

   procedure On_Ada_Op_Double_Dot (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Double_Dot =>
                  Self.On_Ada_Op_Double_Dot (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Double_Dot;

   procedure On_Ada_Op_Eq (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Eq =>
                  Self.On_Ada_Op_Eq (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Eq;

   procedure On_Ada_Op_Gt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Gt =>
                  Self.On_Ada_Op_Gt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Gt;

   procedure On_Ada_Op_Gte (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Gte =>
                  Self.On_Ada_Op_Gte (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Gte;

   procedure On_Ada_Op_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_In =>
                  Self.On_Ada_Op_In (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_In;

   procedure On_Ada_Op_Lt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Lt =>
                  Self.On_Ada_Op_Lt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Lt;

   procedure On_Ada_Op_Lte (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Lte =>
                  Self.On_Ada_Op_Lte (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Lte;

   procedure On_Ada_Op_Minus (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Minus =>
                  Self.On_Ada_Op_Minus (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Minus;

   procedure On_Ada_Op_Mod (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Mod =>
                  Self.On_Ada_Op_Mod (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Mod;

   procedure On_Ada_Op_Mult (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Mult =>
                  Self.On_Ada_Op_Mult (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Mult;

   procedure On_Ada_Op_Neq (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Neq =>
                  Self.On_Ada_Op_Neq (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Neq;

   procedure On_Ada_Op_Not (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Not =>
                  Self.On_Ada_Op_Not (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Not;

   procedure On_Ada_Op_Not_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Not_In =>
                  Self.On_Ada_Op_Not_In (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Not_In;

   procedure On_Ada_Op_Or (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Or =>
                  Self.On_Ada_Op_Or (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Or;

   procedure On_Ada_Op_Or_Else (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Or_Else =>
                  Self.On_Ada_Op_Or_Else (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Or_Else;

   procedure On_Ada_Op_Plus (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Plus =>
                  Self.On_Ada_Op_Plus (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Plus;

   procedure On_Ada_Op_Pow (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Pow =>
                  Self.On_Ada_Op_Pow (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Pow;

   procedure On_Ada_Op_Rem (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Rem =>
                  Self.On_Ada_Op_Rem (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Rem;

   procedure On_Ada_Op_Xor (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Op_Xor =>
                  Self.On_Ada_Op_Xor (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Op_Xor;

   procedure On_Ada_String_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_String_Literal =>
                  Self.On_Ada_String_Literal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_String_Literal;

   procedure On_Ada_Null_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Literal =>
                  Self.On_Ada_Null_Literal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Literal;

   procedure On_Ada_Int_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Int_Literal =>
                  Self.On_Ada_Int_Literal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Int_Literal;

   procedure On_Ada_Real_Literal (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Real_Literal =>
                  Self.On_Ada_Real_Literal (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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

      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Mode_Default =>
                  Self.On_Ada_Mode_Default (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Mode_Default;

   procedure On_Ada_Mode_In (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Mode_In =>
                  Self.On_Ada_Mode_In (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Mode_In;

   procedure On_Ada_Mode_In_Out (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Mode_In_Out =>
                  Self.On_Ada_Mode_In_Out (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Mode_In_Out;

   procedure On_Ada_Mode_Out (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Mode_Out =>
                  Self.On_Ada_Mode_Out (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Mode_Out;

   procedure On_Ada_Not_Null_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Not_Null_Absent =>
                  Self.On_Ada_Not_Null_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Not_Null_Absent;

   procedure On_Ada_Not_Null_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Not_Null_Present =>
                  Self.On_Ada_Not_Null_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Others_Designator =>
                  Self.On_Ada_Others_Designator (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Others_Designator;

   procedure On_Ada_Overriding_Not_Overriding (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Overriding_Not_Overriding =>
                  Self.On_Ada_Overriding_Not_Overriding (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Overriding_Not_Overriding;

   procedure On_Ada_Overriding_Overriding (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Overriding_Overriding =>
                  Self.On_Ada_Overriding_Overriding (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Overriding_Overriding;

   procedure On_Ada_Overriding_Unspecified (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Overriding_Unspecified =>
                  Self.On_Ada_Overriding_Unspecified (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Overriding_Unspecified;

   procedure On_Ada_Params (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Params =>
                  Self.On_Ada_Params (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Quantifier_All =>
                  Self.On_Ada_Quantifier_All (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Quantifier_All;

   procedure On_Ada_Quantifier_Some (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Quantifier_Some =>
                  Self.On_Ada_Quantifier_Some (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Quantifier_Some;

   procedure On_Ada_Range_Spec (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Range_Spec =>
                  Self.On_Ada_Range_Spec (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Range_Spec;

   procedure On_Ada_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Renaming_Clause =>
                  Self.On_Ada_Renaming_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Renaming_Clause;

   procedure On_Ada_Synthetic_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Synthetic_Renaming_Clause =>
                  Self.On_Ada_Synthetic_Renaming_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Synthetic_Renaming_Clause;

   procedure On_Ada_Reverse_Absent (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Reverse_Absent =>
                  Self.On_Ada_Reverse_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Reverse_Absent;

   procedure On_Ada_Reverse_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Reverse_Present =>
                  Self.On_Ada_Reverse_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Reverse_Present;

   procedure On_Ada_Select_When_Part (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Select_When_Part =>
                  Self.On_Ada_Select_When_Part (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Select_When_Part;

   procedure On_Ada_Accept_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Accept_Stmt =>
                  Self.On_Ada_Accept_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Accept_Stmt;

   procedure On_Ada_Accept_Stmt_With_Stmts (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Accept_Stmt_With_Stmts =>
                  Self.On_Ada_Accept_Stmt_With_Stmts (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Accept_Stmt_With_Stmts;

   procedure On_Ada_For_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_For_Loop_Stmt =>
                  Self.On_Ada_For_Loop_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_For_Loop_Stmt;

   procedure On_Ada_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Loop_Stmt =>
                  Self.On_Ada_Loop_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Loop_Stmt;

   procedure On_Ada_While_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_While_Loop_Stmt =>
                  Self.On_Ada_While_Loop_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_While_Loop_Stmt;

   procedure On_Ada_Begin_Block (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Begin_Block =>
                  Self.On_Ada_Begin_Block (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Begin_Block;

   procedure On_Ada_Decl_Block (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Decl_Block =>
                  Self.On_Ada_Decl_Block (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Decl_Block;

   procedure On_Ada_Case_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Case_Stmt =>
                  Self.On_Ada_Case_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Case_Stmt;

   procedure On_Ada_Extended_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Extended_Return_Stmt =>
                  Self.On_Ada_Extended_Return_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Extended_Return_Stmt;

   procedure On_Ada_If_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_If_Stmt =>
                  Self.On_Ada_If_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_If_Stmt;

   procedure On_Ada_Named_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Named_Stmt =>
                  Self.On_Ada_Named_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Named_Stmt;

   procedure On_Ada_Select_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Select_Stmt =>
                  Self.On_Ada_Select_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Select_Stmt;

   procedure On_Ada_Error_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Error_Stmt =>
                  Self.On_Ada_Error_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Error_Stmt;

   procedure On_Ada_Abort_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Abort_Stmt =>
                  Self.On_Ada_Abort_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Abort_Stmt;

   procedure On_Ada_Assign_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Assign_Stmt =>
                  Self.On_Ada_Assign_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Assign_Stmt;

   procedure On_Ada_Call_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Call_Stmt =>
                  Self.On_Ada_Call_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Call_Stmt;

   procedure On_Ada_Delay_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Delay_Stmt =>
                  Self.On_Ada_Delay_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Delay_Stmt;

   procedure On_Ada_Exit_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Exit_Stmt =>
                  Self.On_Ada_Exit_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Exit_Stmt;

   procedure On_Ada_Goto_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Goto_Stmt =>
                  Self.On_Ada_Goto_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Goto_Stmt;

   procedure On_Ada_Label (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Label =>
                  Self.On_Ada_Label (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Label;

   procedure On_Ada_Null_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Null_Stmt =>
                  Self.On_Ada_Null_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Null_Stmt;

   procedure On_Ada_Raise_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Raise_Stmt =>
                  Self.On_Ada_Raise_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Raise_Stmt;

   procedure On_Ada_Requeue_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Requeue_Stmt =>
                  Self.On_Ada_Requeue_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Requeue_Stmt;

   procedure On_Ada_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Return_Stmt =>
                  Self.On_Ada_Return_Stmt (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Return_Stmt;

   procedure On_Ada_Terminate_Alternative (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Terminate_Alternative =>
                  Self.On_Ada_Terminate_Alternative (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Terminate_Alternative;

   procedure On_Ada_Subp_Kind_Function (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Kind_Function =>
                  Self.On_Ada_Subp_Kind_Function (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Subp_Kind_Function;

   procedure On_Ada_Subp_Kind_Procedure (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Subp_Kind_Procedure =>
                  Self.On_Ada_Subp_Kind_Procedure (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Synchronized_Absent =>
                  Self.On_Ada_Synchronized_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Synchronized_Absent;

   procedure On_Ada_Synchronized_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Synchronized_Present =>
                  Self.On_Ada_Synchronized_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Task_Def =>
                  Self.On_Ada_Task_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Task_Def;

   procedure On_Ada_Access_To_Subp_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Access_To_Subp_Def =>
                  Self.On_Ada_Access_To_Subp_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Access_To_Subp_Def;

   procedure On_Ada_Anonymous_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Anonymous_Type_Access_Def =>
                  Self.On_Ada_Anonymous_Type_Access_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Anonymous_Type_Access_Def;

   procedure On_Ada_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Type_Access_Def =>
                  Self.On_Ada_Type_Access_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Type_Access_Def;

   procedure On_Ada_Array_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Array_Type_Def =>
                  Self.On_Ada_Array_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Array_Type_Def;

   procedure On_Ada_Derived_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Derived_Type_Def =>
                  Self.On_Ada_Derived_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Derived_Type_Def;

   procedure On_Ada_Enum_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Enum_Type_Def =>
                  Self.On_Ada_Enum_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Interface_Type_Def =>
                  Self.On_Ada_Interface_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Interface_Type_Def;

   procedure On_Ada_Mod_Int_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Mod_Int_Type_Def =>
                  Self.On_Ada_Mod_Int_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Floating_Point_Def =>
                  Self.On_Ada_Floating_Point_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
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
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Signed_Int_Type_Def =>
                  Self.On_Ada_Signed_Int_Type_Def (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Until_Absent =>
                  Self.On_Ada_Until_Absent (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Until_Absent;

   procedure On_Ada_Until_Present (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Until_Present =>
                  Self.On_Ada_Until_Present (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Until_Present;

   procedure On_Ada_Use_Package_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Use_Package_Clause =>
                  Self.On_Ada_Use_Package_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Use_Package_Clause;

   procedure On_Ada_Use_Type_Clause (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Use_Type_Clause =>
                  Self.On_Ada_Use_Type_Clause (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
   end On_Ada_Use_Type_Clause;

   procedure On_Ada_Variant (Self : in out Analyzser; Node : Ada_Node'Class) is
   begin
      Put_Line (Source_Location & ":" & Enclosing_Entity & " >> " & Node.Kind'Img & " : " & Node.Image);
      for N of Node.Children loop
         if not N.Is_Null then
            case N.Kind is
               when Ada_Variant =>
                  Self.On_Ada_Variant (N);
               when others =>
                  Put_Line (Source_Location & ":" & Enclosing_Entity & " : " & N.Kind'Img & " : " & N.Image);
            end case;
         end if;
      end loop;
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
