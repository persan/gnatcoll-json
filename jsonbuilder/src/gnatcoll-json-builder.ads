with Ada.Text_IO;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Sets;
with GNATCOLL.Opt_Parse;
package GNATCOLL.Json.Builder is
   VERSION : constant String := "0.0.1";

   procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context; Unit : Analysis_Unit);
   procedure App_Setup (Context : Libadalang.Helpers.App_Context; Jobs : Libadalang.Helpers.App_Job_Context_Array);

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   package Application is new Libadalang.Helpers.App
     (Name           => "json-builder",
      Description    => "Generated JSON bindings from Ada-specs",
      App_Setup      => App_Setup,
      Process_Unit   => Process_Unit);

   package Args is
      use GNATCOLL.Opt_Parse;
      use Ada.Strings.Unbounded;
      use Application.Args;

      package Output_Folder is new Parse_Option
        (Parser, "-o", "--output", "Where to write the output results.",
         Unbounded_String,
         Default_Val => To_Unbounded_String (""));

      package Verbose is new Parse_Flag
        (Parser, "-v", "--verbose", "Be versbose.");

      package Version is new Parse_Flag
        (Parser, "", "--version", "Print version and exit.");

      package Simple is new Parse_Flag
        (Parser, "", "--simple", "Use simple JSON represenation for predefined.");

   end Args;

   type Current_Type_Info is record
      Type_Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Analyzser (Context : access Libadalang.Helpers.App_Job_Context;
                   Unit    : access Analysis_Unit) is tagged limited
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Spec_Buffer : Ada.Strings.Unbounded.Unbounded_String;
         Body_Buffer : Ada.Strings.Unbounded.Unbounded_String;
         Out_Folder  : Ada.Strings.Unbounded.Unbounded_String;
         Withs       : String_Sets.Set;
         Current     : Current_Type_Info;
         Outf        : Ada.Text_IO.File_Type;
      end record;

   procedure Create_File (Self : in out Analyzser; Name : Ada.Strings.Unbounded.Unbounded_String);
   procedure Put_Line (Self : in out Analyzser; Item : String);
   procedure Put_Line (Self : in out Analyzser; Item : Ada.Strings.Unbounded.Unbounded_String);
   procedure Close_File (Self : in out Analyzser);

   --  -------------------------------------------------------------------------

   procedure On_Ada_Abort_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abort_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abstract_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abstract_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Ada_Node_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Alternatives_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Constraint_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Decl_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Stmt_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aspect_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Base_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Expr_Alternative_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Stmt_Alternative_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Compilation_Unit_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Contract_Case_Assoc_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Defining_Name_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discriminant_Spec_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Elsif_Expr_Part_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Elsif_Stmt_Part_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Literal_Decl_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Expr_Alternatives_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discriminant_Choice_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Name_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Parent_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Param_Spec_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Pragma_Node_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Select_When_Part_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Unconstrained_Array_Index_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Variant_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aliased_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aliased_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_All_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_All_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Constrained_Array_Indices (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Unconstrained_Array_Indices (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aspect_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_At_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Attribute_Def_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Rep_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Record_Rep_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aspect_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Contract_Case_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Pragma_Argument_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Entry_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Subp_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Component_List (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Known_Discriminant_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Unknown_Discriminant_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Entry_Completion_Formal_Params (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Formal_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Record_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Record_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aggregate_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Multi_Dim_Array_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discriminant_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Param_Assoc (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Component_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discriminant_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Formal_Obj_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Formal_Package (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Formal_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Param_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Package_Internal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Package_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discrete_Base_Subtype_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subtype_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Classwide_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Incomplete_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Incomplete_Tagged_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Task_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Single_Task_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Anonymous_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Synth_Anonymous_Type_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abstract_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abstract_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Concrete_Formal_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Entry_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Literal_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Subp_Internal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Expr_Function (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Body (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Package_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Task_Body_Stub (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Entry_Body (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Package_Body (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Body (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Task_Body (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Entry_Index_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Error_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Exception_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Exception_Handler (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_For_Loop_Var_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Package_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Subp_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Package_Instantiation (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Subp_Instantiation (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Package_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Generic_Subp_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Label_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Named_Stmt_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Number_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Anonymous_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Extended_Return_Stmt_Object_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Package_Renaming_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Single_Protected_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Single_Task_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Stmt_Alternative (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Compilation_Unit (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Component_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Component_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Constant_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Constant_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Delta_Constraint (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Digits_Constraint (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discriminant_Constraint (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Index_Constraint (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Range_Constraint (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Declarative_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Private_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Public_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Elsif_Expr_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Elsif_Stmt_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Allocator (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Aggregate (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Record_Aggregate (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Bin_Op (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Relation_Op (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Box_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Expr_Alternative (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Contract_Cases (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_If_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Membership_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Attribute_Ref (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Update_Attribute_Ref (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Call_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Defining_Name (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discrete_Subtype_Name (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Dotted_Name (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_End_Name (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Explicit_Deref (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Qual_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Char_Literal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Identifier (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Abs (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_And (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_And_Then (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Concat (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Div (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Double_Dot (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Eq (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Gt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Gte (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_In (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Lt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Lte (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Minus (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Mod (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Mult (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Neq (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Not (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Not_In (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Or (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Or_Else (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Plus (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Pow (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Rem (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Op_Xor (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_String_Literal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Literal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Int_Literal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Real_Literal (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Target_Name (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Paren_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Quantified_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Raise_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Un_Op (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Handled_Stmts (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Interface_Kind_Limited (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Interface_Kind_Protected (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Interface_Kind_Synchronized (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Interface_Kind_Task (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Iter_Type_In (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Iter_Type_Of (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Library_Item (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Limited_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Limited_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_For_Loop_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_While_Loop_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Mode_Default (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Mode_In (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Mode_In_Out (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Mode_Out (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Not_Null_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Not_Null_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Component_Decl (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Others_Designator (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Overriding_Not_Overriding (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Overriding_Overriding (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Overriding_Unspecified (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Params (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Pragma_Node (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Prim_Type_Accessor (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Private_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Private_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Protected_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Quantifier_All (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Quantifier_Some (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Range_Spec (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Synthetic_Renaming_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Reverse_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Reverse_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Select_When_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Accept_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Accept_Stmt_With_Stmts (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_For_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_While_Loop_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Begin_Block (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Decl_Block (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Case_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Extended_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_If_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Named_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Select_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Error_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Abort_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Assign_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Call_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Delay_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Exit_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Goto_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Label (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Null_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Raise_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Requeue_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Return_Stmt (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Terminate_Alternative (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Kind_Function (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subp_Kind_Procedure (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subunit (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Synchronized_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Synchronized_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Tagged_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Tagged_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Task_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Access_To_Subp_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Anonymous_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Type_Access_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Array_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Derived_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Formal_Discrete_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Interface_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Mod_Int_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Private_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Decimal_Fixed_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Floating_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Ordinary_Fixed_Point_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Record_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Signed_Int_Type_Def (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Anonymous_Type (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Enum_Lit_Synth_Type_Expr (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Constrained_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Discrete_Subtype_Indication (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Unconstrained_Array_Index (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Until_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Until_Present (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Use_Package_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Use_Type_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Variant (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_Variant_Part (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_With_Clause (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_With_Private_Absent (Self : in out Analyzser; Node : Ada_Node'Class);

   procedure On_Ada_With_Private_Present (Self : in out Analyzser; Node : Ada_Node'Class);

end GNATCOLL.JSON.Builder;
