with Ada.Command_Line;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;
with Libadalang.Common; use Libadalang.Common;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;
with GNATCOLL.JSON.Builder.Templates;
with Langkit_Support.Text; use Langkit_Support.Text;
package body GNATCOLL.JSON.Builder is
   Spec_Source : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   Body_Source : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   procedure Print_SIGNED_INT_TYPE_DEF (Nodes : Ada_Node_Array) is
      Type_Name : constant Text_Type := Nodes (1).Text;
   begin
      Ada.Text_IO.Put_Line (Templates.SIGNED_INT_TYPE_Template);
   end Print_SIGNED_INT_TYPE_DEF;

   procedure Print_ARRAY_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_ARRAY_TYPE_DEF;

   procedure Print_ENUM_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_ENUM_TYPE_DEF;

   procedure Print_MOD_INT_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_MOD_INT_TYPE_DEF;
   procedure Print_DERIVED_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_DERIVED_TYPE_DEF;
   procedure Print_RECORD_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_RECORD_TYPE_DEF;
   procedure Print_INTERFACE_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_INTERFACE_TYPE_DEF;

   function Visit (Node : Ada_Node'Class) return Visit_Status;
   function Visit (Node : Ada_Node'Class) return Visit_Status is
   begin
      case Node.Kind is
         when Ada_Concrete_Type_Decl =>
            Put_Line ("----------------------------------------------------------------------");
            Ada.Text_IO.Put_Line ("1  " & Node.Image);
            declare
               Children : constant Ada_Node_Array := Node.Children;
            begin
               case Children (3).Kind is
                  when Ada_Signed_Int_Type_Def => Print_SIGNED_INT_TYPE_DEF (Children);
                  when Ada_Array_Type_Def => Print_ARRAY_TYPE_DEF (Children);
                  when Ada_Enum_Type_Def => Print_ENUM_TYPE_DEF (Children);
                  when Ada_Mod_Int_Type_Def => Print_MOD_INT_TYPE_DEF (Children);
                  when Ada_Derived_Type_Def => Print_DERIVED_TYPE_DEF (Children);
                  when Ada_Record_Type_Def => Print_RECORD_TYPE_DEF (Children);
                  when Ada_Interface_Type_Def => Print_INTERFACE_TYPE_DEF (Children);
                  when others => null;
               end case;

            end;

            return Over;
         when others =>
            return Into;
      end case;
   end Visit;
   procedure App_Setup (Context : App_Context; Jobs : App_Job_Context_Array) is
      pragma Unreferenced (Context, Jobs);
   begin
      GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
      GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);

      Ada.Text_IO.Put_Line (Ada.Command_Line.Command_Name);
   end;

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);

   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
      else
         Unit.Root.Traverse (Visit'Access);
      end if;
   end Process_Unit;
end GNATCOLL.JSON.Builder;
