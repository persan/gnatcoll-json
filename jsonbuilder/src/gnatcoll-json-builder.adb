
--  with Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with Libadalang.Common; use Libadalang.Common;

package body GNATCOLL.JSON.Builder is
   Spec_Source : Ada.Strings.Unbounded.Unbounded_String;
   Body_Source : Ada.Strings.Unbounded.Unbounded_String;
   procedure Simple_Spec (Name : String) is
   begin
      Spec_Source.Append ("--  -------------------------------------------------------------------------" & ASCII.LF);
      Spec_Source.Append ("--  short " & ASCII.LF);
      Spec_Source.Append ("--" & ASCII.LF);
      Spec_Source.Append ("function Create (Val : " & Name & ") return JSON_Value;" & ASCII.LF);
      Spec_Source.Append ("function Get (Val : JSON_Value) return " & Name & ";" & ASCII.LF);
      Spec_Source.Append ("function Get (Val : JSON_Value; Field : UTF8_String) return " & Name & ";" & ASCII.LF);
      Spec_Source.Append ("procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : " & Name & ");" & ASCII.LF);
      Spec_Source.Append ("--  -------------------------------------------------------------------------" & ASCII.LF);
      Spec_Source.Append ("" & ASCII.LF);

   end;

   procedure Simple_Body (Name : String) is
   begin
      Body_Source.Append ("   --  --------------------------------------------------------------------" & ASCII.LF);
      Body_Source.Append ("   --  ADA_DERIVED_TYPE_DEF: short " & ASCII.LF);
      Body_Source.Append ("   --  --------------------------------------------------------------------" & ASCII.LF);
      Body_Source.Append ("   function Create (Val : short) return JSON_Value is" & ASCII.LF);
      Body_Source.Append ("   begin" & ASCII.LF);
      Body_Source.Append ("      return ret : JSON_Value := Create_Object do" & ASCII.LF);
      Body_Source.Append ("         null;" & ASCII.LF);
      Body_Source.Append ("      end return;" & ASCII.LF);
      Body_Source.Append ("   end Create;" & ASCII.LF);
      Body_Source.Append ("" & ASCII.LF);
      Body_Source.Append ("   function Get (Val : JSON_Value) return short is" & ASCII.LF);
      Body_Source.Append ("   begin" & ASCII.LF);
      Body_Source.Append ("      return ret : short do" & ASCII.LF);
      Body_Source.Append ("         null;" & ASCII.LF);
      Body_Source.Append ("      end return;" & ASCII.LF);
      Body_Source.Append ("   end Get;" & ASCII.LF);
      Body_Source.Append ("" & ASCII.LF);
      Body_Source.Append ("function Get (Val : JSON_Value; Field : UTF8_String) return short is" & ASCII.LF);
      Body_Source.Append ("   begin" & ASCII.LF);
      Body_Source.Append ("       return short'(Get (JSON_Value'(Get (Val, Field))));" & ASCII.LF);
      Body_Source.Append ("   end Get;" & ASCII.LF);
      Body_Source.Append ("" & ASCII.LF);
      Body_Source.Append ("   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : short) is" & ASCII.LF);
      Body_Source.Append ("   begin" & ASCII.LF);
      Body_Source.Append ("      Set_Field (Val, Field_Name, Create (Field));" & ASCII.LF);
      Body_Source.Append ("   end Set_Field;" & ASCII.LF);
      Body_Source.Append ("" & ASCII.LF);
      Body_Source.Append ("   end;" & ASCII.LF);
      Body_Source.Append ("" & ASCII.LF);
   end Simple_Body;
   procedure Integer_Type (Name : String) is
   begin
      Simple_Spec (Name);
   end Integer_Type;

   procedure Integer_Body (Name : String) is
   begin
      Simple_Spec (Name);
   end Integer_Body;
   use Ada;
   --   use Ada.Strings.Unbounded;

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);

      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
         function Visit_Ada_Concrete_Type_Decl (Node : Ada_Node'Class) return Visit_Status;
         function Visit_Ada_Concrete_Type_Decl (Node : Ada_Node'Class) return Visit_Status is
            function Visit_Ada_Node_List (Node : Ada_Node'Class) return Visit_Status;
            function Visit_Ada_Node_List (Node : Ada_Node'Class) return Visit_Status is
               function Visit_Ada_Component_Decl (Node : Ada_Node'Class) return Visit_Status;
               function Visit_Ada_Component_Decl (Node : Ada_Node'Class) return Visit_Status is

               begin
                  case Node.Kind is
                  when Ada_Defining_Name =>
                     Put_Line ("       " & Node.Image);
                     return Over;
                  when others =>
                     return Into;
                  end case;
               end;

            begin
               case Node.Kind is
               when Ada_Component_Decl =>
                  Put_Line ("   " & Node.Image);
                  Node.Traverse (Visit_Ada_Component_Decl'Access);
                  return Over;
               when others =>
                  return Into;
               end case;
            end;
         begin
            case Node.Kind is
            when Ada_Identifier =>
               Put_Line (" " & Node.Image);
               return Over;
            when Ada_Component_Decl =>
               Node.Traverse (Visit_Ada_Node_List'Access);
               return Over;
            when others =>
               return Into;
            end case;
         end;
      begin
         case Node.Kind is
         when Ada_Concrete_Type_Decl =>
            Put_Line (Node.Image);
            Node.Traverse (Visit_Ada_Concrete_Type_Decl'Access);
            return Over;
         when others =>
            return Into;
         end case;
      end Visit;

   begin
      Unit.Root.Traverse (Visit'Access);
   end Process_Unit;
end GNATCOLL.JSON.Builder;
