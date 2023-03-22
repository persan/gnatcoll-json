with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;

with GNATCOLL.JSON.Builder.Templates;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

with Templates_Parser;

package body GNATCOLL.JSON.Builder.Application is
   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   Spec_Source : Ada.Strings.Unbounded.Unbounded_String;
   Body_Source : Ada.Strings.Unbounded.Unbounded_String;

   Spec_Includes : String_Sets.Set;
   Body_Includes : String_Sets.Set;

   function Get_First (From : Ada_Node'Class; Kind : Ada_Node_Kind_Type; Indent : String := "  ") return Ada_Node'Class is
   begin
      Put_Line (Indent & From.Image);
      for I in From.First_Child_Index .. From.Last_Child_Index loop
         if not From.Child (I).Is_Null then
            if From.Child (I).Kind = Kind then
               return From.Child (I);
            else
               return Get_First (From.Child (I), Kind, Indent & "  ");
            end if;
         end if;
      end loop;
      return No_Ada_Node;
   end Get_First;

   function To_String (Set : String_Sets.Set; Delimiter : String := "" & ASCII.LF) return String is
      First : Boolean := True;
      Ret   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I of Set loop
         if not First then
            Ret.Append (Delimiter);
         end if;
         First := False;
         Ret.Append (I);
      end loop;
      return Ret.To_String;
   end;

   --  -------------------------------------------------------------------------

   procedure Print_SIGNED_INT_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr        : Templates_Parser.Translate_Set;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      Spec_Source.Append (Unbounded_String'(Parse (Templates.SIGNED_INT_TYPE_Template, Tr)));
      Spec_Includes.Include (Parse (Templates.SIGNED_INT_TYPE_With_Template, Tr));
   end Print_SIGNED_INT_TYPE_DEF;

   --  -------------------------------------------------------------------------
   function Get_Name (Node : Ada_Node) return Text_Type is
      Ret : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      function Visit (Node : Ada_Node'Class) return Visit_Status;
      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
         when Ada_Dotted_Name | Ada_Identifier =>
            Ret.Append (Node.Text);
            return Stop;
         when others =>
            return Into;
         end case;
      end;
   begin
      Node.Traverse (Visit'Access);
      return Ret.To_Wide_Wide_String;
   end;

   procedure Print_ARRAY_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr        : Templates_Parser.Translate_Set;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      Tr.Insert (Assoc ("Index", To_String (Get_Name (Nodes (Nodes'First + 2).Child (1)))));
      Tr.Insert (Assoc ("Element", To_String (Get_Name (Nodes (Nodes'First + 2).Child (2)))));
      Spec_Source.Append (Unbounded_String'(Parse (Templates.ARRAY_TYPE_Template, Tr)));
      Spec_Includes.Include (Parse (Templates.ARRAY_TYPE_With_Template, Tr));
   end Print_ARRAY_TYPE_DEF;

   --  -------------------------------------------------------------------------

   procedure Print_ENUM_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr        : Templates_Parser.Translate_Set;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      Spec_Source.Append (Unbounded_String'(Parse (Templates.ENUM_TYPE_Template, Tr)));
      Spec_Includes.Include (Parse (Templates.ENUM_TYPE_With_Template, Tr));
   end Print_ENUM_TYPE_DEF;

   --  -------------------------------------------------------------------------

   procedure Print_MOD_INT_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr        : Templates_Parser.Translate_Set;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      Spec_Source.Append (Unbounded_String'(Parse (Templates.MOD_INT_TYPE_Template, Tr)));
      Spec_Includes.Include (Parse (Templates.MOD_INT_TYPE_With_Template, Tr));
   end Print_MOD_INT_TYPE_DEF;

   --  -------------------------------------------------------------------------

   procedure Print_DERIVED_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr        : Templates_Parser.Translate_Set;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      Spec_Source.Append (Unbounded_String'(Parse (Templates.DERIVED_TYPE_Template, Tr)));
   end Print_DERIVED_TYPE_DEF;

   --  -------------------------------------------------------------------------

   procedure Print_RECORD_TYPE_DEF (Nodes : Ada_Node_Array) is
      Tr          : Templates_Parser.Translate_Set;
      Fields      : Templates_Parser.Vector_Tag;
      Field_Names : Templates_Parser.Vector_Tag;
      use Templates_Parser;
   begin
      Tr.Insert (Assoc ("Name", To_String (Nodes (Nodes'First).Text)));
      for Node of Nodes loop
         Put_Line (" - " & Node.Image);
      end loop;
      for Field of Get_First (Nodes (Nodes'First + 2), Ada_Ada_Node_List).Children loop
         Fields.Append (To_String (Get_First (Field, Ada_Defining_Name).Text));
         Field_Names.Append (To_String (Get_First (Field, Ada_Defining_Name).Text));
      end loop;

      Tr.Insert (Assoc ("Field", Fields));
      Tr.Insert (Assoc ("Field_Name", Field_Names));

      Spec_Source.Append (Unbounded_String'(Parse (Templates.RECORD_TYPE_Template, Tr)));
   end Print_RECORD_TYPE_DEF;

   --  -------------------------------------------------------------------------

   procedure Print_INTERFACE_TYPE_DEF (Nodes : Ada_Node_Array) is
   begin
      null;
   end Print_INTERFACE_TYPE_DEF;

   --  -------------------------------------------------------------------------

   function Visit (Node : Ada_Node'Class) return Visit_Status;
   function Visit (Node : Ada_Node'Class) return Visit_Status is
   begin
      case Node.Kind is
         when Ada_Concrete_Type_Decl =>
            Put_Line (String'("----------------------------------------------------------------------"));
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
                  when others => Ada.Text_IO.Put_Line (Children (3).Kind'Image);
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

   function Unit_Name (Root : Ada_Node'Class) return String is
   begin
      for Ix in Root.First_Child_Index .. Root.Last_Child_Index loop
         if (not Root.Child (Ix).Is_Null) and then Root.Child (Ix).Kind = Ada_Library_Item then
            return To_String (Root.Child (Ix).Child (2).Child (1).Text);
         end if;
      end loop;
      raise Program_Error with "No name found.";
   end;

   Ada2File : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZ.",
                                                                                          "abcdefghijklmnopqrstuvwxyz-");
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
      Outf : Ada.Text_IO.File_Type;
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Ada.Text_IO.Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
      else
         Unit.Root.Traverse (Visit'Access);
         declare
            Name      : constant String := Unit_Name (Unit.Root) & ".JSON";
            File_Name : constant String := Ada.Strings.Fixed.Translate (Name, Ada2File);
            Tr        : Templates_Parser.Translate_Set;
            use Templates_Parser;
         begin
            Tr.Insert (Assoc ("Name", Name));
            Tr.Insert (Assoc ("methods", Spec_Source.To_String));
            Tr.Insert (Assoc ("withs", To_String (Spec_Includes)));
            Outf.Create (Ada.Text_IO.Out_File, File_Name & ".ads");
            Outf.Put_Line (Parse (Templates.Spec_Template, Tr));
            Outf.Close;

            Tr.Insert (Assoc ("Name", Name));
            Tr.Insert (Assoc ("withs", To_String (Body_Includes)));
            Tr.Insert (Assoc ("methods", Body_Source.To_String));
            Outf.Create (Ada.Text_IO.Out_File, File_Name & ".adb");
            Outf.Put_Line (Parse (Templates.Body_Template, Tr));
            Outf.Close;
         end;
      end if;
   end Process_Unit;
end GNATCOLL.JSON.Builder.Application;