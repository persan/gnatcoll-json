------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees is

   ------------
   -- Create --
   ------------

   function Create (Val : Tree) return JSON_Value is
      function Node (C : Cursor) return JSON_Value;
      function Children (C : Cursor) return JSON_Array;
      function Node (C : Cursor) return JSON_Value is
      begin
         return Ret : constant JSON_Value := Create_Object do
            Set_Field (Ret, "Element", Create (Element (C)));
            Set_Field (Ret, "Children", Children (C));
         end return;
      end Node;

      function Children (C : Cursor) return JSON_Array is
         Ret : JSON_Array;
         procedure Process (Position : Cursor) is
         begin
            Append (Ret, Node (Position));
         end Process;
      begin
         Iterate_Children (C, Process'Access);
         return Ret;
      end Children;
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "Capacity", Create (Val.Capacity));
         Set_Field (Ret, "Data", Children (Val.Root));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Tree is

      procedure Append_Children (To  : in out Tree; Root : Cursor; Nodes : JSON_Array);
      procedure Append_Node (To  : in out Tree; Root : Cursor; Node : JSON_Value);

      procedure Append_Node (To  : in out Tree; Root : Cursor; Node : JSON_Value) is
         Element     : Element_Type;
         Children    : JSON_Array;
         Has_Element : Boolean := False;
         procedure CB_Node (Name : UTF8_String; Value : JSON_Value) is
         begin
            if Name = "Element" then
               Element := Get (Value);
               Has_Element := True;
            elsif Name = "Children" then
               Children := Get (Value);
            end if;
         end CB_Node;
      begin
         Map_JSON_Object (Node, CB_Node'Access);
         if Has_Element then
            To.Append_Child (Root, Element);
            Append_Children (To, Root, Children);
         end if;
      end Append_Node;

      procedure Append_Children (To  : in out Tree; Root : Cursor; Nodes : JSON_Array) is
      begin
         for I in 1 .. Length (Nodes) loop
            Append_Node (To, Root, Get (Nodes, I));
         end loop;
      end Append_Children;

      Capacity : Count_Type;
      Data     : JSON_Array;
      pragma Unreferenced (Data);
      procedure CB (Name : UTF8_String; Value : JSON_Value) is
      begin
         if Name = "Capacity" then
            Capacity := Get (Value);
         elsif Name = "Data" then
            Data := Get (Value);
         end if;
      end CB;

   begin
      Map_JSON_Object (Val, CB'Access);
      return Ret : Tree (Capacity) do
         null;
         --  Append_Node (Ret, Ret.Root, Data);
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Tree is

   begin
      return Get ((Get (Val, Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Tree) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
