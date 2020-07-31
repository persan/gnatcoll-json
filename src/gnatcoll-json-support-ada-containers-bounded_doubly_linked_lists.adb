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

package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists is
   Data_Field_Name : constant String := "Data";
   ------------
   -- Create --
   ------------

   function Create (Val : List) return JSON_Array is
   begin
      return Data : JSON_Array do
         for I of Val loop
            Append (Data, Create (I));
         end loop;
      end return;
   end Create;

   -------------------
   -- Create_Object --
   -------------------
   function Create_Object (Val : List) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, Data_Field_Name, Create (Val));
      end return;
   end Create_Object;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return List is
      L : JSON_Array;
      procedure Cb (Name : UTF8_String; Value : JSON_Value) is
      begin
         if Name = Data_Field_Name then
            L := Get (Value);
         end if;
      end Cb;
   begin
      if Kind (Val) = JSON_Array_Type then
         L := Val.Get;
      else
         Map_JSON_Object (Val, Cb'Access);
      end if;
      return Ret : List (Count_Type (Length (L))) do
         for I in 1 .. Length (L) loop
            Ret.Append (Element_Type'(Get (Get (L, I))));
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return List is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : List) is
   begin
      Set_Field (Val, Field_Name, JSON_Array'(Create (Field)));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Doubly_Linked_Lists;
