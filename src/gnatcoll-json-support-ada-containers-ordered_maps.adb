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

package body GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps is

   function Create (Val : Map_Entry) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Ret.Set_Field ("Key", Create (Val.Key));
         Ret.Set_Field ("Element", Create (Val.Element));
      end return;
   end Create;

   function Get (Val : JSON_Value) return Map_Entry is
   begin
      return Ret : Map_Entry do
         Ret.Key := Get (Get (Val, "Key"));
         Ret.Element := Get (Get (Val, "Element"));
      end return;
   end Get;

   ------------
   -- Create --
   ------------

   function Create (Val : Map) return JSON_Value is
      V : JSON_Array;
      procedure Process (Position : Cursor) is
      begin
         Append (V, Create (Map_Entry'((Key (Position), Element (Position)))));
      end Process;
   begin
      Val.Iterate (Process'Access);
      return Create (V);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Map is
      L : constant JSON_Array := Val.Get;
   begin
      return Ret : Map do
         for I in 1 .. Length (L) loop
            declare
               Value : constant Map_Entry := Get (Get (L, I));
            begin
               Ret.Include (Value.Key, Value.Element);
            end;
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Map is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Map)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Ordered_Maps;
