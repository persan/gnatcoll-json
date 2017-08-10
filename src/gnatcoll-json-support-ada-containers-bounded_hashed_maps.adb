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

package body GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Maps is
------------
-- Create --
------------

   function Create (Val : Map) return JSON_Value is
      Data : JSON_Array;
   begin
      for I in Val.Iterate loop
         declare
            O : constant JSON_Value := Create_Object;
         begin
            O.Set_Field ("Key", Create (Key (I)));
            O.Set_Field ("Element", Create (Element (I)));
            Append (Data, O);
         end;
      end loop;
      return Ret : constant JSON_Value := Create_Object do
         Set_Field (Ret, "Data", Create (Data));
         Set_Field (Ret, "Capacity", Create (Val.Capacity));
         Set_Field (Ret, "Modulus", Create (Val.Modulus));
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Map is
      Data     : JSON_Array;
      Capacity : Count_Type := 0;
      Modulus  : Hash_Type := 0;
      procedure Mapper (Name : UTF8_String; Value : JSON_Value) is
      begin
         if Name = "Data" then
            Data := Get (Value);
         elsif Name = "Capacity" then
            Capacity := Get (Value);
         elsif Name = "Modulus" then
            Modulus := Get (Value);
         end if;
      end Mapper;
   begin
      if Capacity < Count_Type (Length (Data)) then
         Capacity :=  Count_Type (Length (Data));
      end if;

      if Modulus = 0 then
         Modulus :=  Default_Modulus (Count_Type (Length (Data)));
      end if;
      Map_JSON_Object (Val, Mapper'Access);
      return Ret : Map (Capacity, Modulus) do
         for I in 1 .. Length (Data) loop
            declare
               O : constant JSON_Value := Get (Data, I);
            begin
               Ret.Insert (Key      => Key_Type'(Get (Get (O, "Key"))),
                           New_Item => Element_Type'(Get (Get (O, "Element"))));
            end;
         end loop;
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Map is
   begin
      return Get (JSON_Value'(Val.Get (Field)));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Map) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Containers.Bounded_Hashed_Maps;
