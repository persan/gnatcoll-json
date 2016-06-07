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

package body GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays is

   ------------
   -- Create --
   ------------

   function Create (Val : Real_Vector) return JSON_Value is
      Data : JSON_Array;
   begin
      for I of Val loop
         Append (Data, Create (Long_Float (I)));
      end loop;
      return Create (Data);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Real_Vector is
      Data : constant JSON_Array := Get (Val);
   begin
      return Ret : Real_Vector (1 .. Length (Data)) do
         for I in Ret'Range loop
            Ret (I) := Real (Get_Long_Float (Get (Data, I)));
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Real_Vector
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Vector)
   is
   begin
      Set_Field (Val, Field_Name , Create (Field));
   end Set_Field;

   ------------
   -- Create --
   ------------

   function Create (Val : Real_Matrix) return JSON_Value is
      Data : JSON_Array;
   begin
      for X in Val'First (1) .. Val'Last (1) loop
         declare
            Dx   : JSON_Array;
         begin
            for Y in Val'First (1) .. Val'Last (1) loop
               Append (Dx, Create (Long_Float (Val (X, Y))));
            end loop;
            Append (Data , Create (Dx));
         end;
      end loop;
      return Create (Data);
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Real_Matrix is
      Data : constant JSON_Array := Get (Val);
      X    : constant Natural := Length (Data);
      Y    : Natural;
   begin
      if X = 0 then
         return Ret : Real_Matrix (1 .. 0, 1 .. 0) do
            null;
         end return;
      end if;
      Y := Length (Get (Get (Data, 1)));
      if Y = 0 then
         return Ret : Real_Matrix (1 .. 0, 1 .. 0) do
            null;
         end return;
      end if;
      return Ret : Real_Matrix (1 .. X , 1 .. Y ) do
         for Ix in 1 .. X loop
            declare
               Data_Row : constant JSON_Array := Get (Get (Data, Ix));
            begin
               for Iy in 1 .. Length (Data_Row) loop
                  Ret (Ix, Iy) := Real (Get_Long_Float (Get (Data_Row , Iy)));
               end loop;
            end;
         end loop;
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return Real_Matrix
   is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Real_Matrix)
   is
   begin
      Set_Field (Val, Field_Name , Create (Field));
   end Set_Field;

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Real_Arrays;
