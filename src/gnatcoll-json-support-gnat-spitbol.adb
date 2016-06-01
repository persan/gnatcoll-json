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

with Interfaces;
package body GNATCOLL.JSON.Support.GNAT.SPitbol is

   ----------------
   -- JSON_Table --
   ----------------

   package body JSON_Table is

      ------------
      -- Create --
      ------------

      function Create (Val : Table) return JSON_Value is
      begin
         return Create (Convert_To_Array (Val));
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return Table is
         Data : constant JSON_Array := Get (Val);
      begin
         return Ret : Table (Interfaces.Unsigned_32 (Length (Data))) do
            for E of Table_Array'(Get (Val)) loop
               Set (Ret, E.Name, E.Value);
            end loop;
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Val : JSON_Value; Field : UTF8_String) return Table is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : Table)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : Table_Entry) return JSON_Value is
      begin
         return Ret : constant JSON_Value := Create_Object do
            Set_Field (Ret, "Key", Create (Val.Name));
            Set_Field (Ret, "Element", Create (Val.Value));
         end return;
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return Table_Entry is
      begin
         return Ret : Table_Entry do
            Ret.Name := Get (Val, "Key");
            Ret.Value := Get (Get (Val, "Element"));
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val   : JSON_Value;
         Field : UTF8_String)
         return Table_Entry
      is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : Table_Entry)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;

      ------------
      -- Create --
      ------------

      function Create (Val : Table_Array) return JSON_Value is
         Data : JSON_Array;
      begin
         for D of   Val loop
            Append (Data, Create (D));
         end loop;
         return Create (Data);
      end Create;

      ---------
      -- Get --
      ---------

      function Get (Val : GNATCOLL.JSON.JSON_Value) return Table_Array is
         D : constant JSON_Array := Get (Val);
      begin
         return Ret : Table_Array (1 .. Length (D)) do
            for I in Ret'Range loop
               Ret (I) := Get (Get (D, I));
            end loop;
         end return;
      end Get;

      ---------
      -- Get --
      ---------

      function Get
        (Val   : JSON_Value;
         Field : UTF8_String)
         return Table_Array
      is
      begin
         return Get (JSON_Value'(Get (Val, Field)));
      end Get;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Val        : JSON_Value;
         Field_Name : UTF8_String;
         Field      : Table_Array)
      is
      begin
         Set_Field (Val, Field_Name, Create (Field));
      end Set_Field;

   end JSON_Table;

end GNATCOLL.JSON.Support.GNAT.SPitbol;
