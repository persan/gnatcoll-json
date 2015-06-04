with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Data is
   type Rec is tagged limited private;

private
   package Foxes is new Ada.Containers.Vectors (Natural, Natural);

   type Rec is tagged limited record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Count : Integer := 0;
      Fox   : Foxes.Vector;
   end record;
end Data;
