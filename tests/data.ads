with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Data is
   pragma Annotate (FOOL, TEXT);
   type Rec is tagged limited private;
   procedure Add (Self : in out Rec; Data : String);
   procedure Add (Self : in out Rec; Data : Integer);
   procedure Add (Self : in out Rec; Data : Float);

private
   package Foxes is new Ada.Containers.Vectors (Natural, Natural);

   type Rec is tagged limited record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Count : Float := 0.0;
      Fox   : Foxes.Vector;
   end record;
end Data;
