--------------------------------------------------------------------------------
--
--  This herarchy cotains a set of "support" packages for packing and
--  unpacking "Standard Types" to JSON_Values.
--  The folowing methods are providet for each type and the layout of the
--  JSON objects are kept equal for when its possible hence:
--     Ada.Containers.Vectors,
--     Ada.Containers.Indefinite_Vectors
--     Ada.Containers.Bounded_Vectors
--     Ada.Containers.Orderd_Sets
--     Ada.Containers.Boundeded_Hashed_Sets
--     Ada.Containers.Hashed_Sets
--     Ada.Containers.Bounded_Doubly_Linked_Lists
--     ...
--  Got the same JSON representation.
--
-- The methods provided for each type is:
--     function Create (Val : TYPE) return JSON_Value;
--     function Get (Val : JSON_Value) return TYPE;
--
--     function Get (Val : JSON_Value; Field : UTF8_String) return TYPE;
--     procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : TYPE);
--------------------------------------------------------------------------------

package GNATCOLL.JSON.Support is
   VERSION : constant String := "0.0.4";
   -- Note Tha above version shall be in sync with
   -- gnatcoll-jison.gpr  file and the README.md file
end GNATCOLL.JSON.Support;
