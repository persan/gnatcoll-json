with ada.Strings.Unbounded;use ada.Strings.Unbounded;
package Simple.Test_Containers.Unbouded_String_Vectors
is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Unbounded_String);
