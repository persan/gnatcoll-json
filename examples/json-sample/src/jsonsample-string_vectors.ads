with Ada.Containers.Indefinite_Vectors;
package JsonSample.String_Vectors is new
  Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                     Element_Type => String);
