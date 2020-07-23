with Ada.Containers.Indefinite_Vectors;
package GNATCOLL.JSON.Support.Generate.String_Vectors is new
  Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                     Element_Type => String);
