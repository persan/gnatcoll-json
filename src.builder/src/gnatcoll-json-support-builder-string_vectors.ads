with Ada.Containers.Indefinite_Vectors;
package GNATCOLL.JSON.Support.Builder.String_Vectors is new
  Ada.Containers.Indefinite_Vectors
    (Index_Type => Natural,
     Element_Type => Standard.String);
