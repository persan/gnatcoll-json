with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors;
with GNATCOLL.JSON; use GNATCOLL.JSON;
package JSONSample.String_Vectors.JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Vectors
    (V             => JSONSample.String_Vectors,
     Create        => Create,
     Get           => Get);
