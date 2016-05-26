with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors
    (Index_Type            => Natural,
     Element_Type          => Integer,
     V                     => GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors,
     Create                => GNATCOLL.JSON.Create,
     Get                   => GNATCOLL.JSON.Get);
