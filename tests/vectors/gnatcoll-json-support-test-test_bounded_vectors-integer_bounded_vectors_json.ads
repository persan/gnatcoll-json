with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors;
with GNATCOLL.JSON;
package GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Bounded_Vectors
    (V       => GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors,
     Create  => Create,
     Get     => Get);
