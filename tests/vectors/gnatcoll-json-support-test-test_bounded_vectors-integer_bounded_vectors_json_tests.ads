with GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_Initialize;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON;
with GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON_Tests is new
  Standard.GNATCOLL.JSON.Support.Test.Ada.Containers.Bounded_Vectors
    (Index_Type   => Natural,
     Element_Type => Integer,
     "="          => "=",
     Create       => Create,
     Get          => Get,
     V            => Integer_Bounded_Vectors,
     JSON         => Standard.GNATCOLL.JSON.Support.Test.Test_Bounded_Vectors.Integer_Bounded_Vectors_JSON,
     Initialize   => Integer_Bounded_Vectors_Initialize);
