with GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_Initialize;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON_Tests is new
  Standard.GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors
    (Index_Type   => Natural,
     Element_Type => Integer,
     "="          => "=",
     Create       => Create,
     Get          => Get,
     V            => Integer_Vectors,
     JSON         => Standard.GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON,
     Initialize   => Integer_Vectors_Initialize);
