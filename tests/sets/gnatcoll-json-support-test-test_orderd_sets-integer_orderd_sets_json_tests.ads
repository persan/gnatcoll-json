with GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_Initialize;
with GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON;
with GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets;
package GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON_Tests is new
  Standard.GNATCOLL.JSON.Support.Test.Ada.Containers.Vectors
    (Index_Type   => Natural,
     Element_Type => Integer,
     "="          => "=",
     Create       => Create,
     Get          => Get,
     V            => Integer_Orderd_Sets,
     JSON         => Standard.GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON,
     Initialize   => Integer_Orderd_Sets_Initialize);
