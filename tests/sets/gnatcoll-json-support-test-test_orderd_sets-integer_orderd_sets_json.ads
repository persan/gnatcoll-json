with GNATCOLL.JSON.Support.Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets;
package GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets_JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Vectors
    (Index_Type            => Natural,
     Element_Type          => Integer,
     V                     => GNATCOLL.JSON.Support.Test.Test_Orderd_Sets.Integer_Orderd_Sets,
     Create                => GNATCOLL.JSON.Create,
     Get                   => GNATCOLL.JSON.Get);
