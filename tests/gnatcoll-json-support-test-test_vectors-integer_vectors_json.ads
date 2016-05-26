with GNATCOLL.JSON.Support.Ada.Containers.Vectors;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Vectors (Index_Type      => Natural,
                                                Element_Type    => Integer,
                                                V               => GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors,
                                                Create          => GNATCOLL.JSON.Create,
                                                Get             => GNATCOLL.JSON.Get);
