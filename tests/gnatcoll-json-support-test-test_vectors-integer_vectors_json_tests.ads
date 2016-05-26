with GNATCOLL.JSON.Support.Ada.Containers.Vectors.Test;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_Initialize;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON;
with GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors;
package GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_JSON_Tests is new
  GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors.Test
    (Initialize => GNATCOLL.JSON.Support.Test.Test_Vectors.Integer_Vectors_Initialize);
