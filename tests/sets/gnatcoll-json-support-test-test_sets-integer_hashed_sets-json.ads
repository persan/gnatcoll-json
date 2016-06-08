with GNATCOLL.JSON.Support.Ada.Containers.Hashed_Sets;
with GNATCOLL.JSON.Support.Test.Test_Sets.Integer_Hashed_Sets;
package GNATCOLL.JSON.Support.Test.Test_Sets.Integer_Hashed_Sets.JSON is
  new GNATCOLL.JSON.Support.Ada.Containers.Hashed_Sets
    (GNATCOLL.JSON.Support.Test.Test_Sets.Integer_Hashed_Sets,
     GNATCOLL.JSON.Create,
     GNATCOLL.JSON.Get);
