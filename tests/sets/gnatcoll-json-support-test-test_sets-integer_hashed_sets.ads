with Ada.Containers.Hashed_Sets;
with GNATCOLL.JSON.Support.Test.Utilities;
package GNATCOLL.JSON.Support.Test.Test_Sets.Integer_Hashed_Sets is new
  Ada.Containers.Hashed_Sets
    (Element_Type        => Integer,
     Hash                => GNATCOLL.JSON.Support.Test.Utilities.Hash,
     Equivalent_Elements => "=");
