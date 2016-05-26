with GNATCOLL.JSON.Support.Ada.Containers.@_NAME_@;
with GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@;
package GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.@_NAME_@
    (Index_Type            => Natural,
     Element_Type          => Integer,
     V                     => GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@,
     Create                => GNATCOLL.JSON.Create,
     Get                   => GNATCOLL.JSON.Get);
