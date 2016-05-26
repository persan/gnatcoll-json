with GNATCOLL.JSON.Support.Test.Ada.Containers.@_NAME_@;
with GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_Initialize;
with GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON;
with GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@;
package GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON_Tests is new
  Standard.GNATCOLL.JSON.Support.Test.Ada.Containers.@_NAME_@
    (Index_Type   => Natural,
     Element_Type => Integer,
     "="          => "=",
     Create       => Create,
     Get          => Get,
     V            => Integer_@_NAME_@,
     JSON         => Standard.GNATCOLL.JSON.Support.Test.Test_@_NAME_@.Integer_@_NAME_@_JSON,
     Initialize   => Integer_@_NAME_@_Initialize);
