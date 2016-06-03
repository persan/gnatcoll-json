
with GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is new Standard.Ada.Numerics.Generic_Complex_Types (<>);
   with package Complex_Types_JSON is new GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Types (Complex_Types);
package GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays is

end GNATCOLL.JSON.Support.Ada.Numerics.Generic_Complex_Arrays;
