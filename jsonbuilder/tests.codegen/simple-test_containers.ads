with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
package Simple.Test_Containers is
   package Integer_Vectors is new Ada.Containers.Vectors (Natural, Integer);
   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, String);
end Simple.test_Containers;
