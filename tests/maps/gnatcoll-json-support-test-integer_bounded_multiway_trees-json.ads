with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees;
package GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees.JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Bounded_Multiway_Trees
    (GNATCOLL.JSON.Support.Test.Integer_Bounded_Multiway_Trees, Create, Get);
