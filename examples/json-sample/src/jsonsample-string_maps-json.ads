with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps;
with GNATCOLL.JSON; use GNATCOLL.JSON;
package JSONSample.String_Maps.JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps
    (JSONSample.String_Maps, Create, Get, Create, Get);
