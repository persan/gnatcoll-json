with GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

package JSONSample.String_Maps.JSON is new
  GNATCOLL.JSON.Support.Ada.Containers.Indefinite_Hashed_Maps
    (M                => JSONSample.String_Maps,
     Create_Key       => GNATCOLL.JSON.Create,
     Get_Name_Key     => GNATCOLL.JSON.Get,
     Create_Element   => GNATCOLL.JSON.Create,
     Get_Name_Element => GNATCOLL.JSON.Get);
