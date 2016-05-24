with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;
package JsonSample.String_Maps is new
  Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                         Element_Type    => String,
                                         Hash            =>  Ada.Strings.Fixed.Hash,
                                         Equivalent_Keys => "=");
