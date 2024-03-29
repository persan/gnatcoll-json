with Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;
package Simple is

   type AA is range 1 .. 20;

   type Aa_Array is array (Natural range <>) of AA;

   type Aa_Array_Simple is array (Natural range <>) of AA with
     Annotate => (JSON, Simple);

   type Enum is (A, B, C, DD);

   type My_Mod is mod 33;

   subtype My is Integer;


   type Simple_Record is record
      D1_Integer     : Integer := 1;
      D3_Long_Float  : Long_Float := 0.0;
      D2_AA          : AA := 3;
      D4_Aa_Array    : Aa_Array (1 .. 10) := (others =>2);
      D5_Enum        : Enum := DD;
      D6_My          : My := 11 with
        Annotate => (JSON, Field_Name, "d6_renamed");
   end record;

   type Some_Interface is interface;
   type Some_Limited_interface is interface;
   type Some_syncronized_Interface is synchronized interface;

   type Abstract_Record is abstract tagged record
      Data : Integer := 0;
   end record;

   type Concrete_Taggd_Record is new Simple.Abstract_Record with record
      Data2 : Integer := 1;
   end record;


   type Concrete_Taggd_Record_with_Time is new Concrete_Taggd_Record with  record
      T : Ada.Calendar.Time := Ada.Calendar.Time_Of (2020, 01, 10, 100.0);
   end record;

   type Record_With_Discriminatns (D1 : Natural ; D2  : Boolean) is record
      Name : String (1 .. D1) := (others => '!');
      case D2 is
         when True =>
            F : Float := 3.14;
         when False =>
            I : Integer := 314;
      end case;
   end record;

   package My_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => String);

private
   type provate_type is new integer;
end Simple;
