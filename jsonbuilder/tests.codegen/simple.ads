with Ada.Calendar;
package Simple is


   type My_Integer is range 1 .. 20;

   type My_Integer_Array is array (Natural range <>) of My_Integer;
   --     type My_Constrained_Integer_Array_1 is array (Natural range 1..100) of My_Integer;
   --     type My_Constrained_Integer_Array_2 is array (1..100) of My_Integer;
   --     type My_Constrained_Integer_Array_3 is array (False .. True) of My_Integer;
   --     type My_Derived_Integer_Array is new  My_Integer_Array ( 1 .. 100);


   type Enum is (A, B, C, DD);

   type My_Mod is mod 33;

   subtype My is Integer;


   type My_Float is digits 4  range -10.0 .. 10.0;
   type My_Derived_Float is new Float;

   type Simple_Record is record
      D1  : Integer with Annotate => (JSON, Name, "d1");
      D3  : Long_Float;
      D2  : My_Integer;
      D4  : My_Integer_Array (1 .. 10);
      D5  : Enum;
      D6  : My;
      D7  : My with Annotate => (JSON, Name, "data-7");
   end record;

   --  type Some_Interface is interface;
   --     type Some_Limited_interface is interface;
   --     type Some_syncronized_Interface is synchronized interface;
   --
   --     type Abstract_Record is tagged record
   --  --   type Abstract_Record is abstract tagged record
   --        Data : Integer;
   --     end record;
   --
   --     type Concrete_Taggd_Record is new Simple.Abstract_Record with record
   --        Data2 : Integer;
   --     end record;
   --
   --     type Concrete_Taggd_Record_No_Codegen is new Abstract_Record with record
   --        Data2 : Integer;
   --        Data3 : Integer with Annotate => (JSON, Name, "data-22");
   --     end record;
   --
   --
   --     type Concrete_Taggd_Record_with_Time is new Concrete_Taggd_Record with  record
   --        T : Ada.Calendar.Time;
   --     end record;


   --     type Record_With_Discrimainant (Disc : Boolean) is  record
   --        Data2 : Integer;
   --     end record;
   --
   --     type Tagged_Record_With_Discrimainant (Disc : Boolean) is tagged record
   --        Data2 : Integer;
   --     end record;
   --
   --     type Record_With_Simple_Discrimainants (Discrimainat_1 : Boolean; Discrimainat_2 : Integer) is record
   --        Data2 : Integer;
   --     end record;
   --
   --     type Tagged_Record_With_Simple_Discrimainants (Discrimainat_1 : Boolean; Discrimainat_2 : Integer) is tagged record
   --        Data2 : Integer;
   --     end record;
   --
   --     type Record_With_Complex_Discrimainant_1 (Discrimainat :access Simple_Record) is record
   --        Data2 : Integer;
   --     end record;
   --     type Record_With_Complex_Discrimainant_2 (Discrimainat : not null access Simple_Record) is record
   --        Data2 : Integer;
   --     end record;

end Simple;
