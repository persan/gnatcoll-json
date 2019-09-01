with Ada.Calendar;
package Simple is

   type AA is range 1 .. 20;

   type Aa_Array is array (Natural range <>) of AA;

   type Enum is (A, B, C, DD);

   type My_Mod is mod 33;

   subtype My is Integer;


   type Simple_Record is record
      D1  : Integer;
      D3  : Long_Float;
      D2  : AA;
      D4  : Aa_Array (1 .. 10);
      D5  : Enum;
      D6  : My;
   end record;

   type Some_Interface is interface;
   type Some_Limited_interface is interface;
   type Some_syncronized_Interface is synchronized interface;

   type Abstract_Record is abstract tagged record
      Data : Integer;
   end record;

   type Concrete_Taggd_Record is new Simple.Abstract_Record with record
      Data2 : Integer;
   end record with Annotate => Top_Level;

   type Concrete_Taggd_Record_No_Codegen is new Abstract_Record with record
      Data2 : Integer;
   end record with Annotate => No_Code_Gen;


   type Concrete_Taggd_Record_with_Time is new Concrete_Taggd_Record with  record
      T : Ada.Calendar.Time with Annotate => Key;
   end record;
end Simple;
