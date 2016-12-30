package Simple is
   type AA is range 1 .. 20;
   type Aa_Array is array (Natural range <>) of AA;
   type Enum is (A, B, C, DD);
   subtype My is Integer;
   type Simple_Record is record
      D1  : Integer;
      D3  : Long_Float;
      D2  : AA;
      D4  : Aa_Array (1 .. 10);
      D5  : Enum;
      D6  : My;
   end record;
end Simple;
