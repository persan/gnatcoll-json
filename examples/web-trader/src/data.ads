with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
package Data is
   use Ada.Strings.Unbounded;
   type Money is delta 0.00000001 range 0.0 .. 9_999_999_999.9;
   type UTC_Date is range 1_500_000_000 .. 3_000_000_000;

   type Trade is record

      Trade_Id   : Integer := 0;
      Pair       : Unbounded_String;
      Trade_Type : Unbounded_String;
      Price      : Money := 0.0;
      Quantity   : Money := 0.0;
      Amount     : Money := 0.0;
      Date       : UTC_Date := UTC_Date'First;
      Saved      : Boolean := False;

   end record;


   package Vector_Trades is new Ada.Containers.Vectors (Natural, Trade);

   type Reply is record
      BTC_USD : Vector_Trades.Vector;
   end record;

end Data;
