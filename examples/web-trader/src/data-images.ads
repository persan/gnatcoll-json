
package Data.Images is
   function Image (Item : Money) return String is (Item'Img);
   function Image (Item : UTC_Date) return String is (Item'Img);
   function Image (Item : Trade) return String;
   function Image (Item : Vector_Trades.Vector) return String;
   function Image (Item : Reply) return String;


end  Data.Images;
