pragma Ada_2012;
package body Data.Images is
   use Ada.Strings.Unbounded;
   function Image (S : Unbounded_String ) return String renames To_String;
   function Image (Item : Integer) return String is (Item'Img);
   function Image (Item : Boolean) return String is (Item'Img);

   -----------
   -- Image --
   -----------

   function Image (Item : Trade) return String is
   begin
     return  "(Trade_Id  =>" & Image(Item.Trade_Id) & ASCII.LF &
      "Pair       =>" & Image(Item.Pair) & ASCII.LF &
      "Trade_Type =>" & Image(Item.Trade_Type) & ASCII.LF &
      "Price      =>" & Image(Item.Price) & ASCII.LF &
      "Quantity   =>" & Image(Item.Quantity) & ASCII.LF &
      "Amount     =>" & Image(Item.Amount) & ASCII.LF &
      "Date       =>" & Image(Item.Date) & ASCII.LF &
      "Saved      =>" & Image(Item.Saved) & ")";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Vector_Trades.Vector) return String is
      Buffer : Unbounded_String;
      First  : Boolean := True;
   begin
      for I of Item loop
         if not First then
            Append (Buffer, ", ");
         end if;
         First := False;
         Append (Buffer, Image (I));
      end loop;
      return To_String (Buffer);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Item : Reply) return String is
   begin
      return "( BTC_USD => " & image(item.BTC_USD) & ")";
   end Image;

end Data.Images;
