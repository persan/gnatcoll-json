pragma Ada_2012;
package body Data.JSON is
   use GNATCOLL.JSON;

   -- ==============================================================================================
   -- ==                                Trade                                                     ==
   -- ==============================================================================================
   ------------
   -- Create --
   ------------

   function Create (Val : Trade) return JSON_Value is
   begin
      return Ret :  JSON_Value := Create_Object do
         Set_Field (Ret, "trade_id", Val.Trade_Id);
         --           Set_Field (Ret, "trade_id", Val.Pair);
         --           Set_Field (Ret, "trade_id", Val.Trade_Type);
         Set_Field (Ret, "price", Val.Price);
         Set_Field (Ret, "quantity", Val.Quantity);
         Set_Field (Ret, "amount", Val.Amount);
         Set_Field (Ret, "date", Val.Date);
         -- Set_Field (Ret, "trade_id", Val.Saved);

      end return;
   end Create;

   ---------
   -- Get --
   ---------
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Trade);
   procedure Map (User_Object : in out Trade;
                  Name        : UTF8_String;
                  Value       : JSON_Value) is
   begin
      if Name = "trade_id" then
         User_Object.Trade_Id := Get (Value);
      elsif Name = "pair" then
         User_Object.pair := Get (Value);
      elsif Name = "trade_type" then
         User_Object.trade_type := Get (Value);
      elsif Name = "price" then
         User_Object.price := Get (Value);
      elsif Name = "quantity" then
         User_Object.quantity := Get (Value);
      elsif Name = "pair" then
         User_Object.pair := Get (Value);
      elsif Name = "amount" then
         User_Object.amount := Get (Value);
      elsif Name = "date" then
         User_Object.date := Get (Value);
      else
         null;
      end if;

   end;

   function Get (Val : JSON_Value) return Trade is

   begin
      return ret :trade do
         Map_JSON_Object (Val, Map'Access, Ret);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Trade is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Trade)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   -- ==============================================================================================
   -- ==                                Reply                                                     ==
   -- ==============================================================================================
   ------------
   -- Create --
   ------------

   function Create (Val : Reply) return JSON_Value is
   begin
      return Ret :  JSON_Value := Create_Object do
         Set_Field (Ret, "BTC_USD", Val.BTC_USD);
      end return;
   end Create;

   ---------
   -- Get --
   ---------
   procedure Map_JSON_Object is new Gen_Map_JSON_Object (Reply);
   procedure Map (User_Object : in out Reply;
                  Name        : UTF8_String;
                  Value       : JSON_Value) is
   begin
      if Name = "BTC_USD" then
         User_Object.BTC_USD := Get (Value);
      else
         null;
      end if;

   end;

   function Get (Val : JSON_Value) return Reply is

   begin
      return ret :Reply do
         Map_JSON_Object (Val, Map'Access, Ret);
      end return;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value; Field : UTF8_String) return Reply is
   begin
      return Get (Get (Val, Field));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Reply)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end Data.JSON;
