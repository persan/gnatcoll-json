with GNATCOLL.JSON.Support.Ada.Calendar;
package body Simple.JSON_Golden is
   use GNATCOLL.JSON.Support.Ada.Calendar;
   --  -------------------------------------------------------------------------
   --  Simple_Record
   --  -------------------------------------------------------------------------

   function Create (Val : Simple_Record) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Fields (Ret, Val);
      end return;
   end;

   function Get (Val : JSON_Value) return Simple_Record is
   begin
      return Ret : Simple_Record do
         Map_JSON_Object (Val, Map_JSON_Value'Access, Ret);
      end return;
   end;

   function Get (Val : JSON_Value; Field : UTF8_String) return Simple_Record is
   begin
      return Get (Get (Val, Field));
   end Get;


   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Simple_Record) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end;

   Simple_Record_D1_Integer_Field_Name    : constant String := "D1_Integer";
   Simple_Record_D3_Long_Float_Field_Name : constant String := "D3_Long_Float";
   Simple_Record_D2_AA_Field_Name         : constant String := "D2_AA";
   Simple_Record_D4_Aa_Array_Field_Name   : constant String := "D4_Aa_Array";
   Simple_Record_D5_Enum_Field_Name       : constant String := "D5_Enum";
   Simple_Record_D6_My_Field_Name         : constant String := "D6_My";

   procedure Set_Fields (Val : JSON_Value; Data : Simple_Record) is
   begin
      -- if tagged then
      --    set_fields(val,Simple_Record{parent}(data);
      -- end if;
      Set_Field (Val, Simple_Record_D1_Integer_Field_Name,    Data.D1_Integer);
      Set_Field_Long_Float (Val, Simple_Record_D3_Long_Float_Field_Name, Data.D3_Long_Float);
      Set_Field (Val, Simple_Record_D2_AA_Field_Name, Data.D2_AA);
      Set_Field (Val, Simple_Record_D4_Aa_Array_Field_Name, Data.D4_Aa_Array);
      Set_Field (Val, Simple_Record_D5_Enum_Field_Name, Data.D5_Enum);
      Set_Field (Val, Simple_Record_D6_My_Field_Name, Data.D6_My);
   end;

   procedure Map_JSON_Value (User_Object : in out Simple_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is
   begin
      if Name = Simple_Record_D1_Integer_Field_Name then
         User_Object.D1_Integer := Get (Value);
      elsif Name = Simple_Record_D3_Long_Float_Field_Name then
         User_Object.D3_Long_Float := Get_Long_Float (Value);
      elsif Name = Simple_Record_D2_AA_Field_Name then
         User_Object.D2_AA := Get (Value);
      elsif Name = Simple_Record_D4_Aa_Array_Field_Name then
         User_Object.D4_Aa_Array := Get (Value);
      elsif Name = Simple_Record_D5_Enum_Field_Name then
         User_Object.D5_Enum := Get (Value);
      elsif Name = Simple_Record_D6_My_Field_Name then
         User_Object.D6_My := Get (Value);
      else
         null;
         -- if tagged then
         --    Map_JSON_Value(Simple_Record{parent}(User_Object), Name, Value);
         -- end if;

      end if;
   end;

   -------------------------------------------------------------------------
   --  Abstract_Record
   -------------------------------------------------------------------------
   Abstract_Record_Data_Field_Name : constant String := "Data";

   procedure Map_JSON_Value (User_Object : in out Abstract_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is
   begin
      if Name = Abstract_Record_Data_Field_Name then
         User_Object.Data := Get (Value);
      else
         null;
      end if;
   end;

   procedure Set_Fields (Val : JSON_Value; Data : Abstract_Record) is
   begin
      Set_Field (Val, Abstract_Record_Data_Field_Name, Data.Data);
   end;

   --  --------------------------------------------------------------------
   --  ADA_DERIVED_TYPE_DEF: Concrete_Taggd_Record
   --  --------------------------------------------------------------------
   function Create (Val : Concrete_Taggd_Record) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Fields (Ret, Val);
      end return;
   end Create;

   function Get (Val : JSON_Value) return Concrete_Taggd_Record is
   begin
      return ret : Concrete_Taggd_Record do
         Map_JSON_Object (Val, Map_JSON_Value'Access, Ret);
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Concrete_Taggd_Record is
   begin
       return Concrete_Taggd_Record'(Get (JSON_Value'(Get (Val, Field))));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Concrete_Taggd_Record) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   Concrete_Taggd_Record_Data2_Field_Name : constant String := "Data2";

   procedure Set_Fields (Val : JSON_Value; Data : Concrete_Taggd_Record) is
   begin
      Set_Fields (Val, Abstract_Record (Data));
      Set_Field (Val, Concrete_Taggd_Record_Data2_Field_Name, Data.Data2);
   end;

   procedure Map_JSON_Value (User_Object : in out Concrete_Taggd_Record;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is
   begin
      if Name = Concrete_Taggd_Record_Data2_Field_Name then
         User_Object.Data2 := Get (Value);
      else
         Map_JSON_Value (Abstract_Record (User_Object), Name, Value);
      end if;

   end;


   --  --------------------------------------------------------------------
   --  Concrete_Taggd_Record_with_Time
   --  --------------------------------------------------------------------

   function Create (Val : Concrete_Taggd_Record_with_Time) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Fields (Ret, Val);
      end return;
   end Create;

   function Get (Val : JSON_Value) return Concrete_Taggd_Record_with_Time is
   begin
      return ret : Concrete_Taggd_Record_with_Time do
         Map_JSON_Object (Val, Map_JSON_Value'Access, Ret);
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Concrete_Taggd_Record_with_Time is
   begin
       return Concrete_Taggd_Record_with_Time'(Get (JSON_Value'(Get (Val, Field))));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Concrete_Taggd_Record_with_Time) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

      Concrete_Taggd_Record_With_Time_T_Field_Name : constant String := "T";

   procedure Map_JSON_Value (User_Object : in out Concrete_Taggd_Record_with_Time;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is
   begin
      if Name = Concrete_Taggd_Record_With_Time_T_Field_Name then
         User_Object.T := Get (Value);

      else
         Map_JSON_Value (Concrete_Taggd_Record (User_Object), Name, Value);
      end if;
   end;

   procedure Set_Fields (Val : JSON_Value; Data : Concrete_Taggd_Record_With_Time) is
   begin
      Set_Fields (Val, Concrete_Taggd_Record (Data));
      Set_Field (Val, Concrete_Taggd_Record_With_Time_T_Field_Name, Data.T);
   end;

end Simple.JSON_Golden;
