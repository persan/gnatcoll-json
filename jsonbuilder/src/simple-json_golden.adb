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
   end Set_Field;

   Simple_Record_D1_Integer_Field_Name    : constant String := "D1_Integer";
   Simple_Record_D3_Long_Float_Field_Name : constant String := "D3_Long_Float";
   Simple_Record_D2_AA_Field_Name         : constant String := "D2_AA";
   Simple_Record_D4_Aa_Array_Field_Name   : constant String := "D4_Aa_Array";
   Simple_Record_D5_Enum_Field_Name       : constant String := "D5_Enum";
   Simple_Record_D6_My_Field_Name         : constant String := "D6_My";

   procedure Set_Fields (Val : JSON_Value; Data : Simple_Record) is
   begin
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
      case Name is
      when Simple_Record_D1_Integer_Field_Name => User_Object.D1_Integer := Get (Value);
      when Simple_Record_D3_Long_Float_Field_Name => User_Object.D3_Long_Float := Get_Long_Float (Value);
      when Simple_Record_D2_AA_Field_Name => User_Object.D2_AA := Get (Value);
      when Simple_Record_D4_Aa_Array_Field_Name => User_Object.D4_Aa_Array := Get (Value);
      when Simple_Record_D5_Enum_Field_Name => User_Object.D5_Enum := Get (Value);
      when Simple_Record_D6_My_Field_Name => User_Object.D6_My := Get (Value);
      when others =>         null;
      end case;
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
   --  Concrete_Taggd_Record
   --  --------------------------------------------------------------------
   --
   Concrete_Taggd_Record_Data2_Field_Name : constant String := "Data2";

   function Create (Val : Concrete_Taggd_Record) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do

         Set_Fields (Ret, Val);
      end return;
   end Create;

   function Get (Val : JSON_Value) return Concrete_Taggd_Record is
   begin
      return Ret : Concrete_Taggd_Record do
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
   --
   Concrete_Taggd_Record_With_Time_T_Field_Name : constant String := "T";

   function Create (Val : Concrete_Taggd_Record_With_Time) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Fields (Ret, Val);
      end return;
   end Create;

   function Get (Val : JSON_Value) return Concrete_Taggd_Record_With_Time is
   begin
      return Ret : Concrete_Taggd_Record_With_Time do
         Map_JSON_Object (Val, Map_JSON_Value'Access, Ret);
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Concrete_Taggd_Record_With_Time is
   begin
      return Concrete_Taggd_Record_With_Time'(Get (JSON_Value'(Get (Val, Field))));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Concrete_Taggd_Record_With_Time) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Map_JSON_Value (User_Object : in out Concrete_Taggd_Record_With_Time;
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

   --  -------------------------------------------------------------------------
   --    Record_With_Discriminatns
   --  -------------------------------------------------------------------------
   --
   Record_With_Discriminatns_D1_Field_Name   : constant String := "D1";
   Record_With_Discriminatns_D2_Field_Name   : constant String := "D2";
   Record_With_Discriminatns_Name_Field_Name : constant String := "Name";
   Record_With_Discriminatns_F_Field_Name    : constant String := "F";
   Record_With_Discriminatns_I_Field_Name    : constant String := "I";

   function Create (Val : Record_With_Discriminatns) return JSON_Value is
   begin
      return Ret : constant JSON_Value := Create_Object do
         Set_Fields (Ret, Val);
      end return;
   end Create;

   function Get (Val : JSON_Value) return Record_With_Discriminatns is
      D1  : Natural;
      D2  : Boolean;

   begin
      D1 := Get (JSON_Value'(Get (Val, Record_With_Discriminatns_D1_Field_Name)));
      D2 := Get (JSON_Value'(Get (Val, Record_With_Discriminatns_D2_Field_Name)));

      return Ret : Record_With_Discriminatns (D1, D2) do
         Map_JSON_Object (Val, Map_JSON_Value'Access, Ret);
      end return;
   end Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return Record_With_Discriminatns is
   begin
      return Record_With_Discriminatns'(Get (JSON_Value'(Get (Val, Field))));
   end Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : Record_With_Discriminatns) is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   procedure Map_JSON_Value (User_Object : in out Record_With_Discriminatns;
                             Name        : UTF8_String;
                             Value       : JSON_Value) is
   begin
      if Name = Record_With_Discriminatns_Name_Field_Name then
         User_Object.Name := Get (Value);
      elsif Name = Record_With_Discriminatns_F_Field_Name then
         User_Object.F := Get (Value);
      elsif Name = Record_With_Discriminatns_I_Field_Name then
         User_Object.I := Get (Value);
      else
         null;
      end if;
   end;

   procedure Set_Fields (Val : JSON_Value; Data : Record_With_Discriminatns) is
   begin
      Set_Field (Val, Record_With_Discriminatns_D1_Field_Name, Data.D1);
      Set_Field (Val, Record_With_Discriminatns_D2_Field_Name, Data.D2);
      Set_Field (Val, Record_With_Discriminatns_Name_Field_Name, Data.Name);
      case Data.D2 is
         when True =>
            Set_Field (Val, Record_With_Discriminatns_F_Field_Name, Data.F);
         when False =>
            Set_Field (Val, Record_With_Discriminatns_I_Field_Name, Data.I);
      end case;
   end;
end Simple.JSON_Golden;
