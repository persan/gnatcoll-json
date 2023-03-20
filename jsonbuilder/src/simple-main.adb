with Ada.Text_IO; use Ada.Text_IO;

with Simple.JSON_Golden; use Simple.JSON_Golden;
-- with Simple.JSON; use Simple.JSON;

with GNATCOLL.JSON; use  GNATCOLL.JSON;

procedure Simple.Main is

   V_AA                                : constant AA := 1;
   V_Aa_Array                          : constant Aa_Array (1 .. 4) := (others => 4);
   V_Enum                              : constant Enum := DD;
   V_My_Mod                            : constant My_Mod := 10;
   V_My                                : constant My := 88;
   V_Simple_Record                     : constant Simple_Record := (2, 1.0, 1, (1, 2, 3, 4, 5, 6, 7, 8, 9, 1), A, 22);
   V_Concrete_Taggd_Record             : constant Concrete_Taggd_Record := Concrete_Taggd_Record'(20, 30);
   V_Concrete_Taggd_Record_With_Time   : constant Concrete_Taggd_Record_With_Time := Concrete_Taggd_Record_With_Time'(2, 3, Ada.Calendar.Time_Of (2021, 02, 11, 11.0));
   V_Record_With_Discriminatns_2_False : constant Record_With_Discriminatns (2, False) := (2, False, (others => '!'), 333);
   V_Record_With_Discriminatns_3_True  : constant Record_With_Discriminatns (3, True) := (3, True, (others => '!'), 1.22);
   V_My_Vectors                        : My_Vectors.Vector;

   procedure Extra_Init  is
   begin
      V_My_Vectors.Append("a1");
      V_My_Vectors.Append("a 2");
      V_My_Vectors.Append("a  3");
   end Extra_Init;

   -- use type My_Vectors.Vector;

       procedure Test (S : String; Data : AA) is
          D : constant AA := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK AA ");
          else
             Put_Line ("FAIL AA ");
          end if;
       end Test;


       procedure Test (S : String; Data : Aa_Array) is
          D : constant Aa_Array := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Aa_Array ");
          else
             Put_Line ("FAIL Aa_Array ");
          end if;
       end Test;


       procedure Test (S : String; Data : Enum) is
          D : constant Enum := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Enum ");
          else
             Put_Line ("FAIL Enum ");
          end if;
       end Test;


       procedure Test (S : String; Data : My_Mod) is
          D : constant My_Mod := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK My_Mod ");
          else
             Put_Line ("FAIL My_Mod ");
          end if;
       end Test;


       procedure Test (S : String; Data : My) is
          D : constant My := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK My ");
          else
             Put_Line ("FAIL My ");
          end if;
       end Test;


       procedure Test (S : String; Data : Simple_Record) is
          D : constant Simple_Record := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Simple_Record ");
          else
             Put_Line ("FAIL Simple_Record ");
          end if;
       end Test;


       procedure Test (S : String; Data : Concrete_Taggd_Record) is
          D : constant Concrete_Taggd_Record := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Concrete_Taggd_Record ");
          else
             Put_Line ("FAIL Concrete_Taggd_Record ");
          end if;
       end Test;


       procedure Test (S : String; Data : Concrete_Taggd_Record_With_Time) is
          D : constant Concrete_Taggd_Record_With_Time := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Concrete_Taggd_Record_With_Time ");
          else
             Put_Line ("FAIL Concrete_Taggd_Record_With_Time ");
          end if;
       end Test;


       procedure Test (S : String; Data : Record_With_Discriminatns) is
          D : constant Record_With_Discriminatns := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK Record_With_Discriminatns ");
          else
             Put_Line ("FAIL Record_With_Discriminatns ");
          end if;
       end Test;


       --  procedure Test (S : String; Data : My_Vectors.Vector) is
       --     D : constant My_Vectors.Vector := Get (Read (S));
       --  begin
       --     if D = Data then
       --        Put_Line ("OK My_Vectors.Vector ");
       --     else
       --        Put_Line ("FAIL My_Vectors.Vector ");
       --     end if;
       --  end Test;

begin
   Extra_Init;

   Test (Write (Create (V_AA)), V_AA);
   Test (Write (Create (V_Aa_Array)), V_Aa_Array);
   Test (Write (Create (V_Enum)), V_Enum);
   Test (Write (Create (V_My_Mod)), V_My_Mod);
   Test (Write (Create (V_My)), V_My);
   Test (Write (Create (V_Simple_Record)), V_Simple_Record);
   Test (Write (Create (V_Concrete_Taggd_Record)), V_Concrete_Taggd_Record);
   Test (Write (Create (V_Concrete_Taggd_Record_With_Time)), V_Concrete_Taggd_Record_With_Time);
   Test (Write (Create (V_Record_With_Discriminatns_2_False)), V_Record_With_Discriminatns_2_False);
   Test (Write (Create (V_Record_With_Discriminatns_3_True)), V_Record_With_Discriminatns_3_True);
   --    Test (Write (Create (V_My_Vectors)), V_My_Vectors);
end Simple.Main;
