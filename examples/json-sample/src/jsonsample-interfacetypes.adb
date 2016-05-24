with Ada.Unchecked_Deallocation;

package body JsonSample.InterfaceTypes is
   type String_Maps_Map_Access is access all String_Maps.Map;
   type Interupt_Message_Type_Access is access all Interupt_Message_Type;
   type Posix_Time_Access is access all Posix_Time;
   type Progress_Type_Access is access all Progress_Type;

   procedure Free is new Ada.Unchecked_Deallocation (String_Maps.Map, String_Maps_Map_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Interupt_Message_Type, Interupt_Message_Type_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Posix_Time, Posix_Time_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Progress_Type, Progress_Type_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Request) is
   begin
      if Object.Args /= null then
         Object.Args := new String_Maps.Map'(Object.Args.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Request) is
   begin
      if Object.Args /= null then
         Free (Object.Args);
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Response) is
   begin
      if Object.Completed /= null then
         Object.Completed := new Posix_Time'(Object.Completed.all);
      end if;
      if Object.Interupt_Message /= null then
         Object.Interupt_Message := new Interupt_Message_Type'(Object.Interupt_Message.all);
      end if;
      if Object.Progress /= null then
         Object.Progress := new Progress_Type'(Object.Progress.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Response) is
   begin
      if Object.Completed /= null then
         Free (Object.Completed);
      end if;
      if Object.Interupt_Message /= null then
         Free (Object.Interupt_Message);
      end if;
      if Object.Progress /= null then
         Free (Object.Progress);
      end if;
   end Finalize;

end JsonSample.InterfaceTypes;
