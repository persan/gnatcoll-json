with JSONSample.String_Maps.JSON;
with JSONSample.String_Vectors.JSON;

package body JSONSample.InterfaceTypes.JSON is
   use GNATCOLL.JSON;
   use JSONSample.String_Maps.JSON;
   use JSONSample.String_Vectors.JSON;
   use Ada.Strings.Unbounded;
   ------------------
   -- Map_Response --
   ------------------

   procedure Map_Response
     (User_Object : in out Response;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      null;
   end Map_Response;

   -----------------
   -- Map_Request --
   -----------------

   procedure Map_Request
     (User_Object : in out Request;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      --  Generated stub: replace with real body!
      --        pragma Compile_Time_Warning (Standard.True, "Map_Request unimplemented");
      raise Program_Error with "Unimplemented procedure Map_Request";
   end Map_Request;

   -----------------
   -- Map_Host_Id --
   -----------------

   procedure Map_Host_Id
     (User_Object : in out Host_Id;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      --  Generated stub: replace with real body!
      --        pragma Compile_Time_Warning (Standard.True, "Map_Host_Id unimplemented");
      raise Program_Error with "Unimplemented procedure Map_Host_Id";
   end Map_Host_Id;

   -------------------------------
   -- Map_Interupt_Message_Type --
   -------------------------------

   procedure Map_Interupt_Message_Type
     (User_Object : in out Interupt_Message_Type;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      --  Generated stub: replace with real body!
      --        pragma Compile_Time_Warning (Standard.True, "Map_Interupt_Message_Type unimplemented");
      raise Program_Error with "Unimplemented procedure Map_Interupt_Message_Type";
   end Map_Interupt_Message_Type;

   -----------------------
   -- Map_Progress_Type --
   -----------------------

   procedure Map_Progress_Type
     (User_Object : in out Progress_Type;
      Name        : UTF8_String;
      Value       : JSON_Value)
   is
   begin
      --  Generated stub: replace with real body!
      --        pragma Compile_Time_Warning (Standard.True, "Map_Progress_Type unimplemented");
      raise Program_Error with "Unimplemented procedure Map_Progress_Type";
   end Map_Progress_Type;

   --  ========================================================
   --                    Req_State_Type
   --  ========================================================
   package Req_State_Type_JSON is new GNATCOLL.JSON.Support.Enum_Generic
     (Req_State_Type,
      Suffix => "_Command");
   function Create (Val : Req_State_Type) return JSON_Value renames Req_State_Type_JSON.Create;
   function Get (Val : JSON_Value) return Req_State_Type renames Req_State_Type_JSON.Get;
   function Get (Val : JSON_Value; Name : UTF8_String ) return Req_State_Type renames Req_State_Type_JSON.Get;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Req_State_Type) renames Req_State_Type_JSON.Set_Field;


   --  ========================================================
   --                    Request
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Request) return JSON_Value is
   begin
      return Ret : JSON_Value := Create_Object do
         Set_Field (Ret, "Req_State", Create (Val.Req_State));
         if Val.Args /= null then
            Set_Field (Ret, "Args", Create (Val.Args.all));
         end if;
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Request is
   begin
      return Ret : Request do
         Map_Request(Val,Request)
         Ret.Req_State := Get (Val , "Req_State");
         Ret.Args := new String_Maps.Map'(Get (Val , "Args"));
      end return;
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Request)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   --  ========================================================
   --                    Host_Id
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Host_Id) return JSON_Value is
      Ret  : JSON_Value := Create_Object;
   begin
      --        return  Ret : JSON_Value := Create_Object do
      Set_Field (Ret, "Host", Create (Val.Host));
      Set_Field (Ret, "Action_Id", Create (Val.Action_Id));
      --        end Create;
      return Ret;
   end Create;
   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Host_Id is
   begin
      null;
      return Ret : Host_Id do
         Ret.Host := (Get (Val , "Host"));
         Ret.Action_Id := Get (Val , "Action_Id");
      end return;
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Host_Id)
   is
   begin
      null; --      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   --  ========================================================
   --                    Severity_Type
   --  ========================================================

   package Severity_Type_JSON is new GNATCOLL.JSON.Support.Enum_Generic (Severity_Type);
   function Create (Val : Severity_Type) return JSON_Value renames Severity_Type_JSON.Create;
   function Get (Val : JSON_Value) return Severity_Type renames Severity_Type_JSON.Get;
   function Get (Val : JSON_Value; Field_Name : UTF8_String ) return Severity_Type renames Severity_Type_JSON.Get;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Severity_Type) renames Severity_Type_JSON.Set_Field;

   --  ========================================================
   --                    State_Type
   --  ========================================================


   package State_Type_JSON is new GNATCOLL.JSON.Support.Enum_Generic (State_Type);
   function Create (Val : State_Type) return JSON_Value renames State_Type_JSON.Create;
   function Get (Val : JSON_Value) return State_Type renames State_Type_JSON.Get;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : State_Type) renames State_Type_JSON.Set_Field;


   --  ========================================================
   --                    Posix_Time
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Posix_Time) return JSON_Value is
   begin
      return Create (Long_Long_Integer (Val));
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Posix_Time is
   begin
      return Posix_Time (Long_Long_Integer'(Get (Val)));
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Posix_Time)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   --  ========================================================
   --                    Interupt_Message_Type
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Interupt_Message_Type) return JSON_Value is
      Ret : JSON_Value := Create_Object;
   begin
      Set_Field (Ret, "Text", Val.Text);
      Set_Field (Ret, "Severity", Val.Severity);
      return Ret;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Interupt_Message_Type is
   begin
      return Ret : Interupt_Message_Type do
         Ret.Text := Get (Val, "Text");
         Ret.Severity := Get (Val, "Severity");
      end return;
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Interupt_Message_Type)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   --  ========================================================
   --                    Progress_Type
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Progress_Type) return JSON_Value is
      Ret : JSON_Value := Create_Object;
   begin
      Set_Field (Ret, "Min", Val.Min);
      Set_Field (Ret, "Max", Val.Max);
      Set_Field (Ret, "Current", Val.Current);
      return Ret;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Progress_Type is
   begin
      return Ret : Progress_Type do
         Ret.Min := Get (Val, "Min");
         Ret.Max := Get (Val, "Max");
         Ret.Current := Get (Val, "Current");
      end return;
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Progress_Type)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

   --  ========================================================
   --                    Response
   --  ========================================================

   ------------
   -- Create --
   ------------

   function Create (Val : Response) return JSON_Value is
      Ret : JSON_Value := Create_Object;
   begin
      Set_Field (Ret, "From", Val.From);
      Set_Field (Ret, "Started", Val.Started);
      if Val.Completed /= null then
         Set_Field (Ret, "Completed", Val.Completed.all);
      end if;
      Set_Field (Ret, "Messages", Val.Messages);
      if Val.Interupt_Message /= null then
         Set_Field (Ret, "Interupt_Message", Val.Interupt_Message.all);
      end if;
      Set_Field (Ret, "State", Val.State);
      Set_Field (Ret, "CanStop", Val.CanStop);
      Set_Field (Ret, "Enabled", Val.Enabled);
      Set_Field (Ret, "Input_Args", Val.Input_Args);
      if Val.Progress /= null then
         Set_Field (Ret, "Progress", Val.Progress.all);
      end if;
      return Ret;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : JSON_Value) return Response is
   begin
      return Ret : Response do
         declare
            procedure Map (Name : UTF8_String; Value : JSON_Value)is
            begin
               if Name =  "From" then
                  Ret.From := Get (Value);
               elsif Name = "Started" then
                  Ret.Started := Get (Value);
               elsif Name = "Completed" then
                  Ret.Completed := new Posix_Time'(Get (Value));
               elsif Name = "Messages" then
                  Ret.Messages := Get (Value);
               elsif Name = "Interupt_Message" then
                  Ret.Interupt_Message := new Interupt_Message_Type'(Get (Value));
               elsif Name = "State" then
                  Ret.State := Get (Value);
               elsif Name = "CanStop" then
                  Ret.CanStop := Get (Value);
               elsif Name = "Enabled" then
                  Ret.Enabled := Get (Value);
               elsif Name = "Input_Args" then
                  Ret.Input_Args := Get (Value);
               elsif Name = "Progress" then
                  Ret.Progress := new Progress_Type'(Get (Value));
               end if;

            end;
         begin
            Map_JSON_Object (Val, Map'Access);
         end;
      end return;
   end Get;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Response)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

end JSONSample.InterfaceTypes.JSON;
