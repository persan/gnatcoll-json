with Ada.Strings.Unbounded;
with JSONSample.String_Maps;
with JSONSample.String_Vectors;
with Ada.Finalization;
package JSONSample.InterfaceTypes is


   type Req_State_Type is (Active_Command,
                           Inactive_Command,
                           Cancel_Command,
                           Abort_Command,
                           Acc_Command) with Default_Value => Active_Command;

   type Request is new Ada.Finalization.Controlled with  record
      Req_State : Req_State_Type;
      Args      : access String_Maps.Map with Annotate => (Optional);
   end record;

   procedure Adjust     (Object : in out Request);
   procedure Finalize   (Object : in out Request);

   type Host_Id is  new Ada.Finalization.Controlled with record
      Host      : Ada.Strings.Unbounded.Unbounded_String;
      Action_Id : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Severity_Type is (Ok, Warn, Error) with Default_Value => Ok;
   type State_Type is (Active, Inactive, Interupted) with Default_Value => Active;

   type Posix_Time is new Integer with Default_Value => 0;

   type Interupt_Message_Type  is record
      Text     : Ada.Strings.Unbounded.Unbounded_String;
      Severity : Severity_Type;
   end record;

   type Progress_Type is record
      Min     : Float := 0.0;
      Max     : Float := 0.0;
      Current : Float := 0.0;
   end record;

   type Response is new Ada.Finalization.Controlled with record
      From             : Host_Id;
      Started          : Posix_Time;
      Completed        : access Posix_Time with Annotate => (Optional);
      Messages         : String_Vectors.Vector;
      Interupt_Message : access Interupt_Message_Type with Annotate => (Optional);
      State            : State_Type;
      CanStop          : Boolean := False;
      Enabled          : Boolean := False;
      Input_Args       : Request;
      Progress         : access Progress_Type with Annotate => (Optional);
   end record;
   procedure Adjust     (Object : in out Response);
   procedure Finalize   (Object : in out Response);

   type Server is limited interface;
   function On_Request
     (Self : in out Server;
      Req  : Request'Class) return Response'Class is abstract;

end JSONSample.InterfaceTypes;
