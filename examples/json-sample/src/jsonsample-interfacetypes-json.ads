with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.JSON.Support.Enumeration_Generic;
package JSONSample.InterfaceTypes.JSON is

   procedure Map_Response (User_Object : in out Response;
                           Name        : UTF8_String;
                           Value       : JSON_Value);
   procedure Map_JSON_Object is new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Response);


   procedure Map_Request (User_Object : in out Request;
                          Name        : UTF8_String;
                          Value       : JSON_Value);
   procedure Map_JSON_Object is new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Request);

   procedure Map_Host_Id (User_Object : in out Host_Id;
                          Name        : UTF8_String;
                          Value       : JSON_Value);
   procedure Map_JSON_Object is new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Host_Id);

   procedure Map_Interupt_Message_Type (User_Object : in out Interupt_Message_Type;
                                        Name        : UTF8_String;
                                        Value       : JSON_Value);
   procedure Map_JSON_Object is new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Interupt_Message_Type);

   procedure Map_Progress_Type (User_Object : in out Progress_Type;
                                Name        : UTF8_String;
                                Value       : JSON_Value);
   procedure Map_JSON_Object is new GNATCOLL.JSON.Gen_Map_JSON_Object (Mapped => Progress_Type);

   function Create (Val : Req_State_Type) return JSON_Value;
   function Get (Val : JSON_Value) return Req_State_Type;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Req_State_Type);


   function Create (Val : Request) return JSON_Value;
   function Get (Val : JSON_Value) return Request;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Request);

   function Create (Val : Host_Id) return JSON_Value;
   function Get (Val : JSON_Value) return Host_Id;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Host_Id);


   function Create (Val : Severity_Type) return JSON_Value;
   function Get (Val : JSON_Value) return Severity_Type;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Severity_Type);

   function Create (Val : State_Type) return JSON_Value;
   function Get (Val : JSON_Value) return State_Type;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : State_Type);

   function Create (Val : Posix_Time) return JSON_Value;
   function Get (Val : JSON_Value) return Posix_Time;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Posix_Time);

   function Create (Val : Interupt_Message_Type) return JSON_Value;
   function Get (Val : JSON_Value) return Interupt_Message_Type;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Interupt_Message_Type);

   function Create (Val : Progress_Type) return JSON_Value;
   function Get (Val : JSON_Value) return Progress_Type;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Progress_Type);

   function Create (Val : Response) return JSON_Value;
   function Get (Val : JSON_Value) return Response;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Response);

end JSONSample.InterfaceTypes.JSON;
