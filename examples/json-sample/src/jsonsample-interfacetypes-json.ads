with GNATCOLL.JSON; use GNATCOLL.JSON;
package JsonSample.InterfaceTypes.JSON is

   procedure Map_Response (User_Object : in out Response;
                           Name        : UTF8_String;
                           Value       : JS ON_Value);
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



   function Create (Val : Boolean) return JSON_Value;
   function Get (Val : JSON_Value) return Boolean;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String);




   function Create (Val : Boolean) return JSON_Value;
   function Get (Val : JSON_Value) return Boolean;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String);


   function Create (Val : Boolean) return JSON_Value;
   function Get (Val : JSON_Value) return Boolean;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String);

   function Create (Val : Boolean) return JSON_Value;
   function Get (Val : JSON_Value) return Boolean;
   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String);

end JsonSample.InterfaceTypes.JSON;
