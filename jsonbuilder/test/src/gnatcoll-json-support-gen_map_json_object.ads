generic
   type Mapped  (<>) is private;
procedure GNATCOLL.JSON.Support.Gen_Map_JSON_Object
  (Val         : JSON_Value;
   CB          : access procedure
     (User_Object : in out Mapped;
      Name        : UTF8_String;
      Value       : JSON_Value);
   User_Object : in out Mapped)
  with Pre => Val.Kind = JSON_Object_Type;
