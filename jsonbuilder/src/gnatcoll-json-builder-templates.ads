with GNATCOLL.JSON.Builder.Templates_Helpers;
package  GNATCOLL.JSON.Builder.Templates is

   function SIGNED_INT_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function ARRAY_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function ENUM_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function MOD_INT_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function DERIVED_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function RECORD_TYPE_Template is new Templates_Helpers.Template_Path_Generic;
   function INTERFACE_TYPE_Template is new Templates_Helpers.Template_Path_Generic;

   function SIGNED_INT_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function ARRAY_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function ENUM_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function MOD_INT_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function DERIVED_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function RECORD_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;
   function INTERFACE_TYPE_With_Template is new Templates_Helpers.Template_Path_Generic;

   function Spec_Template is new Templates_Helpers.Template_Path_Generic;
   function Body_Template is new Templates_Helpers.Template_Path_Generic;

end GNATCOLL.JSON.Builder.Templates;
