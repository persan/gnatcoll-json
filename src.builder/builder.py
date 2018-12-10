omport os


Simple_Type_Spec = """
   package %(impl_name)s is new GNATCOLL.JSON.Support.%(support_name)s (" & type_name & ");" & ASCII.LF);
   function Create (Val : %(type_name)s) return JSON_Value renames %(impl_name)s.Create;" & ASCII.LF);
   function Get (Val : JSON_Value) return %(type_name)s renames %(impl_name)s.Get;" & ASCII.LF);
   function Get (Val : JSON_Value; Field : UTF8_String) return %(type_name)s renames %(impl_name)s.Get;" & ASCII.LF);
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(type_name)s) renames %(impl_name)s.Set_Field;

"""

Array_Template = """
   package %(impl_name)s is new GNATCOLL.JSON.Support.Arrays_Generic (%(index_type)s, %(element_type)s, %(array_type)s);
   function Create (Val : %(array_type)s) return JSON_Value renames %(impl_name)s.Create;
   function Get (Val : JSON_Value) return %(array_type)s renames %(array_type)s.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return %(array_type)s renames %(impl_name)s.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(array_type)s) renames %(impl_name)s.Set_Field;

"""


Simple_Array_Template = """
   package " & Impl_Name & " is new GNATCOLL.JSON.Support.Simple_Arrays_Generic (%(index_type)s, %(element_type)s, %(array_type)s);
   function Create (Val : %(array_type)s) return JSON_Value renames %(impl_name)s.Create;
   function Get (Val : JSON_Value) return %(array_type)s renames %(array_type)s.Get;
   function Get (Val : JSON_Value; Field : UTF8_String) return %(array_type)s renames %(impl_name)s.Get;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(array_type)s) renames %(impl_name)s.Set_Field;

"""


Tagged_Type_Spec_Template"""
   procedure Update(Val : JSON_Value; Fields  : in out %(type_name)s);
   function Create (Val : %(type_name)s) return JSON_Value;
   function Get (Val : JSON_Value) return %(type_name)s;
   function Get (Val : JSON_Value; Field : UTF8_String) return %(type_name)s;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(type_name)s);
"""

Tagged_Type_Body_Template"""
   function Create (Val : %(type_name)s) return JSON_Value;
   function Get (Val : JSON_Value) return %(type_name)s;
   function Get (Val : JSON_Value; Field : UTF8_String) return %(type_name)s;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(type_name)s);
"""

def Float_Type_Spec(Type_Name, outf):
    outf.write(Simple_Type_Spec % {"type_name": Type_Name,
                                   "impl_name": Type_Name + "_Impl",
                                   "support_name": "Float_Generic"})


def Enumeration_Type_Spec(Type_Name, outf):
    outf.write(Simple_Type_Spec % {"type_name": Type_Name,
                                   "impl_name": Type_Name + "_Impl",
                                   "support_name": "Enumeration_Generic"})


def Modular_Type_Spec(Type_Name, outf):
    outf.write(Simple_Type_Spec % {"type_name": Type_Name,
                                   "impl_name": Type_Name + "_Impl",
                                   "support_name": "Modular_Generic"})


def Fixed_Type_Spec(Type_Name, outf):
    outf.write(Simple_Type_Spec % {"type_name": Type_Name,
                                   "impl_name": Type_Name + "_Impl",
                                   "support_name": "Fixed_Generic"})


def Array_Type_Spec(Index_Type,  Element_Type,  Array_Type,   Outf):
    outf.write(Array_Template % {"index_type": Index_Type,
                                 "element_type": Element_Type,
                                 "array_type": Array_Type})


def Simple_Array_Type_Spec(Index_Type,  Element_Type,  Array_Type, Outf):
    outf.write(Simple_Array_Template % {"index_type": Index_Type,
                                        "element_type": Element_Type,
                                        "array_type": Array_Type})


def Tagged_Type_Spec(Type_Name, Parent, Discriminants, Fields, Outf):
    outf.write(Tagged_Type_Spec_Template )


def Tagged_Type_Body(Type_Name, Parent, Discriminants, Fields, Outf):

    outf.write("""       procedure Update(Val : JSON_Value; Fields  : in out %(type_name)s) is
       begin""" % {"type_name": Type_Name})

    outf.write("""       end Update;
""")
    outf.write("""       end Update;
""")
