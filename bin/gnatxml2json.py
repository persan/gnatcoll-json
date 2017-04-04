#!/usr/bin/env python
import xml.etree.ElementTree
from os.path import *
import os
import glob
import sys
import re

###############################################################################
function_spec_header_template = """   ----------------------------------------------------
   --  %(typename)s
   ---------------------------------------------------

"""
function_spec_map_template = """   procedure Map (Val : JSON_Value;
                  To  : in out %(typename)s);

"""
function_spec_mapfield_template = """   procedure Map_Field (Name : UTF8_String; Value : JSON_Value; To : in out %(typename)s);
"""
function_spec_setfield_template = """   procedure Set_Field  (Val        : JSON_Value;
                         Field_Name : UTF8_String;
                         Field      : %(typename)s);

"""
function_spec_getfield_template = """   function Get (Val   : JSON_Value;
                 Field : UTF8_String) return %(ty
                                               pename)s;

"""
function_spec_get_template = """   function Get (Val : GNATCOLL.JSON.JSON_Value) return %(typename)s;

"""
function_spec_create_template = """   function Create (Val : %(typename)s) return JSON_Value;

"""
function_spec_populate_template = """   procedure Populate (Val : %(typename)s; Target : in out JSON_Value);

"""

unconstrained_array_type_function_spec_template = """   ----------------------------------------------------
   --  %(typename)s
   ---------------------------------------------------

   package %(typename)s_Impl is new GNATCOLL.JSON.Support.Arrays_Generic
     ( %(indextype)s, %(elementtype)s, %(typename)s,
      Create, Get, Create, Get);

   function Create (Val : %(typename)s) return JSON_Value renames %(typename)s_Impl.Create;

   function Get (Val : JSON_Value) return %(typename)s renames %(typename)s_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return %(typename)s renames %(typename)s_Impl.Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(typename)s) renames %(typename)s_Impl.Set_Field;

"""


enumeration_type_function_spec_template="""   ----------------------------------------------------
   --  %(typename)s
   ---------------------------------------------------

   package %(typename)s_Impl is new GNATCOLL.JSON.Support.Enumeration_Generic (%(typename)s);

   function Create (Val : %(typename)s) return JSON_Value renames %(typename)s_Impl.Create;

   function Get (Val : JSON_Value) return %(typename)s renames %(typename)s_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return %(typename)s renames %(typename)s_Impl.Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(typename)s) renames %(typename)s_Impl.Set_Field;

"""
signed_integer_type_function_spec_template = """   ----------------------------------------------------
   --  %(typename)s
   ---------------------------------------------------

   package %(typename)s_Impl is new GNATCOLL.JSON.Support.Integer_Generic (%(typename)s);

   function Create (Val : %(typename)s) return JSON_Value renames %(typename)s_Impl.Create;

   function Get (Val : JSON_Value) return %(typename)s renames %(typename)s_Impl.Get;

   function Get (Val : JSON_Value; Field : UTF8_String) return %(typename)s renames %(typename)s_Impl.Get;

   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : %(typename)s) renames %(typename)s_Impl.Set_Field;

"""

copyright_template = """Copyright (C) %(dates), %(holder)"""
license_template = """------------------------------------------------------------------------------
%(name)s
--                                                                          --
%(copyright)s
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-------------------
"""
spec_template = """%(license)swith GNATCOLL.JSON;
%(withs)s
package %(name)s is
   use GNATCOLL.JSON;
%(function_specs)s
end %(name)s;"""


with_template = """with %(name)s; pragma Warnings (Off, %(name)s);
"""
use_template = """   use %(name)s;
"""
populates_template = """      Set_Field (Target, "%(fieldname)s", Create (Val.%(fieldname)s));
"""
if_template = """if Name = "%(fieldname)s" then
         To.%(fieldname)s := Get (Value);
      els"""

function_body_header_template = """   ----------------------------------------------------
   --  %(typename)s
   ----------------------------------------------------
"""
function_body_populate_template = """   --------------
   -- Populate --
   --------------

   procedure Populate (Val : %(typename)s; Target : in out JSON_Value) is
   begin
%(parent_populate)s%(populates)s   end;

"""
function_body_create_template = """   ------------
   -- Create --
   ------------

   function Create (Val : %(typename)s) return JSON_Value is
   begin
      return Ret : JSON_Value := Create_Object do
         Populate (Val, Ret);
      end return;
   end Create;

"""
function_body_get_template = """   ---------
   -- Get --
   ---------

   function Get (Val : GNATCOLL.JSON.JSON_Value) return %(typename)s is
   begin
      return Ret : %(typename)s do
         Map (Val, Ret);
      end return;
   end Get;

"""
function_body_getfield_template = """   ---------
   -- Get --
   ---------

   function Get
     (Val   : JSON_Value;
      Field : UTF8_String)
      return %(typename)s
   is
   begin
      return Get (JSON_Value'(Get (Val, Field)));
   end Get;

"""
function_body_setfield_template = """   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : %(typename)s)
   is
   begin
      Set_Field (Val, Field_Name, Create (Field));
   end Set_Field;

"""
function_body_mapfield_template = """   ---------------
   -- Map_Field --
   ---------------
   procedure Map_Field (Name : UTF8_String; Value : JSON_Value; To : in out %(typename)s) is
   begin
      %(ifs)se
         null;
      end if;
   end;

"""
function_body_map_template = """   ---------
   -- Map --
   ---------
   procedure Map
     (Val : JSON_Value;
      To  : in out %(typename)s)
   is
      procedure Process (Name : UTF8_String; Value : JSON_Value);
      procedure Process (Name : UTF8_String; Value : JSON_Value) is
      begin
         Map_Field (Name, Value, To);
      end;
   begin
      Map_JSON_Object (Val, Process'Access);
   end Map;

"""
functions_template = """
%(header)s
%(populate)s%(create)s%(get)s%(getfield)s%(setfield)s%(mapfield)s%(map)s"""

body_template = """%(license)s%(withs)s
package body %(name)s is

   function Get (Val : JSON_Value) return Long_Float renames GNATCOLL.JSON.Get_Long_Float;

   %(uses)s
%(function_bodies)s
end %(name)s;"""


###############################################################################


class typeSpec:
    def __init__(self, name):
        self.fields = []
        self.name = name
        self.is_abstract = False
        self.is_interface = False
        self.is_signed_integer = False
        self.is_unconstrained_array = False
        self.is_enumeration = False
        self.indextype = ""
        self.elementtype = ""

    def __str__(self):
        return ("%s (is_abstract => %s ,is_interface => %s , fields => %s) " %
                (self.name,
                 self.is_abstract,
                 self.is_interface,
                 ", ".join(self.fields)))


class GNATXML2Json:

    def __init__(self, xml_src, outfolder=".", replace=False):
        self.tree = xml.etree.ElementTree.parse(xml_src)
        self.records = []
        self.imports = []
        self.outfolder = outfolder
        self.ignored_packages = {"Ada": 0,
                                 "Ada.Calendar.Conversions": 0}
        self.replace = replace

        if not exists(outfolder):
            os.makedirs(outfolder)
        self.parse()

    def ada2file(self, name):
        return name.lower().replace(".", "-")

    def getName(self, node):
        ret = node.findall("names_ql/defining_identifier")[0].attrib["def_name"]
        return ret

    def append_with(self, name):
        if name in self.ignored_packages:
            return
        if name not in self.imports:
            self.imports.append(name)

    def on_with_clause(self, node):
        t = []
        for identifier in node.findall(".//identifier"):
            t.append(identifier.attrib["ref_name"])
            self.append_with(".".join(t))

    def on_ordinary_type_declaration(self, _type):

        t = typeSpec(self.getName(_type))
        if (_type.findall("./type_declaration_view_q/limited_interface") +
            _type.findall("./type_declaration_view_q/ordinary_interface") +
            _type.findall("./type_declaration_view_q/synchronized_interface")):
                t.is_interface = True

        elif _type.findall("./type_declaration_view_q/signed_integer_type_definition"):
            t.is_signed_integer = True

        elif _type.findall("./type_declaration_view_q/enumeration_type_definition"):
            t.is_enumeration = True

        elif _type.findall("./type_declaration_view_q/unconstrained_array_definition"):
            t.is_unconstrained_array = True
            __type = _type.findall("./type_declaration_view_q/unconstrained_array_definition")[0]
            t.indextype = __type.findall("./index_subtype_definitions_ql/identifier")[0].attrib["ref_name"]
            t.elementtype = __type.findall("./array_component_definition_q/component_definition/component_definition_view_q/subtype_indication/subtype_mark_q/identifier")[0].attrib["ref_name"]

        elif _type.findall("./type_declaration_view_q/tagged_record_type_definition/has_abstract_q/abstract"):
            t.is_abstract = True

        for element in _type.findall(".//component_declaration"):
            t.fields.append(self.getName(element))
        self.records.append(t)

    def parse(self):
        self.name = self.tree.getroot().attrib["def_name"] + ".JSON"

        for with_clause in self.tree.findall(".//with_clause"):
            self.on_with_clause(with_clause)

        for _type in self.tree.findall(".//ordinary_type_declaration"):
            self.on_ordinary_type_declaration(_type)

    def write(self, path, image):
        if not exists(path) or self.replace:
            with open(path, "w") as outf:
                outf.write(image())

    def writeSpec(self):
        self.write(join(self.outfolder, self.ada2file(self.name)) + ".ads",
                   self.specImage)

    def specImage(self):
        function_specs = []
        contains_signed_integer_types = False
        contains_unconstrained_arrays = False
        contains_enumerations = False
        withs = []
        for i in self.records:
            if i.is_interface:
                function_spec_template = ""

            elif i.is_signed_integer:
                function_spec_template = signed_integer_type_function_spec_template
                contains_signed_integer_types = True
            elif i.is_enumeration:
                function_spec_template = enumeration_type_function_spec_template
                contains_enumerations = True
            elif i.is_unconstrained_array:
                contains_unconstrained_arrays = True
                function_spec_template = unconstrained_array_type_function_spec_template % {"typename": i.name,
                                                                                            "indextype": i.indextype,
                                                                                            "elementtype": i.elementtype}
            elif i.is_abstract:
                function_spec_template = functions_template % {"header":   function_spec_header_template,
                                                               "populate": function_spec_populate_template,
                                                               "create":   "",
                                                               "get":      "",
                                                               "getfield": "",
                                                               "setfield": "",
                                                               "mapfield": function_spec_mapfield_template,
                                                               "map":      ""}
            else:
                function_spec_template = functions_template % {"header":   function_spec_header_template,
                                                               "populate": function_spec_populate_template,
                                                               "create":   function_spec_create_template,
                                                               "get":      function_spec_get_template,
                                                               "getfield": function_spec_getfield_template,
                                                               "setfield": function_spec_setfield_template,
                                                               "mapfield": function_spec_mapfield_template,
                                                               "map":      function_spec_map_template}

            function_specs.append(function_spec_template % {"typename": i.name})

        if contains_signed_integer_types:
            withs.append("with GNATCOLL.JSON.Support.Integer_Generic;\n")
        if contains_unconstrained_arrays:
            withs.append("with GNATCOLL.JSON.Support.Arrays_Generic;\n")
        if contains_enumerations:
            withs.append("with GNATCOLL.JSON.Support.Enumeration_Generic;\n")
        return spec_template % {"license": "",
                                "name": self.name,
                                "withs": "".join(withs),
                                "function_specs": "".join(function_specs)}

    def writeBody(self):
        self.write(join(self.outfolder, self.ada2file(self.name)) + ".adb",
                   self.bodyImage)

    def bodyImage(self):
        function_bodies = []
        function_bodies_template = ""
        withs = []
        uses = []
        paranets = self.name.split(".")[:-2]
        while paranets:
            withs.append(with_template %
                         {"name": (".".join(paranets))+".JSON"})
            uses.append(use_template %
                        {"name": (".".join(paranets))+".JSON"})
            paranets = paranets[:-1]
        for i in self.records:
            populates = []
            ifs = []
            if i.is_interface:
                function_bodies_template = ""
            elif i.is_signed_integer:
                function_bodies_template = ""
            elif i.is_unconstrained_array:
                function_bodies_template = ""
            elif i.is_enumeration:
                function_bodies_template = ""
            elif i.is_abstract:
                function_bodies_template = \
                    (functions_template %
                        {"header":   function_body_header_template,
                         "populate": function_body_populate_template,
                         "create":   "",
                         "get":      "",
                         "getfield": "",
                         "setfield": "",
                         "mapfield": function_body_mapfield_template,
                         "map":      ""})
            else:
                function_bodies_template = \
                    (functions_template %
                        {"header":   function_body_header_template,
                         "populate": function_body_populate_template,
                         "create":   function_body_create_template,
                         "get":      function_body_get_template,
                         "getfield": function_body_getfield_template,
                         "setfield": function_body_setfield_template,
                         "mapfield": function_body_mapfield_template,
                         "map":      function_body_map_template})

            for fieldname in i.fields:
                populates.append(populates_template % {"fieldname": fieldname})
                ifs.append(if_template % {"fieldname": fieldname})

            function_bodies.append(function_bodies_template %
                                   {"typename": i.name,
                                    "ifs":      "".join(ifs),
                                    "parent_populate": "",
                                    "populates":  "".join(populates)})
        for i in self.imports:
            if re.match(r"^(Ada|GNAT\.).*", i):
                withs.append("with GNATCOLL.JSON.Support.%s;\n" % i)
            else:
                withs.append("with %s.JSON;\n" % i)

        for i in self.imports:
            if re.match(r"^(Ada|GNAT\.).*", i):
                uses.append("use GNATCOLL.JSON.Support.%s;\n" % i)
            else:
                uses.append("   use %s.JSON;\n" % i)

        return body_template % {"license": "",
                                "name": self.name,
                                "withs": "".join(withs),
                                "uses": "".join(uses),
                                "function_bodies": "".join(function_bodies)}

    def generate(self):
        self.writeSpec()
        self.writeBody()


def main(argv):
    for i in argv:
        p = GNATXML2Json(i, ".", True)
        p.generate()


main(sys.argv[1:])
