import xml.etree.ElementTree
from os.path import *
import os
import glob

###################################################################################################
function_spec_template="""
   ----------------------------------------------------
   --  %(typename)s
   ---------------------------------------------------
   
   function Create (Val : %(typename)s) return JSON_Value;
   
   procedure Populate (Val : %(typename)s; Target : in out JSON_Value);

   function Get (Val : GNATCOLL.JSON.JSON_Value) return %(typename)s;
   
   function Get (Val   : JSON_Value;
                 Field : UTF8_String) return %(typename)s;
   
   procedure Set_Field  (Val        : JSON_Value;
                         Field_Name : UTF8_String;
                         Field      : %(typename)s);
   
   procedure Map (Val : JSON_Value;
                  To  : in out %(typename)s);   

   procedure Map_Field (Name : UTF8_String; Value : JSON_Value; To : in out %(typename)s);
"""

copyright_template="""Copyright (C) %(dates), %(holder)"""
license_template="""------------------------------------------------------------------------------
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
spec_template="""%(license)swith GNATCOLL.JSON;
%(withs)s
package %(name)s is
   use GNATCOLL.JSON;
%(function_specs)s
end %(name)s;"""


with_template="""with %(name)s; pragma Warnings (Off, %(name)s);
"""
use_template="""   use %(name)s;
"""
populates_template="""      Set_Field (Target, "%(fieldname)s", Create (Val.%(fieldname)s));
"""
if_template="""if Name = "%(fieldname)s" then
         To.%(fieldname)s := Get (Value);
      els"""

function_bodies_template="""
   ----------------------------------------------------
   --  %(typename)s
   ----------------------------------------------------
   procedure Populate (Val : %(typename)s; Target : in out JSON_Value) is
   begin
%(parent_populate)s%(populates)s   end;

   ------------
   -- Create --
   ------------

   function Create (Val : %(typename)s) return JSON_Value is
   begin
      return Ret : JSON_Value := Create_Object do
         Populate (Val, Ret);
      end return;
   end Create;

   ---------
   -- Get --
   ---------

   function Get (Val : GNATCOLL.JSON.JSON_Value) return %(typename)s is
   begin
      return Ret : %(typename)s do
         Map (Val, Ret);
      end return;
   end Get;

   ---------
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

   ---------------
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

   ---------------
   -- Map_Field --
   ---------------
   procedure Map_Field (Name : UTF8_String; Value : JSON_Value; To : in out %(typename)s) is
   begin
      %(ifs)se  
         null;
      end if;
   end;
   
   ---------
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

body_template="""%(license)s%(withs)s
package body %(name)s is
%(uses)s   
%(function_bodies)s
end %(name)s;"""



###################################################################################################

class typeSpec:
    def __init__(self,name):
        self.fields=[]
        self.name=name
        self.is_absract=False
        
        pass
    
class GNATXML2Json:
    def __init__(self,xml_src,outfolder=".",replace=False):        
        self.tree = xml.etree.ElementTree.parse(xml_src)
        self.records=[]
        self.outfolder=outfolder
        if not exists(outfolder):
            os.makedirs(outfolder)
        self.parse()
        self.replace=replace
        
    def ada2file(self,name):
        return name.lower().replace(".","-")
    
    def getName(self,node):
        ret=node.findall("names_ql/defining_identifier")[0].attrib["def_name"]
        return ret
    
    def on_ordinary_type_declaration(self,_type):
        t = typeSpec(self.getName(_type))        
        for element in _type.findall(".//component_declaration"):
            t.fields.append(self.getName(element))
        self.records.append(t)
        
    def parse(self):
        self.name=self.tree.getroot().attrib["def_name"] + ".JSON"
        for _type in self.tree.findall(".//ordinary_type_declaration"):
            self.on_ordinary_type_declaration(_type)

    def write(self,path,image):
        if not exists(path) or self.replace:
            with open(path,"w") as outf:
                outf.write(image())
        
    def writeSpec(self):
        self.write(join(self.outfolder,self.ada2file(self.name))+ ".ads",
                   self.specImage);
            
            
    def specImage(self):
        function_specs=[]
        for t in self.records:
            function_specs.append(function_spec_template % {"typename": t.name})
        return spec_template % {"license": "",
                                "name": self.name,
                                "withs": "",
                                "function_specs": "".join(function_specs)}
            
    def writeBody(self):
        self.write(join(self.outfolder,self.ada2file(self.name))+ ".adb",
                   self.bodyImage);
    
    def bodyImage(self):
        function_bodies=[]
        withs=[];
        uses=[];
        paranets=self.name.split(".")[:-2]        
        while paranets:
            withs.append(with_template % {"name": (".".join(paranets))+".JSON"})
            uses.append(use_template %   {"name": (".".join(paranets))+".JSON"})
            paranets=paranets[:-1]
        for i in self.records:
            populates=[]
            ifs=[]
            for fieldname in i.fields:
                populates.append(populates_template % {"fieldname" : fieldname})
                ifs.append(if_template % {"fieldname" : fieldname})
            function_bodies.append(function_bodies_template % {"typename": i.name,
                                                               "ifs":      "".join(ifs),
                                                               "parent_populate": "",
                                                               "populates":  "".join(populates)})
            
        return body_template % {"license": "",
                                "name": self.name,
                                "withs": "".join(withs),
                                "uses": "".join(uses),
                                "function_bodies": "".join(function_bodies)}
    def generate(self):
        self.writeSpec()
        self.writeBody()

for i in glob.glob("xml/*.ads.xml"):
    p=GNATXML2Json(i,"json",True).generate()

