import os
import glob 
from os.path import *

s1="""with GNATCOLL.JSON.Support.Test.Integer_%(Name)s.JSON.Tests;
with GNATCOLL.JSON.Support.Test.Integer_%(Name)s.Simple_JSON.Tests;"""

s2="""   Test_%(Name)s : aliased GNATCOLL.JSON.Support.Test.Integer_%(Name)s.JSON.Tests.Test_Case;
   Test_%(Name)s_Simple : aliased GNATCOLL.JSON.Support.Test.Integer_%(Name)s.Simple_JSON.Tests.Test_Case;"""
s3="""      Add_Test (Result'Access, Test_%(Name)s'Access);
      Add_Test (Result'Access, Test_%(Name)s_Simple'Access);"""

spec="""------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
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
------------------------------------------------------------------------------

with AUnit.Test_Suites;
package GNATCOLL.JSON.Support.Test.%(type_name)s_Suites is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end GNATCOLL.JSON.Support.Test.%(type_name)s_Suites;"""

body="""------------------------------------------------------------------------------
--                             G N A T C O L L . J S O N                    --
--                                                                          --
--    Copyright (C) 2016-2025, Per Sandberg <per.s.sandberg@bahnhof.se>     --
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
------------------------------------------------------------------------------


%(withs)s

package body GNATCOLL.JSON.Support.Test.%(type_name)s_Suites is

   use AUnit.Test_Suites;

   --  Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:

%(cases)s;
   
   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
%(registers)s
      return Result'Access;
   end Suite;
end GNATCOLL.JSON.Support.Test.%(type_name)s_Suites;
"""
def read(path):
    with file(path) as inf:
        return inf.read()
    
def write(path,data):
    path = abspath(path)
    if not exists(dirname(path)):
        os.makedirs(dirname(path))
    with file(path,"w") as outf:
        outf.write(data)

def main(src,tgt,names,Type_Name):
    withs=[]
    cases=[]
    registers=[]
    for name in names:
        withs.append(s1 % {"Name" : name})
        cases.append(s2 % {"Name" : name})
        registers.append(s3 % {"Name" : name})
    write("gnatcoll-json-support-test-%s_suites.ads" % Type_Name.lower(), spec % {"type_name" : Type_Name,
                                                                                  "withs"     : "\n".join(withs),
                                                                                  "cases"     : "\n".join(cases),
                                                                                  "registers" : "\n".join(registers)})
    write("gnatcoll-json-support-test-%s_suites.adb" % Type_Name.lower(), body % {"type_name" : Type_Name,
                                                                                  "withs"     : "\n".join(withs),
                                                                                  "cases"     : "\n".join(cases),
                                                                                  "registers" : "\n".join(registers)})
    for name in names:
        for src_file_name in glob.glob(join(src,"*.in")):
            tgt_file_name=join(tgt,splitext(basename(src_file_name))[0].replace("@_name_@",name.lower()))
            write(tgt_file_name,read(src_file_name).replace("@_Name_@",name).replace("@_Type_Name_@",Type_Name))
            
    
if __name__ == "__main__":
    main(".","_maps",["Bounded_Hashed_Maps",
                      "Bounded_Ordered_Maps",
                      "Indefinite_Hashed_Maps",
                      "Indefinite_Ordered_Maps",
                      "Hashed_Maps",
                      "Ordered_Maps"],"Map")
        
    main(".","_Sets",["Bounded_Hashed_Sets",
                      "Bounded_Ordered_Sets",
                      "Indefinite_Hashed_Sets",
                      "Indefinite_Ordered_Sets",
                      "Hashed_Sets",
                      "Ordered_Sets"],"Set")
    

