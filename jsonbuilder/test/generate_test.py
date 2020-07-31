#!/usr/bin/env python
import re
from os.path import *
import sys


def process(path):
    with open(path) as inf:
        buffer = inf.read()

    matches = re.match(r"(^.*<begin>)(.*)(^ +-- *<end>.*)", buffer, re.M+re.S)

    out_buffer = [matches.group(1)]
    suffix = matches.group(3)
    typenames = []
    calls = []
    for line in matches.group(1).split("\n"):
        matches = re.match(r" +V_\S+ +: *(constant|) *([\w\.]+).+", line)
        if matches:
            typename = matches.group(2)
            if typename not in typenames:
                typenames.append(matches.group(2))
        matches = re.match(r" +(V_\S+) +: *(constant|) *(\w+).+", line)
        if matches:
            calls.append(matches.group(1))
            
    for typename in typenames:
        out_buffer.append("""
       procedure Test (S : String; Data : %(name)s) is
          D : constant %(name)s := Get (Read (S));
       begin
          if D = Data then
             Put_Line ("OK %(name)s ");
          else
             Put_Line ("FAIL %(name)s ");
          end if;
       end Test;
       """ % {"name": typename})
        
    out_buffer.append("""begin
   Extra_Init;
""")

    for name in calls:
        out_buffer.append("""   Test (Write (Create (%(name)s)), %(name)s);""" % 
                          {"name": name})
    out_buffer.append(suffix)

    with open(path, "w") as outf:
        outf.write("\n".join(out_buffer))

        
def main(argv):
    for i in argv:
        if exists(i) and (splitext(i)[1] == ".adb"):
            process(i)

if __name__ == "__main__":
    main(sys.argv[1:])
