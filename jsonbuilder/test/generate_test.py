import re

with open("src/simple-main.adb") as inf:
    buffer = inf.read()

matches=re.match(r"(^.*<begin>)(.*)(^ +-- *<end>.*)",buffer,re.M+re.S)

out_buffer=[matches.group(1)]
suffix=matches.group(3)
typenames=[]
calls=[]
for line in matches.group(1).split("\n"):
    matches=re.match(r" +V_\S+ +: *(constant|) *(\w+).+",line)
    if matches:
        typename=matches.group(2)
        if typename not in typenames:
            typenames.append(matches.group(2))
    matches=re.match(r" +(V_\S+) +: *(constant|) *(\w+).+",line)
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
""")

for name in calls:
    out_buffer.append("""   Test (Write (Create (%(name)s)), %(name)s);""" % {"name": name})
out_buffer.append(suffix)

with open("src/simple-main.adb","w") as outf:
    outf.write("\n".join(out_buffer))

