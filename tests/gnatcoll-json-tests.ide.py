#
import re
import gps_utils
from os.path import *
import GPS


def ada2filename(name):
    return name.lower().replace(".", "-")

REGISTRATIONS_TEMPLATE = """   Register_Routine (Test, %(name)s'Unrestricted_Access, "%(name)s");"""

REGISTER_TESTS_TEMPLATE = """--  begin read only
separate (%(package_name)s)
overriding procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
%(registrations)s
end Register_Tests;
--  end read only"""


matcher = re.compile("(" +
                     "procedure\s+(\w+)\s*\(\s*\w+\s*:\s*in\s*out\s*AUnit.Test_Cases.Test_Case'Class\)\s*is" +
                     "|" + "package\s*body\s*(\S+)\s*is" +
                     "|" + "(overriding\s+|)procedure\s+Register_Tests\s*\(\S+\s*:\s*in\s+out\s+Test_Case\)\s*is\s+separate\s*;" +
                     ")", re.I)


@gps_utils.hook("file_saved")
def on_file_saved(f):
    path = f.name()
    folder = dirname(path)
    testRoutines = []
    UnitName = None
    shall_generate = False

    if (splitext(path)[1] != ".adb") and ("test" not in path):
        return

    with open(path) as inf:
        for line in inf:
            matches = matcher.match(line.strip())
            if matches:
                if matches.group(2):
                    testRoutines.append(matches.group(2))
                elif matches.group(3):
                    UnitName = matches.group(3)
                elif matches.group(4):
                    shall_generate = True

    if shall_generate and UnitName and testRoutines:
        register_name = UnitName + ".Register_Tests"
        registrations = []
        for name in testRoutines:
            registrations.append(REGISTRATIONS_TEMPLATE % {"name": name})
        out_path = join(folder, ada2filename(register_name)) + ".adb"
        with open(out_path, "w") as outf:
            outf.write(REGISTER_TESTS_TEMPLATE % {"package_name":  UnitName,
                                                  "registrations": "\n".join(registrations)})
