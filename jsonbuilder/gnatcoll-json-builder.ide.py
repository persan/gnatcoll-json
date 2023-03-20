# This is the file "gnatcoll-json.py" that is symlinked to differnet names
# So it will be loaded farr all projects.

# print "test"
import GPS
import gs_utils
from os.path import *


class Console_Process(GPS.Console, GPS.Process):
    def on_output(self, matched, unmatched):
        self.write(unmatched + matched)

    def on_input(self, input):
        self.send(input)

    def on_exit(self, status, unmatched_output):
        self.write("\n-------------------------------------------------\n")

    def on_destroy(self):
        self.kill()  # Will call on_exit

    def __init__(self, command):
        GPS.Console.__init__(
            self,  # command[0],
            on_input=Console_Process.on_input,
            on_destroy=Console_Process.on_destroy,
            force=True)
        self.write("\n\n%s\n" % " ".join(command))
        GPS.Process.__init__(
            self, command, ".+",
            on_exit=Console_Process.on_exit,
            on_match=Console_Process.on_output)


class RunTest(GPS.Action):
    def __init__(self):
        gs_utils.make_interactive(self.on_click,
                                  toolbar="main",
                                  icon="gps-boardloading-flash-symbolic")

    def on_click(self):
        # NDDSHOME = os.getenv("NDDSHOME", "/usr/dds")
        Exec_Dir = GPS.Project.root().get_attribute_as_string("Exec_Dir")
        Project_Dir = GPS.Project.root().get_attribute_as_string("Project_Dir")
        Console_Process([join(Exec_Dir, "json-builder"),
                         "-o", join(Project_Dir, "."),
                         "-P", GPS.Project.root().file().name(),
                          GPS.File("simple.ads").name()])
    #                      "-P", join(NDDSHOME, "lib/gnat/dds-ada.gpr"),
    #                         join(NDDSHOME, "include/ndds/dds_ada/dds.ads")])


class gnatcoll_json_builder_ide:
    def __init__(self):
        RunTest()


try:
    a = GPS.gnatcoll_json_builder_ide
except:
    GPS.gnatcoll_json_builder_ide = gnatcoll_json_builder_ide()
