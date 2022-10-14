# This is the file "gnatcoll-json.py" that is symlinked to differnet names
# So it will be loaded farr all projects.

# print "test"
import GPS
import gs_utils


class RunTest(GPS.Action):
    def __init__(self):
        gs_utils.make_interactive(self.on_click,
                                  toolbar="main",
                                  icon="gps-boardloading-flash-symbolic")
    def on_click(self):
        print("click")


class gnatcoll_json_builder_ide:
    def __init__(self):
        RunTest()


try:
    a = GPS.gnatcoll_json_builder_ide
except:
    GPS.gnatcoll_json_builder_ide = gnatcoll_json_builder_ide()
