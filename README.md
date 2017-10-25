This library provides a set of utilities for serializing/deseriallizing
* Ada Standard containers and most of the Types defind in the package heracies
    Ada, GNAT, System, Interfaces
   to/from JSON
* The final target is to add a code generator that would generate a
```Ada
package ${package-name}.JSON is

   -- With at least the folowing methods for each type in the package ${package-name}
   function Create (Val : TypeName) return JSON_Array;
   function Get (Val : JSON_Value) return TypeName;
   function Get (Val : JSON_Value; Field : UTF8_String) return TypeName;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : TypeName);
end ${package-name};
```

Rev 1.2.0 YYYY-MM-DD
 * Compatible deserialisztion for onedimensional containers.
 *
Rev 1.1.0 2017-04-06
 * Simplify JSON-encoding for onedimensional containers.

Rev 1.0.1 2016-06-14
 * First production revision.

Rev 0.0.6 2016-06-02
 * Initial Version

ToDo:
 * Make compatible deserialisation for maps.
 * Finalize support for multiway_trees.
 * Rewite the crude codegenerator.
 * Add more support packages.


I
