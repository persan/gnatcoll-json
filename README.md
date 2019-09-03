This library provides a set of utilities for serializing/deseriallizing
Ada Standard containers and most of the Types defined in the package hierarchies
    Ada, GNAT, System, Interfaces
   to/from JSON

The final target is to add a code generator that would generate a
```Ada
package ${package-name}.JSON is

   -- With at least the folowing methods for each type in the package ${package-name}
   function Create (Val : TypeName) return JSON_Array;
   function Get (Val : JSON_Value) return TypeName;
   function Get (Val : JSON_Value; Field : UTF8_String) return TypeName;
   procedure Set_Field  (Val : JSON_Value;  Field_Name : UTF8_String; Field  : TypeName);
end ${package-name};
```

Rev 1.3.0 2018-12-xx
* Corrected implementation of simple_maps.
* Clean up compiler warnings.
* Added "PATH" support.

Rev 1.2.0 2017-10-26
 * Compatible JSON for one-dimensional containers such as Vectors, Linked-Lists and  Sets.
 *

ToDo:
 * Make compatible deserialisation for maps.
 * Finalize support for multiway_trees.
 * Rewite the crude codegenerator.
 * Add more support packages.

I
