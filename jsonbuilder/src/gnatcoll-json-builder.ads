with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;
package GNATCOLL.JSON.Builder is

   Version : constant String := "3.0";

  procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Name         => "example_app",
      Description  => "Example app. Will flag goto statements",
      Process_Unit => Process_Unit);

end GNATCOLL.JSON.Builder;
