
pragma Ada_2012;

--  This package defines routines providing the main functionality of
--  GNATCOLL.JSON.Support.Builder.

with Asis; use Asis;

package GNATCOLL.JSON.Support.Builder.Sampler is

   procedure Brief_Help;
   --  Prints brief help information to stdout.

   procedure Initialize;
   --  Reads and checks the command line parameters and initializes the
   --  GNATCOLL.JSON.Support.Builder options. Checks the existence of the files to be processed
   --  by GNATCOLL.JSON.Support.Builder and applicability of the gnatstub options with these files.
   --  Tries to create the tree file, if necessary.
   --  If everything is OK, sets the global Is_Initialized variable True.
   --  This procedure does not use anything from ASIS

   procedure Create_Sample;
   --  If Is_Initialized, generates the sample body. This procedure is an
   --  ASIS application

   procedure Clean_Up;
   --  Beacuse of historical reasons, the clean-up procedure for gnatstub
   --  differs from the standard clean-up procedure for othet AdaCore ASIS
   --  tools that are based on ASIS UL. gnatstub creates the tree file in the
   --  current directory and it may reuse the existing tree file and it may
   --  keep the tree file it has created. So this procedure first removes the
   --  tree file in the current directory (if the option that keeps the tree
   --  is not specified), and then it calls the standard clean-up procedure to
   --  clean the temporarty directory (if it has been created as a part of
   --  project file processing).

end GNATCOLL.JSON.Support.Builder.Sampler;
