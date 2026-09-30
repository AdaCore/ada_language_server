package Pkg is

   --  Declare Foo at a line that is beyong the end of main.adb,
   --  since we want to test that we don't highlight references to
   --  declarations that are not in the current document.

   procedure Foo;
end Pkg;
