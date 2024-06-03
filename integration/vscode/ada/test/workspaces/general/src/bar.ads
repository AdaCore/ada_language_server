package Bar is
   procedure A_Procedure;
   --  This procedure is added for the purposes of testing the gnattest vscode
   --  task. Gnattest generates test skeletons for public subprograms and fails
   --  if there are no subprograms to test, hence adding this one.

private
   procedure A_Procedure is null;
   --  The implementation is provided in the private part to make the build
   --  succeed without causing Gnattest to fail because a null subprogram is not
   --  testable.
end Bar;
