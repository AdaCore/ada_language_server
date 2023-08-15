--  Check that only marked subprograms get highlighted as obsolescent.  In
--  particular, there was a LAL bug that made the pragmas impact the enclosing
--  package, so this test should check that the parent package does not get
--  marked as obsolescent.
package Pkg_With_Obsolescent_Members is

   procedure Proc1;
   pragma Obsolescent;

   procedure Proc2;
   --  Not obsolescent

   procedure Proc3;
   pragma Obsolescent;

end Pkg_With_Obsolescent_Members;
