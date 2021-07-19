with Pkg_1.Child;

procedure Testcomp is
begin
   Pkg_1.;
   --% list(node.f_call.p_complete)
   Pkg_1.Child.;
   --% list(node.f_call.p_complete)
   Pkg_1.Child2.;
   --% list(node.f_call.p_complete)
   -- TODO: for the moment this doesn't return any results, rt. returning the
   --       elements of Pkg_1.Child2.*, which are loaded. For that we need to
   --       have a special mode ignoring the results of `has_visibility`, maybe
   --       via a dynamic var.
end Testcomp;
