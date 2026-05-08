package body Dispatch_Pkg is
   function P (Self : Root) return Integer is (0);
   overriding function P (Self : Child) return Integer is (1);
end Dispatch_Pkg;
