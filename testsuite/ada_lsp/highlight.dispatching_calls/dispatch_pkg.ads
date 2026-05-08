package Dispatch_Pkg is
   type Root is tagged null record;
   function P (Self : Root) return Integer;

   type Child is new Root with null record;
   overriding function P (Self : Child) return Integer;
end Dispatch_Pkg;
