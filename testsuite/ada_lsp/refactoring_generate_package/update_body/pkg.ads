package Pkg is
   function F (A : Integer) return Boolean;
   procedure M;

   overriding function F (A : Natural) return Boolean;
end Pkg;