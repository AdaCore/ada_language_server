package body A.B is
   procedure Proc (X : in out Integer) is
   begin
      X := X + 2 * X;

      Proc (X => X);
   end Proc;
end A.B;
