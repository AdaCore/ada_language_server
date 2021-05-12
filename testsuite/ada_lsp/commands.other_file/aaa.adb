package body Aaa is
   procedure Proc (X : in out Integer) is
   begin
      X := X + 2 * X;

      Proc (X => X);
   end Proc;
end Aaa;
