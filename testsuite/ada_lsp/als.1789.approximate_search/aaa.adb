package body Aaa is
   procedure Proc (Z : in out Integer) is
   begin
      Z := Z + 2 * Z;

      Proc (Z => Z);
   end Proc;
end Aaa;
