procedure Foo is
   procedure Bar (I, J, K, L, M, N, O, P : Integer);
   procedure Bar (I, J, K, L, M, N, O, P : Integer) is
   begin
      null;
   end Bar;
   procedure Bar (A, B, C, D, E, F : Float);
   procedure Bar (A1, A2, A3, A4, A5 : String) is
   begin
      null;
   end Bar;
   procedure Bar (A1, A2, A3, A4, A5 : String);
   procedure Bar (A1, A2, A3, A4, A5 : String) is
   begin
      null;
   end Bar;
begin
   Bar (
end Foo;
