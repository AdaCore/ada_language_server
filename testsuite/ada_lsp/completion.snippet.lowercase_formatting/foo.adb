procedure Foo is

   procedure Bar (A, B, C, D, E, F, G, H, I, J : Integer);

   procedure Bar (AAAAAAAA, BB, CCCCCCCC, DDDDDDDDDDDD, E : Integer);

   procedure Bar (A, B : Integer);

   procedure Bar (A, B, C, D, E, F, G, H, I, J : Integer) is
   begin
      null;
   end Bar;

   procedure Bar (AAAAAAAA, BB, CCCCCCCC, DDDDDDDDDDDD, E : Integer) is
   begin
      null;
   end Bar;

   procedure Bar (A, B : Integer) is
   begin
      null;
   end Bar;
begin
   Bar (
end Foo;
