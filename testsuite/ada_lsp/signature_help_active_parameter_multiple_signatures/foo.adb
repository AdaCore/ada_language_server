procedure Foo
is
   function Bar (A : Integer; B, C : Integer) return Integer;
   function Bar (A : Integer; B, C, D : Integer) return Integer;

   ---------
   -- Bar --
   ---------

   function Bar (A : Integer; B, C : Integer) return Integer is
   begin
      return A + B + C;
   end Bar;

   function Bar (A : Integer; B, C, D : Integer) return Integer is
   begin
      return A + B + C + D;
   end Bar;
begin
   Bar (1
end Foo;