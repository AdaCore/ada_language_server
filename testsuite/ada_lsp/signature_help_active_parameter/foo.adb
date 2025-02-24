procedure Foo
is
   function Bar (A : Integer; B: Integer; C : Integer) return Integer;

   ---------
   -- Bar --
   ---------

   function Bar (A : Integer; B: Integer; C : Integer) return Integer is
   begin
      return A + B + C;
   end Bar;
begin
   Bar (1
end Foo;