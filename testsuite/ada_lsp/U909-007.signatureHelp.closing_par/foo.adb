procedure Foo 
is
   procedure Bar (A : Integer; B, C : Integer);

   ---------
   -- Bar --
   ---------

   procedure Bar (A : Integer; B, C : Integer) is
   begin
      null;
   end Bar;
begin
   Bar (1,2,3);
end Foo;
