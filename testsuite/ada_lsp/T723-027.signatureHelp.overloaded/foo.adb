procedure Foo 
is
   procedure Bar (A : Integer; B, C : Integer);
   procedure Bar (A : Integer; B : Integer);

   ---------
   -- Bar --
   ---------

   procedure Bar (A : Integer; B, C : Integer) is
   begin
      null;
   end Bar;

   ---------
   -- Bar --
   ---------

   procedure Bar (A : Integer; B : Integer) is
   begin
      null;
   end Bar;
begin
   Bar 
end Foo;
