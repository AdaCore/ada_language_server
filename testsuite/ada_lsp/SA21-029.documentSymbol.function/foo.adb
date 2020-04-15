
procedure Foo is
   procedure Bar;
   function Bar return Boolean;
   function FooBar return Boolean is (True);

   ---------
   -- Bar --
   ---------

   procedure Bar is
   begin
      null;
   end Bar;

   ---------
   -- Foo --
   ---------

   function Bar return Boolean is
   begin
      return False;
   end Bar;
begin
   null;
end Foo;
