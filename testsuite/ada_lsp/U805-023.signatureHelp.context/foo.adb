procedure Foo is
   procedure Bar (I, J : Integer);
   procedure Bar (I : Integer; F : Float);
   function Bar (I : Integer) return Integer is (1);

   ---------
   -- Bar --
   ---------

   procedure Bar (I, J : Integer) is
   begin
      null;
   end Bar;

   ---------
   -- Bar --
   ---------

   procedure Bar (I : Integer; F : Float) is
   begin
      null;
   end Bar;

begin
   Bar (1
end Foo;
