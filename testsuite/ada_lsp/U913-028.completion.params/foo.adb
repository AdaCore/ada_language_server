procedure Foo is
   procedure Bar (I, J : Integer);
   procedure Bar (I : Integer; F : Float);
   procedure Bar (I, J : Integer) is
   begin
      null;
   end Bar;
   procedure Bar (I : Integer; F : Float) is
   begin
      null;
   end Bar;
begin
   Bar (
end Foo;
