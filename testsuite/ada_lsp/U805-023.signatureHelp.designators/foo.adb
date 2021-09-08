procedure Foo is

   procedure Bar (I, J, K : Integer);
   procedure Bar (I, J : Integer; F : Float);
   procedure Bar (F, H : Float; I : Integer);
   procedure Bar (J : Integer; F : Float);

begin
   Bar (I => 1, J => 2
end Foo;
