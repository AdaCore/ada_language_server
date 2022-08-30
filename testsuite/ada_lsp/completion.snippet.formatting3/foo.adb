procedure Foo is

   generic
      A      : Integer;
      BBBBBB : Float;
      CCCC   : String;
      with procedure Bar (X : Integer);
   package Foo_Bar is
      procedure Do_Nothing;

      procedure Do_Nothing is
      begin
         null;
      end Do_Nothing;
   end Foo_Bar;

   package Pack is new Foo_Bar
begin
   null;
end Foo;
