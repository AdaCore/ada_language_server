with Bar; use Bar;

procedure Foo is
   M : My_Access := null;
begin
   Hello (1);

   Bar.Hello (1, B => 1);

   M.Hello (1);
end Foo;
