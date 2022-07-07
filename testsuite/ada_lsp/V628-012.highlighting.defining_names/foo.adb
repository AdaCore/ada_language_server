procedure Foo is

   type My_Type is new Integer range 1 .. 10;

   X, Y : My_Type := 1;  --  Decl with multiple defining names
begin
   X := Y + My_Type (9);
end Foo;
