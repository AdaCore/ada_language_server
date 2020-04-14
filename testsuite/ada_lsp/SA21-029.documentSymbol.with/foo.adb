with Ada.Text_IO; --  With statement => visible

procedure Foo is
   I : Integer; --  Nested level = 1 => visible

   procedure Bar;

   procedure Bar is
      J : Integer; --  Nested level = 2 => invisible
   begin
      Ada.Text_IO.Put_Line ("Bar");
   end Bar;
begin
   Ada.Text_IO.Put_Line ("Foo");
end Foo;
