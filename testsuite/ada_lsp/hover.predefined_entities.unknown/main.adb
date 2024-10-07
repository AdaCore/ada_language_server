with Ada.Text_IO; use Ada.Text_IO;

procedure Main
  with Unknown => Off
is
   Foo : Integer := 30;
   pragma Unknown (C, Foo);
begin
   Put_Line (Foo'Unknown);
end Main;
