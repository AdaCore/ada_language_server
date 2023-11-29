with Ada.Text_IO; use Ada.Text_IO;

procedure Main
  with SPARK_Mode => Off
is
   Foo : Integer := 30;
   pragma Convention (C, Foo);
begin
   Put_Line (Foo'Image);
end Main;
