with Ada.Text_IO; use Ada.Text_IO;
with Pack;

procedure Foo2 is
begin
   declare
      procedure Hello;
      procedure Hello (J : Float) is
      begin
         Put_Line ("Hello World");
      end Hello;
   begin
      Hello;
      Hello (1);
      Pack.Hello (1);
      Pack.Hello;
   end;
end Foo2;
