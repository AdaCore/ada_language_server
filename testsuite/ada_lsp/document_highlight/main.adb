with Test; use Test;
with Ada.Text_IO;

procedure Main is
   My_Var : Integer := 10;
begin
   My_Var := 10;
   Ada.Text_IO.Put_Line (My_Var'Img);
end Main;
