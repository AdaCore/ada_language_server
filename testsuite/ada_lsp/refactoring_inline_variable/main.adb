
with Ada.Text_IO;

procedure Main is
   Inline : Integer := 1;
   Var    : Integer := 1;
begin
   Var := Var + Inline;
   Ada.Text_IO.Put_Line (Var'Img);
end Main;
