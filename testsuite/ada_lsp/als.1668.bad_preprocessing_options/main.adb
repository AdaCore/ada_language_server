with Ada.Text_IO;
procedure Main is
    Var : constant Integer := 10;
begin
  # if VERSION = "1" then
   Ada.Text_IO.Put_Line ("Version 1");
  # else
   Ada.Text_IO.Put_Line ("Version 2");
  # end if;
   Ada.Text_IO.Put_Line ("Var = " & Var'Img);
end Main;
