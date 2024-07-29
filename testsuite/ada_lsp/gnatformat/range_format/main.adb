with Ada.Text_IO;

procedure Main is
   procedure You_Say_Hello;

   procedure You_Say_Hello is begin
   Ada.Text_Io.Put_Line ("Hello");
   end;

begin
   Ada.Text_IO.Put_Line ("Hello!");
   You_Say_Hello;
end Main;
