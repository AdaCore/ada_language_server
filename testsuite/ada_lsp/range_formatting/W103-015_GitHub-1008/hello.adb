-- TOP OF FILE
with Ada.Text_IO;
with Messages;

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello!");
   Messages.You_Say_Hello;
   Messages.I_Say_Goodbye;
end Hello;
