
with Ada.Text_IO;

procedure Main is
   type Kind is (Red, Blue, Green);
   K, K1 : Kind := Red;
begin
   case K is
      when Red =>
         Ada.Text_IO.Put_Line ("Red");
      when Blue =>
         Ada.Text_IO.Put_Line ("Blue");
      when Green =>
         Ada.Text_IO.Put_Line ("Green");
   end case;

   case K1 is
      when Blue =>
         Ada.Text_IO.Put_Line ("Blue");
      when Red =>
         Ada.Text_IO.Put_Line ("Red");
      when Green =>
         Ada.Text_IO.Put_Line ("Green");
   end case;
end Main;
