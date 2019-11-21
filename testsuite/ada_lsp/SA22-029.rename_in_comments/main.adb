
with Ada.Text_IO;
with P1;

procedure Main is
   List : P1.P_Rec;
begin
   List.Flag := False;
   List.Count := 1;

   Ada.Text_IO.Put_Line ("Hello");

   Ada.Text_IO.Put_Line ("Hello1");

   Ada.Text_IO.Put_Line ("Hello2");

   Ada.Text_IO.Put_Line (List.Count'Img);
end Main;
