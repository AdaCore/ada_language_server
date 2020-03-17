with Ada.Text_IO;

procedure Main is
   A : Integer := 10;
begin
   --  Check that A_letter is not renamed
   Ada.Text_IO.Put_Line (Integer'Image (A));
end Main;
