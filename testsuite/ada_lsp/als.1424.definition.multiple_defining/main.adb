procedure Main is
   procedure Do_Something (First, Second : Integer) is
   begin
      Ada.Text_IO.Put_Line (First'Img);
      Ada.Text_IO.Put_Line (Second'Img);
   end Do_Something;
begin
   Do_Something (2, 3);
end Main;
