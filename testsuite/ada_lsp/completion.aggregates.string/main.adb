with Ada.Text_IO;

procedure Main is
   A : constant String := (1 .. 0 => 'a');
begin
   if A'Last in A'Range then
      Ada.Text_IO.Put_Line ("Hello");
   end if;
end Main;
