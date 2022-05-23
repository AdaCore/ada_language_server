with Ada.Text_IO;
with My_Package;
procedure Main is
begin
   Ada.Text_IO.Put_Line (My_Package.C1.Q.B.F.I'Image);
   Ada.Text_IO.Put_Line (My_Package.C1.Q.B.F.I'Image);
   Ada.Text_IO.Put_Line (My_Package.C2.Q.B.F.I'Image);
end Main;
