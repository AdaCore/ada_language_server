with Ada.Text_IO;

procedure Main is
   A : Integer;
   procedure Foo (V : access Integer) is
   begin
      V.all := 3;
   end Foo;
begin
   A := 1;
   Foo (A'Unrestricted_Access);
   Ada.Text_IO.Put_Line (A'Image);
end Main;
