with Ada.Text_IO;

procedure Main is
   package B is
      procedure C is begin null; end C;
   end B;

   package body B is
      procedure C is begin null; end C;
   end B;
begin
end Main;