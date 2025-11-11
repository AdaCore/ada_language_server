
with Ada.Text_IO;

package body A is

   C : Boolean := True;

   -----------
   -- Print --
   -----------

   procedure Print is
      B : Boolean := True;
   begin
      Ada.Text_IO.Put_Line (B'Img & C'Img);
   end Print;

end A;
