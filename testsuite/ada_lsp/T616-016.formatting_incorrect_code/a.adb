with Ada.Text_IO;

package body A is

   -----------
   -- Print --
   -----------

   procedure Print is B : Boolean := True begin Ada.Text_IO.Put_Line ("A");
      if B
      then
         null;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Value : String) is
   begin
      Ada.Text_IO.Put_Line (Value);
   end Print;

end A;
