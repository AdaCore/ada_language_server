with Ada.Text_IO;

package body Children is

   procedure Primitive (Self : Child) is
   begin
      Ada.Text_IO.Put_Line ("Child");
   end Primitive;

end Children;
