with Ada.Text_IO;

package body Children is

   procedure Primitive (Self : in out Child;
                        Id   : Integer) is
   begin
      Self.Id := Id;
      Ada.Text_IO.Put_Line ("Child");
   end Primitive;

end Children;
