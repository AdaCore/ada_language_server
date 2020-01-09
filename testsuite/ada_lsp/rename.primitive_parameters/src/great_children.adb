with Ada.Text_IO;

package body Great_Children is

   procedure Primitive (Self : in out Great_Child;
                        Id   : Integer) is
   begin
      Self.Id := Id;

      Ada.Text_IO.Put_Line ("Great child");
   end Primitive;

end Great_Children;
