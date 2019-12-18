with Ada.Text_IO;

package body Great_Children is

   procedure Primitive (Self : Great_Child) is
   begin
      Ada.Text_IO.Put_Line ("Great child");
   end Primitive;

end Great_Children;
