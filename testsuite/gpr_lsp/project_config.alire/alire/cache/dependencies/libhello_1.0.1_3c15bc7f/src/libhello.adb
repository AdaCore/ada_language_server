with Ada.Text_IO;

package body Libhello is

   -----------------
   -- Hello_World --
   -----------------

   procedure Hello_World is
      use Ada.Text_IO;
   begin
      Put_Line ("Hello, world!");
   end Hello_World;

end Libhello;
