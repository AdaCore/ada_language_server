with Ada.Text_Io;
package body messages is

   procedure You_Say_Hello
   is
   begin
      Ada.Text_Io.Put_Line ("Hello");
   end;
   
   procedure I_Say_Goodbye
   is
   begin
      Ada.Text_Io.Put_Line ("Goodbye");
   end;
end Messages;
