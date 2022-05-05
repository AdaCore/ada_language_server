with Ada.Text_IO;

package body My_Package is

   procedure My_Procedure is
      Start_Index : constant Integer := 1;
      End_Index   : constant Integer := 10;
      J_Squared   : Integer;

   begin
      Ada.Text_IO.Put_Line ("After");
      for J in Start_Index .. End_Index loop
         J_Squared := J * J;
         Ada.Text_IO.Put_Line("Line " & J_Squared'Image);
      end loop;
      Ada.Text_IO.Put_Line ("Before");
   end My_Procedure;

end My_Package;
