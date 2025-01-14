with Ada.Text_IO;

package body Common is

   procedure Put (Item : Enum := Hello) is
   begin
      case Item is
         when Hello => null;
         when Goodbye => null;
      end case;
   end Put;

end Common;
