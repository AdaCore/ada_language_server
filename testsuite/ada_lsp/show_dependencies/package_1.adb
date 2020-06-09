with Package_2;

package body Package_1 is

   procedure Do_Something is
   begin
      Package_2.Do_Something;
   end Do_Something;

end Package_1;
