with Base_Types;

package body Other is

   protected body Prot is
      procedure Set is
      begin
         Lock := False;
      end Set;
   end Prot;

   task body Worker is
   begin
      accept Start;
   end Worker;

end Other;
