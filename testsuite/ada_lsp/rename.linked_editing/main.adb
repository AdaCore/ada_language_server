with System;
procedure Main is
   use type System.Address;
begin
   if Main'Address = System.Null_Address then 
      Main;
   end if;
end Main;
