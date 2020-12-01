package body pkg is

   function ABC1 return Integer is (1);

   function ABC2 return Integer is (2);

   function ABC3 return Integer is
   begin
      return ABC12 : Integer := 0;
   end ABC3;

   protected body Prot is
      entry ABC9 when ABC10 is
      begin
         null;
      exception
         when ABC11 : others =>
            null;
      end ABC9;
   end Prot;

end pkg;
