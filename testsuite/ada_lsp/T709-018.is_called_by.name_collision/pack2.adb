package body Pack2 is

   overriding function Create (Self : access Data2) return Integer is
   begin
      return Common.Uno + Common.Uno;
   end Create;

end Pack2;
