package body Pack1 is

   overriding function Create (Self : access Data1) return Integer
   is
      I : constant Integer := Common.Uno;
   begin
      return Common.Uno + Common.Uno + Common.Uno + I;
   end Create;

end Pack1;
