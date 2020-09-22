procedure Main is
   procedure Aaa is
   begin
      null;
   end Aaa;
   procedure Bbb is
   begin
      Bbb;
      Aaa;
   end Bbb;
begin
   Aaa;
   Bbb;
end Main;
