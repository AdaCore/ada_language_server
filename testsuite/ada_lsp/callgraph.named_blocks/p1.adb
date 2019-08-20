procedure P1 is

   procedure A is begin null; end A;

   procedure B is
   begin
      Named_Loop: loop
         A;
      end loop Named_Loop;
   end B;

begin
   B;
end P1;
