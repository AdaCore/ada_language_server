procedure Main is
begin
   for I in 1 .. 10 loop
      while True loop
         loop
            exit when I > 5;
            -- Inner loop body
         end loop;
      end loop;
   end loop;
end Main;
