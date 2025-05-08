procedure Iv_Test is
   S : constant String := "abc";
   X : constant := 23;
   Y : constant Integer := S'Length;
   Z : Integer := X + Y;

begin
   while Z > 0 loop
      Z := Z - 10;
   end loop;
end Iv_Test;
