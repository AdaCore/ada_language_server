procedure Test is
   type T is mod 2;
   procedure P (Pr : T);
   procedure B (Br : T; X : Natural := 0) is null;
begin
   P (1);
   B (2, 0);
   P (3);
   B (2);
end;
