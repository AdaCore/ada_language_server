procedure Test is
   type T is mod 2;
   procedure P (Pr : T);
   procedure B (Br : T) is null;
begin
   P (1);
   B (2);
end;
