procedure Test is
   type T is mod 2;
   type Rec (Ds : T) is record
      Cm : T;
   end record;
   procedure P (Pr : T) is null;
begin
   null;
exception
  when Ex : Constraint_Error;
end;