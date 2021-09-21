with Generic_Pack;
with Parents; use Parents;

procedure Main is
   package A is new Generic_Pack (3);
   use A;

begin
   Prim  
end Main;
