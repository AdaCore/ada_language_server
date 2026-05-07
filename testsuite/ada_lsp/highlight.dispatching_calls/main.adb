with Dispatch_Pkg; use Dispatch_Pkg;

procedure Main is
   Obj : Root'Class := Child'(null record);
   X   : Integer;
begin
   X := P (Obj);
   X := Obj.P;
end Main;
