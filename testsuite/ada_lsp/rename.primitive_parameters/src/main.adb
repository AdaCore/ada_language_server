with Children; use Children;
with Great_Children; use Great_Children;

procedure Main is
   Obj : Child'Class := Great_Child'(others => <>);
begin
   Obj.Primitive (3);
end Main;
