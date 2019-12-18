with Children; use Children;
with Great_Children; use Great_Children;

procedure Main is
   Obj : constant Child'Class := Great_Child'(others => <>);
begin
   Obj.Primitive;
end Main;
