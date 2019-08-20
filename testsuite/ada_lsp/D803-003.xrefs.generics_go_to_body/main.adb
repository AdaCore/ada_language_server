with Gen;
with Genp;

procedure Main is
   package G is new Gen;
   procedure P is new Genp;
begin
   G.Foo;
   P;
end Main;
