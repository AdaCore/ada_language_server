procedure Test is
   function Foo return Float is (12.0);
   function Foo return Integer is (12);

   procedure Bar (A : Float; B : Integer) is null;
begin
   -- This should resolve to the wrong Foo, using the imprecise fallback
   Bar (Foo, );
end Test;
