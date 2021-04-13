-- Test package has the with but not the use
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

separate (Test)
function Baz (A : Integer) return Boolean is
begin
     return Ada.Numerics.Elementary_Functions.Log (10.0) > 5.0;
end Baz;
