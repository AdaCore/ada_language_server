package U is
   procedure V;
end U;

with GNAT.OS_Lib; use GNAT.OS_Lib;
package body U is
   procedure V is separate;
   procedure W is separate;
begin
   W;
end U;
