with Ada.Text_IO;
separate (U) procedure V is
begin
   Ada.Text_IO.Put_Line ("U.V.");
end V;

with GNAT.Regexp;
separate (U) procedure W is
begin
   null;
end W;
