--  These with and use clauses will be parsed by the codefix
with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   package Nested is
      function Hello return Integer is (1);
   end Nested;

   Res : Integer := Hello + Foo_Bar;
begin
   null;
end Foo;
