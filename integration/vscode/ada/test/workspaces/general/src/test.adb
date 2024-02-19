with Bar;

with Foo;


----------
-- Test --
----------

procedure Test is

   procedure P1 is
      procedure P2 is
         procedure P3 is
         begin
            null;
            null;
            null;
         end P3;
      begin
         null;
      end P2;
   begin
      null;
   end P1;

begin

   null;

end Test;
