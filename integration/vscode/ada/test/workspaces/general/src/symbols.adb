with Foo;

package body Symbols is

   procedure Test is

      procedure P1 is
         procedure P2 is
            procedure P3 is
            begin
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

end Symbols;

--  This file is intended for testing the getSymbols helper function. See
--  helpers.test.ts. This comment is provided at the end to avoid disturbing
--  symbol locations above.
