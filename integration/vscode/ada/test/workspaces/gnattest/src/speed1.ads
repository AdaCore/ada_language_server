--
--  Copyright (C) 2011-2012, AdaCore
--

--  manual speed controller definition

package Speed1 is
   subtype Speed_Type  is Integer range 0 .. 200;
   subtype Speed_Delta is Integer range -5 .. +5;

   type Controller is tagged private;
   function Speed (This : Controller) return Speed_Type;
   procedure Adjust_Speed (This : in out Controller; Increment : Speed_Delta);

private
   type Controller is tagged record
      Actual_Speed : Speed_Type := 0;
   end record;

end Speed1;
