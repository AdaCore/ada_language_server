--
--  Copyright (C) 2011-2012, AdaCore
--

package body Speed1 is
   function Speed (This : Controller) return Speed_Type is
   begin
      return This.Actual_Speed;
   end Speed;

   procedure Adjust_Speed
     (This : in out Controller; Increment : Speed_Delta) is
   begin
      This.Actual_Speed := This.Actual_Speed + Increment;
   end Adjust_Speed;

end Speed1;
