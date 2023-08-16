--
--  Copyright (C) 2011-2012, AdaCore
--

--  derived class violating LSP on adjustSpeed
with Speed1; use Speed1;
package Speed2 is
   type Auto_Controller is new Controller with private;

   function Desired_Speed (This : Auto_Controller) return Speed_Type;

   procedure Set_Desired_Speed
     (This : in out Auto_Controller; Val : Speed_Type);

--     overriding
   procedure Adjust_Speed
     (This : in out Auto_Controller; Increment : Speed_Delta);

private
   type Auto_Controller is new Controller with record
      Desired_Speed : Speed_Type := 0;
   end record;
end Speed2;
