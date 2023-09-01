--
--  Copyright (C) 2011-2012, AdaCore
--

package body Speed2 is

   function Desired_Speed (This : Auto_Controller) return Speed_Type is
   begin
      return This.Desired_Speed;
   end Desired_Speed;

   procedure Set_Desired_Speed
     (This : in out Auto_Controller; Val : Speed_Type) is
   begin
      This.Desired_Speed := Val;
   end Set_Desired_Speed;

   procedure Adjust_Speed
     (This : in out Auto_Controller; Increment : Speed_Delta) is
   begin
      null;
   end Adjust_Speed;

end Speed2;
