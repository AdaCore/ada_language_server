package Base_Types is

   type Abstract_Shape;

   type Abstract_Shape is abstract tagged limited private;

   type Int is range 1 .. 100;

   type Private_Type is private;

   type Limit is limited interface;

   type Plain_1 is interface;
   type Plain_2 is interface;

private

   type Abstract_Shape is abstract tagged limited null record;

   type Private_Type is new Plain_1 and Plain_2 with null record;

end Base_Types;
