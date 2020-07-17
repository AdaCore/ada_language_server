package Common is

   type Data is abstract tagged limited null record;
   type Data_Access is access all Data'Class;

   function Create (Self : access Data) return Integer is abstract;

   function Uno return Integer is (1);

end Common;
