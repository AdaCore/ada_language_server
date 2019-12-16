package pack is

   procedure Foo is null;
   function Bla return Integer is (42);
   
   type X is abstract tagged null record;
   procedure Nobody (Arg : X) is abstract;

   procedure Allow_A_Body;
end pack;
