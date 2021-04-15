package P is
   type Root is abstract tagged null record;

   procedure Foo (R : Root) is abstract;
end P;
