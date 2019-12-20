package pack is

   type Root is tagged null record;
   procedure Method (X : Root) is abstract;
   
   type Child is new Root with null record;
   procedure Method (X : Child);
   
   type Grandchild is new Child with null record;
   procedure Method (X : Grandchild);

   type Great_Grandchild is new Grandchild with null record;
   procedure Method (X : Great_Grandchild);
   
end pack;
