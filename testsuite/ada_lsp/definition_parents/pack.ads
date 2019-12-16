package pack is

   type Root is abstract tagged null record;

   procedure Method (X : Root) is abstract;

   type Child is new Root with null record;

   overriding procedure Method (X : Child);

   type Grandchild is new Child with null record;

   overriding procedure Method (X: Grandchild) is null;

   type Other_Grandchild is new Child with null record;

   overriding procedure Method (X : Other_Grandchild);

end pack;
