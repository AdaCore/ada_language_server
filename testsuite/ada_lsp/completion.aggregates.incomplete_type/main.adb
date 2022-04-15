procedure Main is

   type Cell;
   type Link is access Cell;

   type Cell is
   record
      Value : Integer;
      Succ  : Link;
      Pred  : Link;
   end record;

   Head : Link := new Cell'(

begin
   null;
end Main;
