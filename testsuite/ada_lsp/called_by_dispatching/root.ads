package root is

   type root_t is tagged null record;
   
   procedure foo (t : root_t);

   function create return root_t'class;
end root;
