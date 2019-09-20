with root; use root;

package child is

   type child_t is new root_t with null record;
   
   overriding procedure foo (t : child_t) is null;

end child;
