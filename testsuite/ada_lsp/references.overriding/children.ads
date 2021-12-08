with Parents; use Parents;

package Children is

   type Child_Type is new Parent_Type with null record;
   
   overriding procedure Primitive (A : Child_Type) is null;

end Children;
