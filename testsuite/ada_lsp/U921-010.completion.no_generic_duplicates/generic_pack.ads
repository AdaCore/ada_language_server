with Parents; use Parents;

generic
   A : Integer;
package Generic_Pack is

   type Child is new Parent with null record;

end Generic_Pack;
