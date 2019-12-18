with Parents; use Parents;
package Children is

   type Child is new Parent with null Record;

   procedure Primitive (Self : Child);

end Children;
