with P; use P;
package Q is
   type Child is new Root with null record;
   overriding procedure Foo (R : Child) is null;
end Q;
