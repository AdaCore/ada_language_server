-- Check that object declarations get their identifiers highlighted as
-- variable.name.ada by the TM grammar
package Objects is
   Obj : T;
   Obj : constant T;

   -- Multiple objects of the same type
   Obj1, Obj2, Obj3 : T;

   -- Subprogram parameters
   function S5 (Param1 : T1; Param2 : T2; Param3 : T3);
   function S5 (Param1, Param2 : T1; Param3 : T2);

   -- Objects with initial values
   Obj1, Obj2, Obj3 : T := 1;
   function S5 (Param1 : T1; Param2 : T2 := Default; Param3 : T3);
   function S5 (Param1, Param2 : T1 := Default; Param3 : T2);

end Objects;
