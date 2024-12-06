-- This test aims at checking that subprograms get highlighted as
-- entity.name.function.ada
package body Subprograms is
   function S1 return Boolean;
   procedure S2;

   -- With parameters
   function S3 (Param1 : T1; Param2 : T2);
   procedure S4 (Param1 : T1; Param2 : T2);

   -- Multiple parameters of same type
   function S5 (Param1, Param2 : T1; Param3 : T2);

   -- Operator declarations
   function "+" (Param1, Param2 : T1) return T1;
   function "xor" (Param1, Param2 : T1) return T1;

end Subprograms;
