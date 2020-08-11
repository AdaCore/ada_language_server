package A is

   procedure Subprogram;

   package B is
      procedure Subprogram;
            procedure Private_Subprogram_1;
   end B;

   procedure Private_Subprogram_1;
end A;
