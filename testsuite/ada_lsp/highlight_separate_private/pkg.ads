package Pkg is
   type Private_Type is private;
   procedure Use_Type (Value : Private_Type);
   procedure Separate_Proc;
private
   type Private_Type is null record;
end Pkg;
