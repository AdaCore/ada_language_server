package body Pkg is
   procedure Separate_Proc is separate;

   procedure Use_Type (Value : Private_Type) is
   begin
      null;
   end Use_Type;
begin
   Separate_Proc;
end Pkg;
