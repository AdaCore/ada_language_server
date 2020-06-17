package A is

   procedure Print;

   procedure Print (Value : String);

   type Print_Access is access procedure (Value : String);

end A;
