package Bar is

   procedure Hello (A : Integer; B : Float);

   type My_Type is tagged
      record
         B : Boolean;
      end record;
   type My_Access is access all My_Type;

   procedure Hello (M : access My_Type; A : Integer; B : Float);

end Bar;
