package pkg is

   function ABC1 return Integer;

   type ABC4 (ABC5 : Integer) is record
      ABC6 : Boolean := (for some ABC7 in Boolean => ABC7);
   end record;

   procedure Test (ABC8 : Integer) is null;

   protected Prot is
      entry ABC9;
   private
      ABC10 : Boolean := False;
   end Prot;

   generic
      ABC13 : Integer;
   package ABC14 is
   end ABC14;

private
   function ABC2 return Integer;
end pkg;
