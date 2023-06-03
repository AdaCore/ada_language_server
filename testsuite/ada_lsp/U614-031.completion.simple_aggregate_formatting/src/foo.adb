procedure Foo is
   type My_Record is tagged record
      I, J : Integer;
      F    : Float;
   end record;

   Bar : My_Record := My_Record'(
begin
   null;
end Foo;
