procedure Foo is
   task Bar is
      entry Start;
   end Bar;

   task body Bar is
   begin
      accept Start;
   end Foo;

   protected Obj is
      function Get return Integer;
   private
      Local : Integer := 0;
   end Obj;

   protected body Obj is
      function Get return Integer is
      begin
         return Local;
      end Get;
   end Obj;
begin
   Bar.Start;
end Foo;
