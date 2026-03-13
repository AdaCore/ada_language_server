procedure Main is
   protected M is
      procedure Foo;
   end M;

   protected body M is
      procedure Foo is
      begin
         null;
      end Foo;
   end M;
begin
   null;
end Main;
