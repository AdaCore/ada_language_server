package Parent.Children is

   type Child;
   
   type Child is tagged limited private;
   
   procedure Do_Someting (D : Children.Child) is null;
   
private 
   
   type Child is tagged limited record 
      A : Integer;
   end record;
   
end Parent.Children;
