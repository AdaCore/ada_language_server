procedure Main is
   procedure Proc (X : access procedure);
   procedure Proc_2;

   procedure Proc (X : access procedure) is
   begin
      null;
   end;

   procedure Proc_2 is
   begin
      null;
   end Proc_2;
begin
   Proc (Proc_2'Access);
end Main;
