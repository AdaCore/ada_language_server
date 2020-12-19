procedure Main is
   procedure My_Proc is null;

   procedure New_Proc renames My_Proc;
begin
   New_Proc;
end Main;
