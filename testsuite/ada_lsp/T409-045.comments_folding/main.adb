
procedure Main is
   procedure C_Function
   with Convention => C,
        Import     => True,
        Link_Name  => "c_function";

begin

   -----------------------
   -- test comment here --
   -----------------------

   C_Function;
end Main;
