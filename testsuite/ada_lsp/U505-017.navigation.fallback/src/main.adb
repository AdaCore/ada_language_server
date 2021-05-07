procedure Main is

   procedure Do_Nothing (A : Integer; B : Integer);


   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (A : Integer) is
   begin
      null;
   end Do_Nothing;

begin
   --  Insert code here.
   Do_Nothing (1);
   Do_Nothing (1, 2);
   Do_Nothing (1, 2, 3);
end Main;
