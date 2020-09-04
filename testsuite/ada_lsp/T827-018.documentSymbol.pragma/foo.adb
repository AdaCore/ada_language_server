procedure Foo is
   
   procedure Proc_A (I : Integer);

   ------------
   -- Proc_A --
   ------------

   procedure Proc_A (I : Integer) 
   is
      pragma Unreferenced (I);
      --  Nested pragmas are not reported
   begin
      null;
   end Proc_A;
   
   procedure Proc_B;
   pragma Inline (Proc_B);

   ------------
   -- Proc_B --
   ------------

   procedure Proc_B is
   begin
      null;
   end Proc_B;
begin
   pragma Warnings (Off);
   Proc_B;
   Proc_A (42);
   pragma Warnings (On);
end Foo;
