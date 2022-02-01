procedure Foo is
  task type Worker is
      entry Start;
   end Worker;

   task body Worker is
   begin
      accept Start;
      accept Start;
   end Worker;

   Workers : array (1 .. 4) of Worker;
begin
   for J of Workers loop
      J.Start;
      delay 1.0;
      J.Start;
   end loop;
end Foo;
