with Ada.Text_IO; use Ada.Text_IO;

procedure Hello is

   task type Monitor_Task is
      entry Start (Data_To_Monitor : Integer);
      entry Stop;
   end Monitor_Task;

   ------------------
   -- Monitor_Task --
   ------------------

   task body Monitor_Task is
      Data           : Integer;
      Stop_Requested : Boolean := False;
   begin
      loop
         --  Wait until Start or Stop
         select
            accept Start
              (Data_To_Monitor : Integer)
            do
               Data := Data_To_Monitor;
            end Start;
         or
            accept Stop do
               Stop_Requested := True;
            end Stop;
         end select;

         if Stop_Requested then
            Put_Line ("Bye " & Data'Image);
            exit;
         else
            Put_Line ("Monitoring " & Data'Image);
         end if;
      end loop;
   end Monitor_Task;

   T : Monitor_Task;
begin
   T.Start (1);
   T.Start (2);
   T.Stop;
end Hello;
