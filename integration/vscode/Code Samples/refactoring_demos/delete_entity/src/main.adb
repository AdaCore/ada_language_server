procedure Main is
   procedure To_Be_Deleted (Value : Float);

   Ref : access procedure (Value : Float) := To_Be_Deleted'Access;
   --  A "not-a-call" reference to the procedure --------^

   procedure To_Be_Deleted (Value : Float) is
   begin
      if Value > 0.0 then
         To_Be_Deleted (Value - 1.0);
      end if;
   end To_Be_Deleted;

   procedure Another is
   begin
      To_Be_Deleted (9.0);  --  We replace this call with null;
   end Another;
begin
   To_Be_Deleted (9.0);
   --  ^ We delete this call
   begin
      --  We delete whole begin/end block
      To_Be_Deleted (9.0);
   exception
      when others =>
         null;
   end;

   return;
end Main;
