with foo; use foo;
procedure Main is
begin
   if A = 2 then
      raise Program_Error;
   end if;
end;
