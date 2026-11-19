procedure Foo is
   procedure Bar (A : Integer; B : Integer) is null;
begin
   Bar (123 , 456);
end Foo;
