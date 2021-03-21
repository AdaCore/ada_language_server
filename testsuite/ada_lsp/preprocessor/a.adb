with mytypes; use mytypes;

procedure A is

   My_Var : recordtype
   :=
#if debug="true"
              (Field1     => Integer'Last,
#else
              (Field1     => Integer'First,
#end if; -- debug="true"
               Field2 => 42);
begin
   My_Var.Field1 := 2;
   My_Var.Field2 := 2;
end;
