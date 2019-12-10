procedure Foo
is
   type My_Record is tagged record
      I : Integer := 1;
   end record
     with Alignment => (8);

   type My_New_Record is new My_Record with
     Record
       J : Integer := 2;
       K : Integer := 3;
    end record with Alignment => (8);

   My_Int : Integer := 42 with Atomic;
   My_Var : My_Record := (I => My_Int);

   procedure Bar (V : in out My_Record)
     with
       Pre  => V.I > 0,
       Post => V.I > 0;

   procedure FooBar (V : in out My_New_Record) with
     Pre =>
       V.I > 0
       and then (V.J > V.K
                 and then V.K > V.I),
     Post => V.I = 0;

   procedure Bar (V : in out My_Record) is null;
   procedure FooBar (V : in out My_New_Record) is null;

begin
   null;
end Foo;
