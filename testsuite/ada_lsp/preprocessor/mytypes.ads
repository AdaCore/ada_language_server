package mytypes is

   type recordtype is record
#if debug="true"
      Field1 
#else
      Field1
#end if; -- debug="true"
      : Integer;
#if debug="true"
      Field2 : Integer;
#else
      Field2 : Integer;
#end if; -- debug="true"
   end record;

   My_Var : recordtype :=
#if debug="true"
              (Field1     => Integer'Last,
#else
              (Field1     => Integer'First,
#end if; -- debug="true"
               Field2 => 42);
end mytypes;
