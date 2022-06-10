procedure Foo is
   type Foo (X : Integer) is record
      case X is
         when -6 =>
            A : Integer;
         when -5 | -4 =>
            B : Integer;
         when -3 .. -1 =>
            C : Integer;
         when Natural =>
            D : Integer;
         when others =>
            E : Integer;
      end case;
   end record;

   Y : Foo := Foo'
begin
   null;
end Foo;
