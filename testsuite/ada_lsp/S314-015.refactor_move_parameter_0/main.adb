procedure Main is
    procedure Foo (A, B: Integer; C: Float);

    procedure Foo (A, B: Integer; C: Float) is
    begin
        null;
    end Foo;
begin
    Foo (1, 2, 3.0);
end Main;
