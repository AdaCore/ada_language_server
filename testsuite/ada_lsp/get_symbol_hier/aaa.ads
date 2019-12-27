package Aaa is
   type Enum is (A, B);
   Variable : Enum;
   package Nested_Package is
     procedure Proc (X : Integer) is null;
   end Nested_Package;
end Aaa;
