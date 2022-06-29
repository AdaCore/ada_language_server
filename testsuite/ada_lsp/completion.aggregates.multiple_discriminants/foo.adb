procedure Foo is
   type Foo (X, Y, Z : Boolean) is record
      case X is
         when True =>
            case Y is
               when True =>
                  A : Integer;
               when False =>
                  B : Integer;
            end case;
         when others =>
            case Z is
               when True =>
                  C : Float;
               when False =>
                  D : Float;
            end case;
      end case;
   end record;

   F : Foo := Foo'(
begin
   null;
end Foo;
