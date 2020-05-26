procedure Main is

   type Base_Rec (Disc : Integer) is tagged record
      case Disc is
         when 1 =>
            A : Integer;
         when 2 =>
            Z : Integer;
         when others =>
            B : Integer;
      end case;
   end record;

   type Derived_Rec is new Base_Rec with record
      D : Integer;
   end record;

   Obj : Derived_Rec :=
begin
   null;
end Main;
