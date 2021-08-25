with Ada.Containers;

procedure Main is
   e1 : constant := 1e3;
   d1 : constant := 16#ada.c#;
   X : Integer;
begin
   --  Incomplete literals:
   X := 1_;
   X := 1#;
   X := 1.;
   X := 1.0_;
   X := 1.0e;
   X := 1.0e+;
   X := 1e0_;
   X := 16#e;
   X := 16#ada.;
   X := 16#ada.c;
end Main;