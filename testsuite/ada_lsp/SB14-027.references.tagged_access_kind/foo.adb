with Ada.Text_IO;

procedure Foo is
   type My_Rec is record
      My_Int : Integer;
   end record;

   Rec : My_Rec := (My_Int => 1);

   procedure Bar (I : access Integer);

   ---------
   -- Bar --
   ---------

   procedure Bar (I : access Integer) is
   begin
      I.all := 42;
   end Bar;
begin
   Ada.Text_IO.Put_Line ("Before: " & Rec.My_Int'Image);
   Bar (Rec.My_Int'Unrestricted_Access);
   Ada.Text_IO.Put_Line ("After: " & Rec.My_Int'Image);
end Foo;
