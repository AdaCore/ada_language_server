with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   procedure Foo (I : Integer);

   procedure Foo (I : Integer) is
   begin
      Put_Line (I'Image);
   end Foo;

   type Foo_Access is access procedure (A : Integer);

   type Bar is
      record
         Subp_Anonymous_Access : access procedure (A : Integer);
         Subp_Access           : Foo_Access;
      end record;

   procedure Qux (Q : access procedure (A : Integer));

   procedure Corge (C : Foo_Access);

   procedure Qux (Q : access procedure (A : Integer)) is
   begin
      Q (1);
   end Qux;

   procedure Corge (C : Foo_Access) is
   begin
      C (1);
   end Corge;

   type Grault is array (Natural range <>) of Foo_Access;

   B : Bar :=
     Bar'(Subp_Anonymous_Access => Foo'Access, Subp_Access => Foo'Access);
   C : access procedure (A : Integer) := Foo'Access;
   D : Foo_Access := Foo'Access;
   E : Grault (0 .. 1) := (C, D);

begin
   Qux (C);
   Corge (D);

   C (1);
   B.Subp_Anonymous_Access (1);
   B.Subp_Access (1);
   D (1);
   E (0) (1);
   E (1) (1);
end Main;
