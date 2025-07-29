with Ada.Text_IO;

package body Cycle is

   function Baz (t : Integer) return Integer;
   function Qux (t : Integer) return Integer;

   function Bar (t : Integer) return Integer is
   begin
      return Foo (t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t) +
      Foo(t);
   end;

   function Foo (t : Integer) return Integer is
   begin
      Ada.Text_IO.Put_Line ("Hello");
      Ada.Text_IO.Put_Line ("This");
      Ada.Text_IO.Put_Line ("is");
      Ada.Text_IO.Put_Line ("Multiple");
      Ada.Text_IO.Put_Line ("Put_Line");
      return Foo(t) +
      Foo(t) + Foo(t) +
      Foo(t) + Foo(t) +
      Foo(t) + Foo(t) +
      Foo(t) + Foo(t) + Foo(t);
   end Foo;

   function Baz (t : Integer) return Integer is
   begin
      return Qux (t) + Foo (t);
   end Baz;

   function Qux (t : Integer) return Integer is
   begin
      return Baz (t);
   end Qux;

   function Bat (t : Integer) return Integer is
   begin
      return Baz (t) + Bat (4);

   end Bat;


end Cycle;