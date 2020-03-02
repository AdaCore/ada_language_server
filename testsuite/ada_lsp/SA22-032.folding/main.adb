
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   S : Unbounded_String := To_Unbounded_String ("Hello");
begin

   -- simple comment
   -- for test

   Put_Line (To_String (S));
end Main;
