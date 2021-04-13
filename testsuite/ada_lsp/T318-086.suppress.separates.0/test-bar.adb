-- Test package has the with and the use
with Ada.Text_IO; use Ada.Text_IO;
-- Test package has the with but not the use
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
-- Test package does not have the with
with Ada.Assertions;
-- Test package does not have the with not the use
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

separate (Test) procedure Bar is
   B : Boolean;
begin
   Put_Line ("Separate_Do_Nothing");
   Put_Line (Ada.Numerics.Elementary_Functions.Log (10.0)'Image);
   Ada.Assertions.Assert (1 = 1);
   Ada.Assertions.Assert (Is_Character (Wide_Character'('a')));
   B := Baz (2);
end Bar;
