
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   S : Unbounded_String := To_Unbounded_String ("Hello");

   --  Block for the procedure should also include conditions [12:0-16:0]
   procedure A
     (X : Integer;
      Y : Integer)
     with Pre => X > -2
     and then Y > -2;

   procedure A is
   begin
      null;
   end A;

   type My_Record is
      record
         Var : Integer;
      end record;

begin

   -- simple comment
   -- for test

   Put_Line (To_String (S));
end Main;
