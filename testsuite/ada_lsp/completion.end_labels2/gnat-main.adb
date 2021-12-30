with Ada.Text_IO;

procedure GNAT.Main is

   task T is
      entry Start;
   end T;

   task body T is
   begin
      select
         accept Start do
            null;
         end Start;
      or
         terminate;
      end select;
   end T;

   function "+" (X : Integer) return Integer is
   begin
      return F : Integer do
         F := X;
      end return;
   end "+";

   package Pkg is end Pkg;

   generic
   package GP is
      function GF return Integer;
   end GP;

   package body GP is
      function GF return Integer is
      begin
         return 0;
      end GF;
   end GP;

   protected PO is
      entry E (X : Integer);
   end PO;

   protected body PO is
      entry E (X : Integer) when True is
      begin
         null;
      end E;

   end PO;

   type Rec is record
      X : Natural;
   end record;
   for Rec use record
      X at 0 range 0 .. 31;
   end record;

   type Optional_Integer (Is_Set : Boolean := True) is record
      case Is_Set is
         when True  => X : Integer;
         when False => null;
      end case;
   end record;
begin
   select
      T.Start;
   then abort
      Ada.Text_IO.New_Line;
   end select;

   Blk:
   declare
   begin
      null;
   end Blk;

   for S in Boolean'Range loop
      begin
         null;
      end;
   end loop;

   case True is
      when True | False =>
         if True then
            null;
         end if;
   end case;

end GNAT.Main;
