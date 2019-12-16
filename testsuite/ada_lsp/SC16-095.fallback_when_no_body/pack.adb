package body pack is

   procedure Foo (Y : Integer);
   
   function Bla (Y : Integer) return Integer is
   begin
      return 2;
   end Bla;
   
   type Y is null record;
   
   procedure Nobody (Arg : Y) is
   begin
      null;
   end;
   
   procedure Allow_A_Body (Y : Integer) is null;

   procedure Foo (Y : Integer) is
   begin
      null;
   end;
   
end pack;
