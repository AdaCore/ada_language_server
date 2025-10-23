with Interfaces;

procedure Main is

   type UInt_4 is mod 2**4;
   type Int_3 is range -4 .. 3;

   type Rec (X2 : Boolean) is record
      Base : Integer;
      X0   : Integer;
      X1   : Integer range 0 .. 3;
      X3   : Interfaces.Unsigned_8;
      X4   : Interfaces.Integer_8;
      X5   : UInt_4;
      X6   : Int_3;
   end record;

   for Rec use record
      Base at 0 range 0 .. 31;
      X0 at 4 range 0 .. 31;
      
   end record;
begin
   null;
end Main;
