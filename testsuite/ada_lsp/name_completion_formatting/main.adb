procedure Main is
   package Ops is
      type Receiver is tagged null record;
      procedure Long_Name
        (First_Long_Parameter   : Integer;
         Second_Long_Parameter  : out Integer;
         Third_Long_Parameter   : in out Integer;
         Fourth_Long_Parameter  : Integer;
         Fifth_Long_Parameter   : Integer;
         Sixth_Long_Parameter   : Integer;
         Seventh_Long_Parameter : Integer;
         Eighth_Long_Parameter  : Integer) is null;
      procedure Long_Method
        (Self                   : Receiver;
         First_Long_Parameter   : Integer;
         Second_Long_Parameter  : out Integer;
         Third_Long_Parameter   : in out Integer;
         Fourth_Long_Parameter  : Integer;
         Fifth_Long_Parameter   : Integer;
         Sixth_Long_Parameter   : Integer;
         Seventh_Long_Parameter : Integer;
         Eighth_Long_Parameter  : Integer) is null;
   end Ops;
   use Ops;

   procedure Small_Name (A : out Integer; B : in out Integer) is null;
   procedure No_Args is null;
   Obj : Receiver;
   Value : Integer;
begin
   Long_N;
   Small_N;
   No_Ar;
   Val;
   Ops.Long_N;
   Ops.;
   Obj.Long_M;
   Obj.;
end Main;
