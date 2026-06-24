with Ada.Finalization; use Ada.Finalization;

procedure Test is
   type Point is record
      X, Y : Natural;
   end record;

   V_Point : Point := ();

   Static_Value : constant := -22;
   type TD (N : Integer) is record
      case N is
         when -1 .. 0 =>
            On_Range : Boolean;
         when Positive =>
            On_Positive : Boolean;
         when Static_Value =>
            On_Static_Value : Boolean;
         when others =>
            On_Others : Boolean;
      end case;
   end record;

   V_TD : TD := ();

   type Dynamic is new Ada.Finalization.Controlled with null record;

   V_Dynamic : Dynamic := (Controlled with null record);

   type Matrix_3x3 is array (1 .. 3, 1 .. 3) of Float;

   V_Matrix_3x3 : Matrix_3x3 := ();
   U_Matrix_3x3 : Matrix_3x3 := (others => ());

   type Comp is record
      Arr : Matrix_3x3;
      Pt  : Point;
      Next : access Comp;
      Char : Character;
      Enum : Boolean;
      Int  : Integer;
      Real : Float;
   end record;

   V_Comp : Comp := ();
begin
   null;
end Test;
