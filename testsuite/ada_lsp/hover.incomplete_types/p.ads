package P is
   type Incomplete;
   type Incomplete_2;

   type Something is record
      X : Incomplete;
   end record;

   type Incomplete is record
      Y : Integer;
   end record;

private
    type Incomplete_2 is record
        Z : Integer;
    end record;
end P;