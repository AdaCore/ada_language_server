package Test is

   type Parent is tagged record 
      Num : Integer;
   end record
   
   function Value (Self : Parent) return Integer 
   is
     (Self.Num);
   
   type Child is new Parent with null record;
   
   function Value (Self : Child) return Integer 
   is
     (Self.Num);

end Test;
