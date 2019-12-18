package Parents is

   type Parent is abstract tagged record
      A : Integer;
   end record;

   procedure Primitive (Self : Parent) is abstract;
   --  Parent procedure

end Parents;
