package Parents is

   type Parent is abstract tagged record
      Id : Integer;
   end record;

   procedure Primitive (Self : in out Parent;
                        Id   : Integer) is abstract;
   --  Parent procedure

end Parents;
