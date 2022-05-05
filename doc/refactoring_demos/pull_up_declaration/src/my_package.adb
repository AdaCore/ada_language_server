package body My_Package is

   procedure My_Procedure is
      type My_Float_Type is new Float;

      My_Float_0 : constant My_Float_Type := 1.0;
      My_Float_1 : My_Float_Type;

      function My_Function
        (F : My_Float_Type)
         return My_Float_Type
      is (F + My_Float_0);


   begin
      My_Float_1 := My_Function (2.0);
   end My_Procedure;

end My_Package;
