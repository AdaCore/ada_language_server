package body PkgBodyWithSpec is
   Global1 : Integer := 0;

   --  'null' in value position: the TextMate grammar classifies it as a
   --  keyword, but the server should emit a constant semantic token for it.
   type Int_Access is access all Integer;

   Null_Ptr : constant Int_Access := null;
   Ptr      : Int_Access := null;

   procedure Reset_Ptr is
   begin
      if Ptr /= null then
         Ptr := null;
      end if;
   end Reset_Ptr;

   procedure Proc1 (Arg1, Arg2 : Integer) is
      Y : Integer := Arg1 + Arg2 + Global1;
   begin
      null;
   end Proc1;

   function Expr_Fct (Arg1, Arg2 : Integer) return Integer
   is (Arg1 + Arg2 + Global1);

   procedure Proc2 (Arg1, Arg2 : Integer) is
      function Nested_Expr_Fct (Arg3, Arg4 : Integer) return Integer
      is (Arg1 + Arg2 + Arg3 + Arg4 + Global1);
   begin
      null;
   end Proc2;

end PkgBodyWithSpec;
