with Ada.Text_IO;

procedure Main is

   type Subp_Access is access procedure (A : Integer);

   procedure Print_Integer (A : integer);

   procedure Print_Integer (A : integer) is
   begin
      Ada.Text_IO.Put_Line (A'Img);
   end Print_Integer;

   procedure Do_Something (Subp : Subp_Access; A : Integer);

   procedure Do_Something (Subp : Subp_Access; A : Integer) is
   begin
      Subp (A);
   end Do_Something;

begin
   Do_Something (Pri
end Main;
