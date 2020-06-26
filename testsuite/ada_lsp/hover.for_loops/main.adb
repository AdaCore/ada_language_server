procedure Main is
   function X (Y : Natural) return Natural
     with Pre => (for all JJJ in 1 .. 2 => Y > JJJ);
begin
   null;
end Main;
