pragma Ada_2012;
package body pack is


   procedure Method (X : Child) is
   begin
      null;
   end Method;

   ------------
   -- Method --
   ------------

   procedure Method (X : Grandchild) is
      X : Child;
   begin
      X.Method;
   end Method;

   ------------
   -- Method --
   ------------

   procedure Method (X : Great_Grandchild) is
   begin
      null;
   end Method;

end pack;
