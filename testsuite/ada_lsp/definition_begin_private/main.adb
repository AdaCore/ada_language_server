procedure Main is
   package Pkg is
      X : Integer;
   private
      function Y return Integer;
   end Pkg;

   package body Pkg is
      Z : Integer;

      function Y return Integer is
      begin
         return (Z);
      end Y;

   begin
      Z := 0;
   end Pkg;

   protected type Pr_Type is
      procedure Proc;
   private
      procedure Priv;
   end Pr_Type;

   protected body Pr_Type is
      procedure Proc is
      begin
         Priv;
      end Proc;

      procedure Priv is
      begin
         null;
      end Priv;
   end Pr_Type;

   protected Pr_Obj is
      procedure Proc;
   private
      procedure Priv;
   end Pr_Obj;

   protected body Pr_Obj is
      procedure Proc is
      begin
         Priv;
      end Proc;

      procedure Priv is
      begin
         null;
      end Priv;
   end Pr_Obj;

   task type Task_Type is
      pragma Page;
   private
      entry Entr;
   end Task_Type;

   task body Task_Type is
      X : Integer;
   begin
      loop
         accept Entr;
      end loop;
   end Task_Type;

   task Task_Obj is
      pragma Page;
   private
      entry Entr;
   end Task_Obj;

   task body Task_Obj is
      X : Integer;
   begin
      loop
         accept Entr;
      end loop;
   end Task_Obj;

   procedure Local (A : Integer) is
      X : Integer;
   begin
      X := A;
   end Local;

begin
   null;
end Main;
