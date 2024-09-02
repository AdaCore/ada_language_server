with Base_Types;

package Other is

   type New_Private_Type is new Base_Types.Private_Type;

   type New_Int is new Base_Types.Int;

   type Circle is new Base_Types.Abstract_Shape with record
      Radius : Integer;
   end record;

   type Prot;

   protected type Prot is new Base_Types.Limit with
      procedure Set;
   private
      Lock : Boolean := True;
   end Prot;

   task Worker is new Base_Types.Limit with
      entry Start;
   end Worker;

end Other;
