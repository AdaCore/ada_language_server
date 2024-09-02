private
package Base_Types.Ext is

   type New_Private_Type is new Private_Type with null record;

   type New_Int is new Int;

   type Cube is new Abstract_Shape with record
      Side : Integer;
   end record;

end Base_Types.Ext;
