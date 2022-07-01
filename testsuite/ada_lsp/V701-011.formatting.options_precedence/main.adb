with Ada.Unchecked_Deallocation;

procedure Main is

    type My_Type is new Integer;
    type My_Type_Access is access all My_Type;

    procedure Free is new Ada.Unchecked_Deallocation (My_Type, My_Type_Access);

    A : My_Type_Access := new My_Type'(3);
begin
    Free (A);
end Main;
