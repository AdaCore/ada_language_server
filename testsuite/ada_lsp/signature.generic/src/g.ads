-- Source:

generic
    type Elt_Type(<>) is limited private;
    type Elt_Ptr is access all Elt_Type;
package G is
    type T(Length: Natural) is private;
    type T_Ptr is access all T;
private
    type Elt_Array is array(Positive range <>) of Elt_Ptr;
    type T(Length: Natural) is
	record
	    Elts: Elt_Array(1..Length);
	end record;
end G;
