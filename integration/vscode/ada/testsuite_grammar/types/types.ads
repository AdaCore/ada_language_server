package Types is

    type Incomplete;

    type Taggle_Incomplete is tagged;

    type Day    is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
    type Suit   is 
        (Clubs, Diamonds, Hearts, Spades);
    type Gender is 
        (
            M,
            F
        );
    type Level  is 
        (Low, 
        Medium,
        -- This is a comment
        Urgent);
    type Light  is (Red, Amber, Green); -- Red and Green are overloaded

    type Hexa   is ('A', 'B', 'C', 'D', 'E', 'F');
    type Mixed  is ('A', 'B', '*', B, None, '?', '%');

    subtype Weekday is 
        Day range Mon .. Fri;
    subtype Major   is Suit  range Hearts .. Spades;
    subtype Rainbow is Color range Red .. Blue;  --  the Color Red, not the Light

    type Page_Num  is range 1 .. 2_000;
    type Line_Size is
        range 1 .. Max_Line_Size;

    subtype Small_Int   is Integer   range -10 .. 10;
    subtype Column_Ptr  is Line_Size range 1 .. 10;
    subtype Buffer_Size is Integer   range 0 .. Max;

    type Byte        is mod 256; -- an unsigned byte
    type Hash_Index  is
        mod 97;  -- modulus is prime

    type Vector     is array(Integer  range <>) of Real;
    type Matrix     is 
        array(Integer  range <>, Integer range <>) of Real;
    type Bit_Vector is 
        array(
            Integer  range <>
        ) of Boolean;
    type Roman      is array(Positive range <>) of Roman_Digit;
    type Table    is array(1 .. 10) of Integer;
    type Schedule is array(Day) of Boolean;
    type Line     is array(1 .. Max_Line_Size) of Character;

    type Date is
   record
      Day   : Integer range 1 .. 31;
      Month : Month_Name;
      Year  : Integer range 0 .. 4000;
   end record;

    type Complex is
    record
        Re : Real := 0.0;
        -- Comment
        Im : Real := 0.0;
    end record;

    type Peripheral(Unit : Device := Disk) is record
        Status : State;
        case Unit is
            when Printer =>
                Line_Count : Integer range 1 .. Page_Size;
            when others =>
                Cylinder   : Cylinder_Index;
                -- Another comment
                Track      : Track_Number;
            end case;
        end record;

    type Peripheral_Ref is not null access Peripheral;  --  see 3.8.1
    type Binop_Ptr is 
        access all Binary_Operation'Class;

    type Local_Coordinate is new Coordinate;   --  two different types
    type Midweek is new Day range Tue .. Thu;  --  see 3.5.1
    type Counter is new Positive;              --  same range as Positive 


    type Queue is limited interface;
    type Synchronized_Queue is synchronized interface and Queue; -- see 9.11
    type Serial_Device is task interface;  -- see 9.1

    task type Server is
        entry Next_Work_Item(WI : in Work_Item);
        entry Shut_Down;
    end Server;

    task type Keyboard_Driver(ID : Keyboard_ID := New_ID) is
        new Serial_Device with  -- see 3.9.4
        entry Read 
            (C : out Character);
        entry Write(C : in  Character);
    end Keyboard_Driver;

end Types;