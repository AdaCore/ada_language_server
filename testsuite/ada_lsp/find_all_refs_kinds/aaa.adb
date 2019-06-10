with Ada.Streams;

procedure Aaa (Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
    Local : Ada.Streams.Stream_Element_Array (1 .. 5);
    Last : Ada.Streams.Stream_Element_Count;
begin
    Ada.Streams.Read (Stream.all, Local, Last);
    Aaa (Stream);
end Aaa;