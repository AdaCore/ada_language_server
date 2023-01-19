with Ada.Exceptions;
with Ada.Text_IO;

procedure Dump_Stack (E : Ada.Exceptions.Exception_Occurrence) is
begin
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      Ada.Exceptions.Exception_Information (E));
end Dump_Stack;
