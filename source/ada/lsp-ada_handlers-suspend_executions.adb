------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020-2023, AdaCore                  --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with VSS.JSON.Streams;

with LSP.Servers;

package body LSP.Ada_Handlers.Suspend_Executions is

   ------------
   -- Create --
   ------------

   overriding function Create
     (Any : not null access LSP.Structures.LSPAny_Vector)
      return Suspend_Execution
   is
      use type VSS.Strings.Virtual_String;
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      use all type VSS.JSON.JSON_Number_Kind;

      Index : Natural := Any.First_Index;
   begin
      return Result : Suspend_Execution :=
        (Input_Queue_Length => 0)
      do
         if Index < Any.Last_Index
           and then Any (Index).Kind = Start_Array
         then
            Index := Index + 1;

            if Index < Any.Last_Index
              and then Any (Index).Kind = Start_Object
            then
               Index := Index + 1;

               if Index < Any.Last_Index
                 and then Any (Index).Kind = Key_Name
                 and then Any (Index).Key_Name = "inputQueueLength"
               then
                  Index := Index + 1;

                  if Index < Any.Last_Index
                    and then Any (Index).Kind = Number_Value
                    and then Any (Index).Number_Value.Kind = JSON_Integer
                  then
                     Result.Input_Queue_Length :=
                       Natural (Any (Index).Number_Value.Integer_Value);
                  end if;
               end if;
            end if;
         end if;
      end return;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : Suspend_Execution;
      Handler  : not null access LSP.Ada_Handlers.Message_Handler'Class;
      Response : in out LSP.Structures.LSPAny_Or_Null;
      Error    : in out LSP.Errors.ResponseError_Optional)
   is
      pragma Unreferenced (Response);
   begin
      while Handler.Server.Input_Queue_Length < Self.Input_Queue_Length loop
         delay 0.1;
      end loop;
   end Execute;

end LSP.Ada_Handlers.Suspend_Executions;
