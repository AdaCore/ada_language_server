------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with GNATCOLL.Traces;

with VSS.Characters;
with VSS.Strings;
with VSS.Text_Streams;

package LSP.GNATCOLL_Trace_Streams is

   type Output_Text_Stream is limited
     new VSS.Text_Streams.Output_Text_Stream with private;
   --  Implementation of the text stream interface over GNATCOLL.Traces

   procedure Initialize
     (Self  : in out Output_Text_Stream'Class;
      Trace : GNATCOLL.Traces.Trace_Handle);
   --  Server_Trace - main trace for the LSP.

private

   type Output_Text_Stream is limited
     new VSS.Text_Streams.Output_Text_Stream with
   record
      Incomplete : VSS.Strings.Virtual_String;
      Trace      : GNATCOLL.Traces.Trace_Handle;
   end record;

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding procedure Put
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure Put_Line
     (Self    : in out Output_Text_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean);

   overriding procedure New_Line
     (Self    : in out Output_Text_Stream;
      Success : in out Boolean);

   overriding function Has_Error (Self : Output_Text_Stream) return Boolean is
    (False);

   overriding function Error_Message
     (Self : Output_Text_Stream) return VSS.Strings.Virtual_String
        is (VSS.Strings.Empty_Virtual_String);

end LSP.GNATCOLL_Trace_Streams;
