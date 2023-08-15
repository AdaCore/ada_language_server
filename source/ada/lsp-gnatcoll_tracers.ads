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

with Ada.Exceptions;

with GNATCOLL.Traces;

with VSS.Strings;
with VSS.Stream_Element_Vectors;
with LSP.Tracers;

package LSP.GNATCOLL_Tracers is

   type Tracer is limited new LSP.Tracers.Tracer with private;
   --  Implementation of abstract tracer interface over GNATCOLL.Traces

   procedure Initialize
     (Self         : in out Tracer'Class;
      Server_Trace : GNATCOLL.Traces.Trace_Handle;
      In_Trace     : GNATCOLL.Traces.Trace_Handle;
      Out_Trace    : GNATCOLL.Traces.Trace_Handle);
   --  Server_Trace - main trace for the LSP.
   --  In_Trace and Out_Trace - traces that logs all input & output for
   --  debugging purposes.

   overriding procedure Trace
     (Self : in out Tracer;
      Text : String);

   overriding procedure Trace_Text
     (Self : in out Tracer;
      Text : VSS.Strings.Virtual_String'Class);

   overriding procedure Trace_Exception
     (Self    : in out Tracer;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   overriding procedure Trace
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Trace_Input
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Trace_Output
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

private

   type Tracer is limited new LSP.Tracers.Tracer with record
      Server_Trace    : GNATCOLL.Traces.Trace_Handle;
      In_Trace        : GNATCOLL.Traces.Trace_Handle;
      Out_Trace       : GNATCOLL.Traces.Trace_Handle;
   end record;

end LSP.GNATCOLL_Tracers;
