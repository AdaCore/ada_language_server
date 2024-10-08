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

   type Tracer_Record is new GNATCOLL.Traces.Trace_Handle_Record with null record;
   type Tracer is access all Tracer_Record'Class;
   --  This type extends the GNATCOLL.Traces.Trace_Handle type with primitives
   --  for logging Virtual_Strings, exception occurrences, and VSS element
   --  vectors.

   function Create
     (Trace_Name : String;
      Default   : GNATCOLL.Traces.Default_Activation_Status
         := GNATCOLL.Traces.From_Config)
      return Tracer;

   procedure Trace_Text
     (Self : in out Tracer_Record;
      Text : VSS.Strings.Virtual_String'Class);

   procedure Trace_Exception
     (Self    : in out Tracer_Record;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   procedure Trace
     (Self : in out Tracer_Record;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   type Server_Tracer is limited new LSP.Tracers.Tracer with private;
   --  Implementation of abstract tracer interface over GNATCOLL.Traces

   procedure Initialize
     (Self         : in out Server_Tracer'Class;
      Server_Trace : Tracer;
      In_Trace     : Tracer;
      Out_Trace    : Tracer);
   --  Server_Trace - main trace for the LSP.
   --  In_Trace and Out_Trace - traces that logs all input & output for
   --  debugging purposes.

   overriding procedure Trace
     (Self : in out Server_Tracer;
      Text : String);

   overriding procedure Trace_Text
     (Self : in out Server_Tracer;
      Text : VSS.Strings.Virtual_String'Class);

   overriding procedure Trace_Exception
     (Self    : in out Server_Tracer;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   overriding procedure Trace
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Trace_Input
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Trace_Output
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector);

   overriding function Location
     (Self : Server_Tracer) return VSS.Strings.Virtual_String;

private

   type Server_Tracer is limited new LSP.Tracers.Tracer with record
      Server_Trace    : Tracer;
      In_Trace        : Tracer;
      Out_Trace       : Tracer;
   end record;

end LSP.GNATCOLL_Tracers;
