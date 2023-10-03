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

with VSS.Stream_Element_Vectors;
with VSS.Strings;

package LSP.Tracers is
   pragma Preelaborate;

   type Tracer is limited interface;
   --  The tracer can write exceptions, errors and other information into log
   --  files/traces.

   type Tracer_Access is access all Tracer'Class with Storage_Size => 0;

   procedure Trace
     (Self : in out Tracer;
      Text : String) is abstract;
   --  Append Text to default trace file

   procedure Trace_Text
     (Self : in out Tracer;
      Text : VSS.Strings.Virtual_String'Class) is abstract;
   --  Append Text to default trace file

   procedure Trace_Exception
     (Self    : in out Tracer;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is abstract;
   --  Append an exception information to default trace file

   procedure Trace
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector) is abstract;
   --  Append raw data as text to default trace file

   procedure Trace_Input
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector) is abstract;
   --  Append raw data as text to input trace file

   procedure Trace_Output
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector) is abstract;
   --  Append raw data as text to output trace file

end LSP.Tracers;
