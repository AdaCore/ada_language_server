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

with Ada.Strings.Unbounded;

with GNAT.Traceback.Symbolic;

with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;

package body LSP.GNATCOLL_Tracers is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Tracer'Class;
      Server_Trace : GNATCOLL.Traces.Trace_Handle;
      In_Trace     : GNATCOLL.Traces.Trace_Handle;
      Out_Trace    : GNATCOLL.Traces.Trace_Handle) is
   begin
      Self.Server_Trace := Server_Trace;
      Self.In_Trace := In_Trace;
      Self.Out_Trace := Out_Trace;
   end Initialize;

   -----------
   -- Trace --
   -----------

   overriding procedure Trace (Self : in out Tracer; Text : String) is
   begin
      Self.Server_Trace.Trace (Text);
   end Trace;

   -----------
   -- Trace --
   -----------

   overriding procedure Trace
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Aux  : Ada.Strings.Unbounded.String_Access :=
        new String'(VSS.Stream_Element_Vectors.Conversions
                    .Unchecked_To_String (Text));
   begin
      Self.Server_Trace.Trace (Aux.all);
      Ada.Strings.Unbounded.Free (Aux);
   end Trace;

   ---------------------
   -- Trace_Exception --
   ---------------------

   overriding procedure Trace_Exception
     (Self    : in out Tracer;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
   begin
      Self.Trace_Text
        (if Message.Is_Empty then "Exception:" else Message);

      Self.Trace
        (Ada.Exceptions.Exception_Name (Error) & " - " &
           Ada.Exceptions.Exception_Message (Error));

      Self.Trace (GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end Trace_Exception;

   -----------------
   -- Trace_Input --
   -----------------

   overriding procedure Trace_Input
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Aux  : Ada.Strings.Unbounded.String_Access;
   begin
      if Self.In_Trace.Is_Active then
         Aux := new String'(VSS.Stream_Element_Vectors.Conversions
                            .Unchecked_To_String (Text));
         Self.In_Trace.Trace (Aux.all);
         Ada.Strings.Unbounded.Free (Aux);
      end if;
   end Trace_Input;

   ------------------
   -- Trace_Output --
   ------------------

   overriding procedure Trace_Output
     (Self : in out Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Aux  : Ada.Strings.Unbounded.String_Access;
   begin
      if Self.Out_Trace.Is_Active then
         Aux := new String'(VSS.Stream_Element_Vectors.Conversions
                            .Unchecked_To_String (Text));
         Self.Out_Trace.Trace (Aux.all);
         Ada.Strings.Unbounded.Free (Aux);
      end if;
   end Trace_Output;

   ----------------
   -- Trace_Text --
   ----------------

   overriding procedure Trace_Text
     (Self : in out Tracer;
      Text : VSS.Strings.Virtual_String'Class) is
   begin
      Self.Server_Trace.Trace (VSS.Strings.Conversions.To_UTF_8_String (Text));
   end Trace_Text;

end LSP.GNATCOLL_Tracers;
