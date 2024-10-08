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

   function Tracer_Factory return GNATCOLL.Traces.Logger;

   --------------------
   -- Tracer_Factory --
   --------------------

   function Tracer_Factory return GNATCOLL.Traces.Logger
   is (new Tracer_Record);

   ------------
   -- Create --
   ------------

   function Create
     (Trace_Name : String;
      Default   : GNATCOLL.Traces.Default_Activation_Status
         := GNATCOLL.Traces.From_Config)
      return Tracer is
   begin
      return Tracer (GNATCOLL.Traces.Create (Trace_Name, Default, Factory => Tracer_Factory'Access));
   end Create;

   ----------------
   -- Trace_Text --
   ----------------

   procedure Trace_Text
     (Self : in out Tracer_Record;
      Text : VSS.Strings.Virtual_String'Class) is
   begin
      if Self.Is_Active then
         Self.Trace (VSS.Strings.Conversions.To_UTF_8_String (Text));
      end if;
   end Trace_Text;

   ---------------------
   -- Trace_Exception --
   ---------------------

   procedure Trace_Exception
     (Self    : in out Tracer_Record;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is
   begin
      if Self.Is_Active then
         Self.Trace_Text
         (if Message.Is_Empty then "Exception:" else Message);

         Self.Trace
         (Ada.Exceptions.Exception_Information (Error));

         Self.Trace (GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
      end if;
   end Trace_Exception;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Self : in out Tracer_Record;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Aux  : Ada.Strings.Unbounded.String_Access;
   begin
      if Self.Is_Active then
         Aux := new String'(VSS.Stream_Element_Vectors.Conversions
            .Unchecked_To_String (Text));
         Self.Trace (Aux.all);
         Ada.Strings.Unbounded.Free (Aux);
      end if;
   end Trace;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Server_Tracer'Class;
      Server_Trace : Tracer;
      In_Trace     : Tracer;
      Out_Trace    : Tracer) is
   begin
      Self.Server_Trace := Server_Trace;
      Self.In_Trace := In_Trace;
      Self.Out_Trace := Out_Trace;
   end Initialize;

   --------------
   -- Location --
   --------------

   overriding function Location
     (Self : Server_Tracer) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Conversions.To_Virtual_String
        (Self.Server_Trace.Get_Stream_File.Display_Full_Name);
   end Location;

   -----------
   -- Trace --
   -----------

   overriding procedure Trace (Self : in out Server_Tracer; Text : String) is
   begin
      Self.Server_Trace.Trace (Text);
   end Trace;

   -----------
   -- Trace --
   -----------

   overriding procedure Trace
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Self.Server_Trace.Trace (Text);
   end Trace;

   ---------------------
   -- Trace_Exception --
   ---------------------

   overriding procedure Trace_Exception
     (Self    : in out Server_Tracer;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String)
   is
   begin
      Self.Server_Trace.Trace_Exception (Error, Message);
   end Trace_Exception;

   -----------------
   -- Trace_Input --
   -----------------

   overriding procedure Trace_Input
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Self.In_Trace.Trace (Text);
   end Trace_Input;

   ------------------
   -- Trace_Output --
   ------------------

   overriding procedure Trace_Output
     (Self : in out Server_Tracer;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Self.Out_Trace.Trace (Text);
   end Trace_Output;

   ----------------
   -- Trace_Text --
   ----------------

   overriding procedure Trace_Text
     (Self : in out Server_Tracer;
      Text : VSS.Strings.Virtual_String'Class) is
   begin
      Self.Server_Trace.Trace_Text (Text);
   end Trace_Text;

end LSP.GNATCOLL_Tracers;
