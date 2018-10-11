------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Spawn.Processes.Monitor;
with Spawn.Posix;

package body Spawn.Processes is

   ---------------
   -- Arguments --
   ---------------

   function Arguments (Self : Process'Class)
                       return Spawn.String_Vectors.UTF_8_String_Vector is
   begin
      return Self.Arguments;
   end Arguments;

   --------------------------
   -- Close_Standard_Error --
   --------------------------

   procedure Close_Standard_Error (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stderr));
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stdin));
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Monitor.Enqueue ((Monitor.Close_Pipe, Self'Unchecked_Access, Stdout));
   end Close_Standard_Output;

   -----------------
   -- Environment --
   -----------------

   function Environment
     (Self : Process'Class)
      return Spawn.Environments.Process_Environment is
   begin
      return Self.Environment;
   end Environment;

   ---------------
   -- Exit_Code --
   ---------------

   function Exit_Code (Self : Process'Class) return Integer is
   begin
      return Self.Exit_Code;
   end Exit_Code;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Process) is
   begin
      if Self.Status /= Not_Running then
         raise Program_Error;
      end if;
   end Finalize;

   --------------
   -- Listener --
   --------------

   function Listener (Self : Process'Class) return Process_Listener_Access is
   begin
      return Self.Listener;
   end Listener;

   -------------
   -- Program --
   -------------

   function Program (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Program);
   end Program;

   -------------------------
   -- Read_Standard_Error --
   -------------------------

   procedure Read_Standard_Error
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Count : constant Interfaces.C.size_t :=
        Posix.read (Self.pipe (Stderr), Data, Data'Length);
   begin
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Count : constant Interfaces.C.size_t :=
        Posix.read (Self.pipe (Stdout), Data, Data'Length);
   begin
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
   end Read_Standard_Output;

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
     (Self      : in out Process'Class;
      Arguments : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Arguments := Arguments;
   end Set_Arguments;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self        : in out Process'Class;
      Environment : Spawn.Environments.Process_Environment) is
   begin
      Self.Environment := Environment;
   end Set_Environment;

   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self     : in out Process'Class;
      Listener : Process_Listener_Access)
   is
   begin
      Self.Listener := Listener;
   end Set_Listener;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Self    : in out Process'Class;
      Program : UTF_8_String) is
   begin
      Self.Program := Ada.Strings.Unbounded.To_Unbounded_String (Program);
   end Set_Program;

   ---------------------------
   -- Set_Working_Directory --
   ---------------------------

   procedure Set_Working_Directory
     (Self      : in out Process'Class;
      Directory : UTF_8_String) is
   begin
      Self.Directory := Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Working_Directory;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Status := Starting;
      Self.Exit_Code := -1;
      Monitor.Enqueue ((Monitor.Start, Self'Unchecked_Access));
   end Start;

   ------------
   -- Status --
   ------------

   function Status (Self : Process'Class) return Process_Status is
   begin
      return Self.Status;
   end Status;

   -----------------------
   -- Working_Directory --
   -----------------------

   function Working_Directory (Self : Process'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Directory);
   end Working_Directory;

   --------------------------
   -- Write_Standard_Input --
   --------------------------

   procedure Write_Standard_Input
     (Self : in out Process'Class;
      Data : Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Count : constant Interfaces.C.size_t :=
        Posix.write (Self.pipe (Stderr), Data, Data'Length);
   begin
      Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
   end Write_Standard_Input;

end Spawn.Processes;
