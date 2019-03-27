------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Glib.Main;
with Glib.Spawn;

with Spawn.Processes.Windows;

with Spawn.Windows_API;
pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

package body Spawn.Processes is

   subtype Context is Internal.Context;

   type Process_Access is access all Processes.Process'Class;

   procedure Do_Start_Process (Self : aliased in out Process'Class);

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe);

   function Child_Watch is new Glib.Main.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Collback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   package Read_Write_Ex is
     new Windows_API.Generic_Read_Write_Ex (Context);

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        with Convention => Stdcall;

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

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
      Windows.Do_Close_Pipe (Self, Stderr);
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Windows.Do_Close_Pipe (Self, Stdin);
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Windows.Do_Close_Pipe (Self, Stdout);
   end Close_Standard_Output;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe)
   is
      procedure On_No_Data;

      ----------------
      -- On_No_Data --
      ----------------

      procedure On_No_Data is
         use type Windows_API.BOOL;

         Ok    : Windows_API.BOOL;
         Pipe  : Context renames Self.pipe (Kind);
      begin
         Ok := Read_Write_Ex.ReadFileEx
           (hFile                => Pipe.Handle,
            lpBuffer             => Pipe.Buffer,
            nNumberOfBytesToRead => Pipe.Buffer'Length,
            lpOverlapped         => Pipe'Access,
            lpCompletionRoutine  => Callback (Kind));

         if Ok = System.Win32.FALSE then
            Self.Listener.Error_Occurred
              (Integer (System.Win32.GetLastError));
         end if;
      end On_No_Data;

   begin
      Windows.Do_Read (Self, Data, Last, Kind, On_No_Data'Access);
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      procedure On_Start;

      procedure On_Start is
      begin
         Self.Event := Child_Watch
           (Glib.Spawn.GPid (Self.pid.hProcess),
            My_Death_Collback'Access,
            Self.Reference'Access);
      end On_Start;

   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Windows.Do_Start_Process (Self, On_Start'Access);
   end Do_Start_Process;

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

   -----------------------
   -- My_Death_Collback --
   -----------------------

   procedure My_Death_Collback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
   is
      pragma Unreferenced (pid, status);

      Process : constant Process_Access := Process_Access (data.Self);
   begin
      Windows.On_Process_Died (Process.all);
   end My_Death_Collback;

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
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Do_Read (Self, Data, Last, Stderr);
   end Read_Standard_Error;

   --------------------------
   -- Read_Standard_Output --
   --------------------------

   procedure Read_Standard_Output
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Do_Read (Self, Data, Last, Stdout);
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

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stderr);
   end Standard_Error_Callback;

   ------------------------------
   -- Standard_Output_Callback --
   ------------------------------

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdout);
   end Standard_Output_Callback;

   -----------------------------
   -- Standard_Input_Callback --
   -----------------------------

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdin);
   end Standard_Input_Callback;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Process'Class) is
   begin
      Self.Status := Starting;
      Self.Exit_Code := -1;
      Do_Start_Process (Self);
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
      procedure On_No_Data;

      ----------------
      -- On_No_Data --
      ----------------

      procedure On_No_Data is
         use type Windows_API.BOOL;
         Ok   : Windows_API.BOOL;
         Pipe : Context renames Self.pipe (Stdin);
      begin
         Ok := Read_Write_Ex.WriteFileEx
           (hFile                 => Pipe.Handle,
            lpBuffer              => Pipe.Buffer,
            nNumberOfBytesToWrite => Windows_API.DWORD (Pipe.Last),
            lpOverlapped          => Pipe'Access,
            lpCompletionRoutine   => Standard_Input_Callback'Access);

         if Ok = System.Win32.FALSE then
            Self.Listener.Error_Occurred
              (Integer (System.Win32.GetLastError));
         end if;
      end On_No_Data;

   begin
      Windows.Do_Write (Self, Data, Last, On_No_Data'Access);
   end Write_Standard_Input;

end Spawn.Processes;
