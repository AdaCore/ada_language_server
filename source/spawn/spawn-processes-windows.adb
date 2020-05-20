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

with Ada.Characters.Wide_Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Interfaces.C;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Environments.Internal;

package body Spawn.Processes.Windows is

   package Read_Write_Ex is
     new Windows_API.Generic_Read_Write_Ex (Internal.Context);

   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context)
        with Convention => Stdcall;

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context)
        with Convention => Stdcall;

   procedure Append_Escaped_String
     (Command_Line : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Argument     : Ada.Strings.Unbounded.Unbounded_String);
   --  Append the given argument to a command line such that CommandLineToArgvW
   --  return the argument string unchanged. Arguments in a command line should
   --  be separated by spaces; this subprogram doesn't add these spaces.

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

   ---------------------------
   -- Append_Escaped_String --
   ---------------------------

   procedure Append_Escaped_String
     (Command_Line : in out Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      Argument     : Ada.Strings.Unbounded.Unbounded_String)
   is
      --  Implementation of the subprogam based on Microsoft's blog post
      --  "Everyone quotes command line arguments the wrong way".

      use Ada.Strings.Wide_Unbounded;

      Quotation_Check_Pattern : constant Wide_String :=
        Ada.Characters.Wide_Latin_1.Space
        & Ada.Characters.Wide_Latin_1.Quotation
        & Ada.Characters.Wide_Latin_1.LF
        & Ada.Characters.Wide_Latin_1.HT
        & Ada.Characters.Wide_Latin_1.VT;

      S : constant Wide_String :=
        Ada.Strings.UTF_Encoding.Wide_Strings.Decode
          (Ada.Strings.Unbounded.To_String (Argument));

      J : Natural;  --  Iterator
      N : Natural;  --  Number of sequencial backslashes.

   begin
      if S'Length /= 0
        and then Ada.Strings.Wide_Fixed.Index (S, Quotation_Check_Pattern) /= 0
      then
         --  Don't quote unless we actually need to do so - hopefully avoid
         --  problems if programs won't parse quotes properly.

         Append (Command_Line, S);

      else
         Append (Command_Line, '"');
         --  Opening double quotation mark

         J := S'First;

         while J <= S'Last loop
            N := 0;

            while J <= S'Last and then S (J) = '\' loop
               J := J + 1;
               N := N + 1;
            end loop;

            if J > S'Last then
               --  Escape all backslashed, but let the terminating double
               --  quotation mark we add below be interpreted as a
               --  metacharacter.

               Append (Command_Line, (N * 2) * '\');

            elsif S (J) = '"' then
               --  Escape all backslashes and the following double
               --  quotation mark.

               Append (Command_Line, (N * 2 + 1) * '\');
               Append (Command_Line, '"');

            else
               --  Backslashes aren't special here.

               Append (Command_Line, N * '\');
               Append (Command_Line, S (J));
            end if;

            J := J + 1;
         end loop;

         Append (Command_Line, '"');
         --  Closing double quotation mark
      end if;
   end Append_Escaped_String;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Pipe_Kinds)
   is
      use type Windows_API.HANDLE;

      procedure Check_Error (Value : Windows_API.BOOL);

      -----------------
      -- Check_Error --
      -----------------

      procedure Check_Error (Value : Windows_API.BOOL) is
         use type Windows_API.BOOL;
      begin
         if Value = System.Win32.FALSE then
            Self.Listener.Error_Occurred
              (Integer (System.Win32.GetLastError));
         end if;
      end Check_Error;

      Handle : Windows_API.HANDLE renames Self.pipe (Kind).Handle;
   begin
      if Handle /= System.Win32.INVALID_HANDLE_VALUE then
         Check_Error (Windows_API.CancelIo (Self.pipe (Kind).Handle));
         Check_Error (System.Win32.CloseHandle (Self.pipe (Kind).Handle));
         Self.pipe (Kind).Handle := System.Win32.INVALID_HANDLE_VALUE;
      end if;
   end Do_Close_Pipe;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self       : in out Process'Class;
      Data       : out Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      Kind       : Pipe_Kinds;
      On_No_Data : access procedure)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Count : constant Ada.Streams.Stream_Element_Offset :=
        Self.pipe (Kind).Last;
   begin
      if Count = 0 then
         Last := Data'First - 1;
         On_No_Data.all;
      elsif Count > Data'Length then
         Self.pipe (Kind).Last := Count - Data'Length;
         Last := Data'Last;
         Data := Self.pipe (Kind).Buffer (1 .. Data'Length);

         Self.pipe (Kind).Buffer (1 .. Count - Data'Length) :=
           Self.pipe (Kind).Buffer (Data'Length + 1 .. Count);
      else
         Self.pipe (Kind).Last := 0;
         Last := Data'First + Count - 1;
         Data (Data'First .. Last) := Self.pipe (Kind).Buffer (1 .. Count);
      end if;
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process
     (Self     : aliased in out Process'Class;
      On_Start : access procedure)
   is
      function Make_Command_Line return Interfaces.C.wchar_array;
      function Work_Directory return String;
      function Is_Error (Value : Windows_API.BOOL) return Boolean;
      procedure Request_Read (Kind : Standard_Pipe);

      procedure Create_Pipe
        (Parent_Handle : out Windows_API.HANDLE;
         Child_Handle  : out Windows_API.HANDLE;
         Inbound       : Boolean;
         Success       : in out Boolean);

      function Create_Pipes
        (Start : access Windows_API.STARTUPINFOW) return Boolean;

      use type Windows_API.BOOL;
      use type Windows_API.DWORD;
      use type Windows_API.HANDLE;

      -----------------
      -- Create_Pipe --
      -----------------

      procedure Create_Pipe
        (Parent_Handle : out Windows_API.HANDLE;
         Child_Handle  : out Windows_API.HANDLE;
         Inbound       : Boolean;
         Success       : in out Boolean)
      is
         Mode : constant array (Boolean) of Windows_API.DWORD :=
           (True  => Windows_API.PIPE_ACCESS_INBOUND,
            False => Windows_API.PIPE_ACCESS_OUTBOUND);

         Client_Access : constant array (Boolean) of Windows_API.DWORD :=
           (True  => System.Win32.GENERIC_WRITE,
            False => System.Win32.GENERIC_READ);

         Inherit : aliased System.Win32.SECURITY_ATTRIBUTES :=
           (nLength             => System.Win32.SECURITY_ATTRIBUTES'Size / 8,
            pSecurityDescriptor => System.Null_Address,
            bInheritHandle      => System.Win32.TRUE);

         Name : constant Interfaces.C.char_array := Interfaces.C.To_C
           (Windows_API.Pipe_Name_Prefix &
              "my_" & Boolean'Image (Inbound));

      begin
         Parent_Handle := Windows_API.CreateNamedPipeA
           (lpName               => Name,
            dwOpenMode           => Mode (Inbound) +
              Windows_API.FILE_FLAG_OVERLAPPED,
            dwPipeMode           => Windows_API.PIPE_TYPE_BYTE,
            nMaxInstances        => Windows_API.PIPE_UNLIMITED_INSTANCES,
            nOutBufferSize       => 4096,
            nInBufferSize        => 4096,
            nDefaultTimeOut      => 0,
            lpSecurityAttributes => null);

         if Parent_Handle = System.Win32.INVALID_HANDLE_VALUE then
            Self.Listener.Error_Occurred (Integer (System.Win32.GetLastError));
            Child_Handle := Parent_Handle;
            Success := False;
            return;
         end if;

         Child_Handle := System.Win32.CreateFileA
           (lpFileName            => Name'Address,
            dwDesiredAccess       => Client_Access (Inbound),
            dwShareMode           => 0,
            lpSecurityAttributes  => Inherit'Access,
            dwCreationDisposition => System.Win32.OPEN_EXISTING,
            dwFlagsAndAttributes  => System.Win32.FILE_ATTRIBUTE_NORMAL,
            hTemplateFile         => 0);

         if Child_Handle = System.Win32.INVALID_HANDLE_VALUE then
            Self.Listener.Error_Occurred (Integer (System.Win32.GetLastError));
            Success := False;
            return;
         end if;
      end Create_Pipe;

      ------------------
      -- Create_Pipes --
      ------------------

      function Create_Pipes
        (Start : access Windows_API.STARTUPINFOW) return Boolean
      is
         Child : constant array (Standard_Pipe) of
           not null access Windows_API.HANDLE :=
             (Stdin  => Start.hStdInput'Access,
              Stdout => Start.hStdOutput'Access,
              Stderr => Start.hStdError'Access);

         Ok : Boolean := True;
      begin
         for J in Standard_Pipe loop
            Self.pipe (J).Process := Self'Unchecked_Access;
            Self.pipe (J).Kind := J;
            Create_Pipe
              (Parent_Handle => Self.pipe (J).Handle,
               Child_Handle  => Child (J).all,
               Inbound       => J /= Stdin,
               Success       => Ok);

            exit when not Ok;
         end loop;

         return Ok;
      end Create_Pipes;

      --------------
      -- If_Error --
      --------------

      function Is_Error (Value : Windows_API.BOOL) return Boolean is
      begin
         if Value = System.Win32.FALSE then
            Self.Listener.Error_Occurred (Integer (System.Win32.GetLastError));
            return True;
         else
            return False;
         end if;
      end Is_Error;

      -----------------------
      -- Make_Command_Line --
      -----------------------

      function Make_Command_Line return Interfaces.C.wchar_array is
         Result : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

      begin
         Append_Escaped_String (Result, Self.Program);

         for Arg of Self.Arguments loop
            Ada.Strings.Wide_Unbounded.Append (Result, ' ');
            Append_Escaped_String
              (Result, Ada.Strings.Unbounded.To_Unbounded_String (Arg));
         end loop;

         return Interfaces.C.To_C
           (Ada.Strings.Wide_Unbounded.To_Wide_String (Result));
      end Make_Command_Line;

      ------------------
      -- Request_Read --
      ------------------

      procedure Request_Read (Kind : Standard_Pipe) is
      begin
         if Is_Error
           (Read_Write_Ex.ReadFileEx
              (hFile                => Self.pipe (Kind).Handle,
               lpBuffer             => Self.pipe (Kind).Buffer,
               nNumberOfBytesToRead => Self.pipe (Kind).Buffer'Length,
               lpOverlapped         => Self.pipe (Kind)'Access,
               lpCompletionRoutine  => Callback (Kind)))
         then
            null;  --  then what?
         end if;
      end Request_Read;

      --------------------
      -- Work_Directory --
      --------------------

      function Work_Directory return String is
         Directory : constant String :=
           Ada.Strings.Unbounded.To_String (Self.Directory);
      begin
         if Directory = "" then
            return ".";
         else
            return Directory;
         end if;
      end Work_Directory;

      Start : aliased Windows_API.STARTUPINFOW :=
        (cb      => Windows_API.STARTUPINFOW'Size / 8,
         dwFlags => Windows_API.STARTF_USESTDHANDLES,
         others  => <>);

      Exe : constant Interfaces.C.wchar_array :=
        Interfaces.C.To_C
          (Ada.Strings.UTF_Encoding.Wide_Strings.Decode
             (Ada.Strings.Unbounded.To_String (Self.Program)));

      Args : Interfaces.C.wchar_array := Make_Command_Line;

      Env  : constant Interfaces.C.wchar_array :=
        Spawn.Environments.Internal.Raw (Self.Environment);

      Dir  : constant Interfaces.C.wchar_array :=
        Interfaces.C.To_C
          (Ada.Strings.UTF_Encoding.Wide_Strings.Decode
             (Work_Directory));
   begin
      if not Create_Pipes (Start'Access) then
         return;
      end if;

      if Is_Error
        (Windows_API.CreateProcessW
           (lpApplicationName    => Exe,
            lpCommandLine        => Args,
            lpProcessAttributes  => null,
            lpThreadAttributes   => null,
            bInheritHandles      => System.Win32.TRUE,
            dwCreationFlags      => Windows_API.CREATE_NO_WINDOW +
              Windows_API.CREATE_UNICODE_ENVIRONMENT,
            lpEnvironment        => Env,
            lpCurrentDirectory   => Dir,
            lpStartupInfo        => Start'Access,
            lpProcessInformation => Self.pid'Access))
      then
         return;
      end if;

      On_Start.all;

      Self.Status := Running;
      Self.Listener.Started;
      Self.Listener.Standard_Input_Available;
      Request_Read (Stdout);
      Request_Read (Stderr);
   end Do_Start_Process;

   --------------
   -- Do_Write --
   --------------

   procedure Do_Write
     (Self       : in out Process'Class;
      Data       : Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      On_No_Data : access procedure)
   is
      use type Ada.Streams.Stream_Element_Count;

      Pipe  : Internal.Context renames Self.pipe (Stdin);
      Count : constant Ada.Streams.Stream_Element_Offset := Pipe.Last;
      Min   : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Min
          (Internal.Stream_Element_Buffer'Length, Data'Length);
   begin
      Last := Data'First - 1;

      if Count = 0 then
         --  Buffer isn't busy, we can write
         Last := Data'First + Min - 1;
         Pipe.Buffer (1 .. Min) := Data (Data'First .. Last);
         Pipe.Last := Min;

         On_No_Data.all;
      elsif Count in Internal.Stream_Element_Buffer'Range then
         --  Buffer is busy, mark stdin as 'send notification'
         Pipe.Last := Pipe.Last + Internal.Stream_Element_Buffer'Last;
      end if;
   end Do_Write;

   -----------------
   -- IO_Callback --
   -----------------

   procedure IO_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context;
      Kind                      : Standard_Pipe)
   is
      use type Windows_API.DWORD;
      use type Ada.Streams.Stream_Element_Count;

      Self : Process'Class renames
        Process'Class (lpOverlapped.Process.all);
      Last : Ada.Streams.Stream_Element_Count := lpOverlapped.Last;
   begin
      if dwErrorCode /= 0 then
         if not (Self.Status = Not_Running
                 and then dwErrorCode = Windows_API.ERROR_OPERATION_ABORTED)
         then
            Self.Listener.Error_Occurred (Integer (dwErrorCode));
         end if;

         return;
      end if;

      case Kind is
         when Stdin =>
            lpOverlapped.Last := 0;

            if Last not in Internal.Stream_Element_Buffer'Range then
               Last := Last - Internal.Stream_Element_Buffer'Last;
               Self.Listener.Standard_Input_Available;
            end if;

         when Stderr =>
            lpOverlapped.Last := Ada.Streams.Stream_Element_Count
              (dwNumberOfBytesTransfered);

            Self.Listener.Standard_Error_Available;
         when Stdout =>
            lpOverlapped.Last := Ada.Streams.Stream_Element_Count
              (dwNumberOfBytesTransfered);

            Self.Listener.Standard_Output_Available;
      end case;
   end IO_Callback;

   ---------------------
   -- On_Process_Died --
   ---------------------

   procedure On_Process_Died (Self : in out Process'Class) is

      function Is_Error (Value : Windows_API.BOOL) return Boolean;

      function To_Integer is new Ada.Unchecked_Conversion
        (Windows_API.DWORD, Integer);

      --------------
      -- If_Error --
      --------------

      function Is_Error (Value : Windows_API.BOOL) return Boolean is
         use type Windows_API.BOOL;
      begin
         if Value = System.Win32.FALSE then
            Self.Listener.Error_Occurred
              (Integer (System.Win32.GetLastError));
            return True;
         else
            return False;
         end if;
      end Is_Error;

      Exit_Code : aliased Windows_API.DWORD := 0;
   begin
      --  Close stdio pipes
      for J in Self.pipe'Range loop
         Windows.Do_Close_Pipe (Self, J);
      end loop;

      if not Is_Error
        (Windows_API.GetExitCodeProcess
           (Self.pid.hProcess, Exit_Code'Access))
        and then not Is_Error
          (System.Win32.CloseHandle (Self.pid.hProcess))
          and then not Is_Error
            (System.Win32.CloseHandle (Self.pid.hThread))
      then
         Self.Exit_Code := To_Integer (Exit_Code);
         Self.Status := Not_Running;
         Self.Listener.Finished (Self.Exit_Code);
      end if;
   end On_Process_Died;

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context) is
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
      lpOverlapped              : access Internal.Context) is
   begin
      Windows.IO_Callback
        (dwErrorCode, dwNumberOfBytesTransfered, lpOverlapped, Stdout);
   end Standard_Output_Callback;

end Spawn.Processes.Windows;
