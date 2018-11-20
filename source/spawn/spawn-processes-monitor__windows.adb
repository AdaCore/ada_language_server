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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Environments.Internal;
with Spawn.Windows_API;

package body Spawn.Processes.Monitor is

   subtype Context is Internal.Context;
   subtype Stream_Element_Buffer is Internal.Stream_Element_Buffer;

   type Process_Access is access all Processes.Process'Class;

   procedure Start_Process (Self : access Processes.Process'Class);

   package Command_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (Command);

   package Command_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Command_Queue_Interfaces);

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

   generic
      Is_Error : Boolean;
   procedure Standard_Read_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
     with Convention => Stdcall;
   --  Implementation shared between Standard_[Output/Error]_Callback

   procedure Do_Watch_Pipe
     (Process : not null Process_Access;
      Kind    : Standard_Pipe);

   procedure Do_Close_Pipe
     (Process : not null Process_Access;
      Kind    : Standard_Pipe);

   function Listener (Self : access Context) return Process_Listener_Access;

   Callback : constant array (Stdout .. Stderr) of Read_Write_Ex.Callback :=
     (Standard_Output_Callback'Access,
      Standard_Error_Callback'Access);

   Queue : Command_Queues.Queue;

   package Poll is
      procedure Wake_Up;

      procedure Add_Process (Process : Process_Access);
      procedure Wait_Process_Death (Timeout : Integer);
   end Poll;

   package body Poll is
      package Info_Vectors is new Ada.Containers.Vectors
        (Positive, Process_Access);
      type pollfd_array_access is access all Windows_API.HANDLE_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Windows_API.HANDLE_Array, pollfd_array_access);

      List  : Info_Vectors.Vector;
      fds   : pollfd_array_access;
      Last  : Natural := 0;
      wake  : Windows_API.HANDLE := 0;

      -----------------
      -- Add_Process --
      -----------------

      procedure Add_Process (Process : Process_Access) is
      begin
         if fds = null then
            --  Make event for wake up poll and initialize fds
            wake := Windows_API.CreateEventW
              (lpSecurityAttributes => null,
               bManualReset         => System.Win32.FALSE,
               bInitialState        => System.Win32.FALSE,
               lpName               => null);
            fds := new Windows_API.HANDLE_Array (1 .. 5);
            fds (1) := wake;
            List.Append (null); --  Dummy value
            Last := 1;
         elsif fds'Length < Last + 1 then
            --  Grow fds by factor of 1.5
            declare
               Old : pollfd_array_access := fds;
            begin
               fds := new Windows_API.HANDLE_Array (1 .. fds'Last * 2 / 3);
               fds (Old'Range) := Old.all;
               Free (Old);
            end;
         end if;

         Last := Last + 1;
         Process.Index := Last;

         fds (Last) := Process.pid.hProcess;

         if List.Last_Index < Last then
            List.Append (Process);
         else
            List (Last) := (Process);
         end if;
      end Add_Process;

      ------------------------
      -- Wait_Process_Death --
      ------------------------

      procedure Wait_Process_Death (Timeout : Integer) is
         use type Windows_API.DWORD;
         function To_Integer is new Ada.Unchecked_Conversion
           (Windows_API.DWORD, Integer);

         function Is_Error (Value : Windows_API.BOOL) return Boolean;

         Result    : Windows_API.DWORD;
         Index     : Integer;
         Process   : Process_Access;
         Exit_Code : aliased Windows_API.DWORD := 0;

         --------------
         -- If_Error --
         --------------

         function Is_Error (Value : Windows_API.BOOL) return Boolean is
            use type Windows_API.BOOL;
         begin
            if Value = System.Win32.FALSE then
               Process.Listener.Error_Occurred
                 (Integer (System.Win32.GetLastError));
               return True;
            else
               return False;
            end if;
         end Is_Error;
      begin
         loop
            Result := Windows_API.WaitForMultipleObjectsEx
              (nCount         => Windows_API.DWORD (Last),
               lpHandles      => fds.all,
               bWaitAll       => System.Win32.FALSE,
               dwMilliseconds => Windows_API.DWORD (Timeout),
               bAlertable     => System.Win32.TRUE);

            if Result /= Windows_API.WAIT_IO_COMPLETION then
               exit;
            end if;
         end loop;

         Index := Integer (Result) + 1;

         if Result = Windows_API.WAIT_TIMEOUT then
            return;
         elsif Index = 1 then
            --  Wake up event triggered
            return;
         elsif Index <= Last then
            --  Some process died
            Process := List (Index);

            --  Close stdio pipes
            for J in Process.pipe'Range loop
               declare
                  use type Windows_API.HANDLE;
                  Handle : Windows_API.HANDLE renames Process.pipe (J).Handle;
               begin
                  if Handle /= System.Win32.INVALID_HANDLE_VALUE
                    and then not Is_Error (System.Win32.CloseHandle (Handle))
                  then
                     Handle := System.Win32.INVALID_HANDLE_VALUE;
                  end if;
               end;
            end loop;

            if not Is_Error
              (Windows_API.GetExitCodeProcess
                 (Process.pid.hProcess, Exit_Code'Access))
              and then not Is_Error
                (System.Win32.CloseHandle (Process.pid.hProcess))
                and then not Is_Error
                  (System.Win32.CloseHandle (Process.pid.hThread))
            then
               Process.Exit_Code := To_Integer (Exit_Code);
               Process.Status := Not_Running;
               Process.Listener.Finished (Process.Exit_Code);
            end if;
         else
            raise Program_Error with "WaitForMultipleObjectsEx FAILED";
         end if;
      end Wait_Process_Death;

      -------------
      -- Wake_Up --
      -------------

      procedure Wake_Up is
         use type Windows_API.BOOL;
         Result : constant Windows_API.BOOL := Windows_API.SetEvent (wake);
      begin
         pragma Assert (Result = System.Win32.FALSE);
      end Wake_Up;
   end Poll;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Process : not null Process_Access;
      Kind    : Standard_Pipe)
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
            Process.Listener.Error_Occurred
              (Integer (System.Win32.GetLastError));
         end if;
      end Check_Error;

      Handle : Windows_API.HANDLE renames Process.pipe (Kind).Handle;
   begin
      if Handle /= System.Win32.INVALID_HANDLE_VALUE then
         Check_Error (Windows_API.CancelIo (Process.pipe (Kind).Handle));
         Check_Error (System.Win32.CloseHandle (Process.pipe (Kind).Handle));
         Process.pipe (Kind).Handle := System.Win32.INVALID_HANDLE_VALUE;
      end if;
   end Do_Close_Pipe;

   -------------------
   -- Do_Watch_Pipe --
   -------------------

   procedure Do_Watch_Pipe
     (Process : not null Process_Access;
      Kind    : Standard_Pipe)
   is
      use type Ada.Streams.Stream_Element_Count;
      use type Windows_API.BOOL;
      Ok : Windows_API.BOOL;
      Last : Ada.Streams.Stream_Element_Count;
   begin
      case Kind is
         when Stdout | Stderr =>
            Ok := Read_Write_Ex.ReadFileEx
              (hFile                => Process.pipe (Kind).Handle,
               lpBuffer             => Process.pipe (Kind).Buffer,
               nNumberOfBytesToRead => Process.pipe (Kind).Buffer'Length,
               lpOverlapped         => Process.pipe (Kind)'Access,
               lpCompletionRoutine  => Callback (Kind));

            if Ok = System.Win32.FALSE then
               Process.Listener.Error_Occurred
                 (Integer (System.Win32.GetLastError));
            end if;
         when Stdin =>
            Last := Process.pipe (Kind).Last;

            if Last not in Stream_Element_Buffer'Range then
               Last := Last - Stream_Element_Buffer'Last;
            end if;

            Ok := Read_Write_Ex.WriteFileEx
              (hFile                 => Process.pipe (Kind).Handle,
               lpBuffer              => Process.pipe (Kind).Buffer,
               nNumberOfBytesToWrite => Windows_API.DWORD (Last),
               lpOverlapped          => Process.pipe (Kind)'Access,
               lpCompletionRoutine   => Standard_Input_Callback'Access);

            if Ok = System.Win32.FALSE then
               Process.Listener.Error_Occurred
                 (Integer (System.Win32.GetLastError));
            end if;
      end case;
   end Do_Watch_Pipe;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Value : Command) is
   begin
      Queue.Enqueue (Value);
      Poll.Wake_Up;
   end Enqueue;

   --------------
   -- Listener --
   --------------

   function Listener (Self : access Context) return Process_Listener_Access is
   begin
      return Process_Access (Self.Process).Listener;
   end Listener;

   ----------------
   -- Loop_Cycle --
   ----------------

   procedure Loop_Cycle (Timeout : Integer) is
      use type Ada.Containers.Count_Type;
      Command : Monitor.Command;
   begin
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Command);

         case Command.Kind is
            when Start =>
               Start_Process (Command.Process);
            when Close_Pipe =>
               Do_Close_Pipe (Command.Process, Command.Pipe);
            when Watch_Pipe =>
               Do_Watch_Pipe (Command.Process, Command.Pipe);
         end case;
      end loop;

      Poll.Wait_Process_Death (Timeout);
   end Loop_Cycle;

   ----------------------------
   -- Standard_Read_Callback --
   ----------------------------

   procedure Standard_Read_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
   is
      use type Windows_API.DWORD;
   begin
      if dwErrorCode /= 0 then
         Listener (lpOverlapped).Error_Occurred (Integer (dwErrorCode));
         return;
      end if;

      lpOverlapped.Last := Ada.Streams.Stream_Element_Count
        (dwNumberOfBytesTransfered);

      if Is_Error then
         Listener (lpOverlapped).Standard_Error_Available;
      else
         Listener (lpOverlapped).Standard_Output_Available;
      end if;
   end Standard_Read_Callback;

   -----------------------------
   -- Standard_Error_Callback --
   -----------------------------

   procedure Standard_Error_Callback_Instance is new
     Standard_Read_Callback (True);
   procedure Standard_Error_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        renames Standard_Error_Callback_Instance;

   ------------------------------
   -- Standard_Output_Callback --
   ------------------------------

   procedure Standard_Output_Callback_Instance is new
     Standard_Read_Callback (False);
   procedure Standard_Output_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
        renames Standard_Output_Callback_Instance;

   -----------------------------
   -- Standard_Input_Callback --
   -----------------------------

   procedure Standard_Input_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Context)
   is
      use type Windows_API.DWORD;
      use type Ada.Streams.Stream_Element_Count;

      Last : Ada.Streams.Stream_Element_Count := lpOverlapped.Last;
   begin
      if dwErrorCode /= 0 then
         Listener (lpOverlapped).Error_Occurred (Integer (dwErrorCode));
         return;
      end if;

      lpOverlapped.Last := 0;

      if Last not in Stream_Element_Buffer'Range then
         Last := Last - Stream_Element_Buffer'Last;
         Listener (lpOverlapped).Standard_Input_Available;
      end if;

      pragma Assert (Windows_API.DWORD (Last) = dwNumberOfBytesTransfered);
   end Standard_Input_Callback;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process (Self : access Processes.Process'Class) is
      function Make_Command_Line return Interfaces.C.wchar_array;
      function Work_Directory return String;
      function Escape (Text : Wide_String) return Wide_String;
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
            Self.pipe (J).Process := Self;
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

      ------------
      -- Escape --
      ------------

      function Escape (Text : Wide_String) return Wide_String is
      begin
         return Text;
      end Escape;

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
         Ada.Strings.Wide_Unbounded.Append
           (Result,
            Ada.Strings.UTF_Encoding.Wide_Strings.Decode
              (Ada.Strings.Unbounded.To_String (Self.Program)));

         for Arg of Self.Arguments loop
            Ada.Strings.Wide_Unbounded.Append (Result, ' ');

            Ada.Strings.Wide_Unbounded.Append
              (Result,
               Escape (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Arg)));
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

      Poll.Add_Process (Process_Access (Self));
      Self.Status := Running;
      Self.Listener.Started;
      Self.Listener.Standard_Input_Available;
      Request_Read (Stdout);
      Request_Read (Stderr);
   end Start_Process;

end Spawn.Processes.Monitor;
