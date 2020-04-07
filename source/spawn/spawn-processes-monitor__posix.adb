------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Interrupts.Names;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with Spawn.Posix;
with Spawn.Environments.Internal;
with Interfaces.C.Strings;

package body Spawn.Processes.Monitor is
   use type Interfaces.C.int;

   type Process_Access is access all Process'Class;

   procedure Start_Process (Self : Process_Access);

   procedure Do_Close_Pipe
     (Self : Process_Access;
      Kind : Standard_Pipe);

   procedure My_IO_Callback
     (Process : Process_Access;
      Kind    : Pipe_Kinds);

   procedure My_End_Callback
     (Process : Process_Access;
      Kind    : Pipe_Kinds);

   procedure Check_Children;

   package Command_Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (Command);

   package Command_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Command_Queue_Interfaces);

   Queue : Command_Queues.Queue;

   function Hash (Value : Interfaces.C.int) return Ada.Containers.Hash_Type;

   package Process_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Interfaces.C.int,
      Element_Type    => Process_Access,
      Hash            => Hash,
      Equivalent_Keys => Interfaces.C."=",
      "="             => "=");

   Map : Process_Maps.Map;

   Pipe_Flags : constant Interfaces.C.int := Posix.O_CLOEXEC;

   protected SIGCHLD is
      entry Wait;

      procedure Handle
        with Interrupt_Handler,
             Attach_Handler => Ada.Interrupts.Names.SIGCHLD;
   private
      Fired : Boolean := False;
   end SIGCHLD;

   protected body SIGCHLD is

      entry Wait when Fired is
      begin
         Fired := False;
      end Wait;

      procedure Handle is
      begin
         Fired := True;
      end Handle;

   end SIGCHLD;

   package Poll is
      procedure Wake_Up;

      procedure Add_Descriptor
        (Process : Process_Access;
         Kind    : Pipe_Kinds;
         fd      : Interfaces.C.int);

      procedure Remove_Descriptor
        (Process : Process_Access;
         Kind    : Pipe_Kinds);

      procedure Watch_Pipe
        (Self : Process_Access;
         Kind : Standard_Pipe);

      procedure Wait
        (Timeout     : Interfaces.C.int;
         IO_Callback : access procedure
           (Process : Process_Access;
            Kind    : Pipe_Kinds);
         End_Callback : access procedure
           (Process : Process_Access;
            Kind    : Pipe_Kinds));
   end Poll;

   package body Poll is
      type Info is record
         Process : Process_Access;
         Kind    : Pipe_Kinds;
      end record;

      package Info_Vectors is new Ada.Containers.Vectors (Positive, Info);

      type pollfd_array_access is access all Posix.pollfd_array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Posix.pollfd_array, pollfd_array_access);

      List  : Info_Vectors.Vector;
      fds   : pollfd_array_access;
      Last  : Natural := 0;
      wake  : Interfaces.C.int := 0;

      procedure Swap (K, J : Positive);

      ----------
      -- Swap --
      ----------

      procedure Swap (K, J : Positive) is
         Save : constant Posix.pollfd := fds (K);
      begin
         fds (K) := fds (J);
         fds (J) := Save;
         List.Swap (K, J);
         List (J).Process.Index (List (J).Kind) := J;
         List (K).Process.Index (List (K).Kind) := K;
      end Swap;

      --------------------
      -- Add_Descriptor --
      --------------------

      procedure Add_Descriptor
        (Process : Process_Access;
         Kind    : Pipe_Kinds;
         fd      : Interfaces.C.int)
      is
         Event_Map : constant array (Pipe_Kinds) of
           Interfaces.C.unsigned_short :=
             (Stdin => 0,  --  We don't monitor this until buffer is full
              others => Posix.POLLIN);
      begin
         if fds = null then
            --  Make pipe for wake up poll and initialize fds
            declare
               Value  : Posix.Fd_Pair;
               Result : constant Interfaces.C.int :=
                 Posix.pipe2 (Value, Pipe_Flags);
            begin
               if Result /= 0 then
                  raise Program_Error with GNAT.OS_Lib.Errno_Message;
               end if;

               fds := new Posix.pollfd_array (1 .. 5);
               fds (1) :=
                 (fd      => Value (Posix.Read_End),
                  events  => Posix.POLLIN,
                  revents => 0);
               wake := Value (Posix.Write_End);
               List.Append ((null, Kind)); --  Dummy value
               Last := 1;
            end;
         elsif fds'Length < Last + 1 then
            --  Grow fds by factor of 1.5
            declare
               Old : pollfd_array_access := fds;
            begin
               fds := new Posix.pollfd_array (1 .. fds'Last * 3 / 2);
               fds (Old'Range) := Old.all;
               Free (Old);
            end;
         end if;

         Last := Last + 1;
         Process.Index (Kind) := Last;

         fds (Last) :=
           (fd      => fd,
            events  => Event_Map (Kind),
            revents => 0);

         if List.Last_Index < Last then
            List.Append ((Process, Kind));
         else
            List (Last) := (Process, Kind);
         end if;
      end Add_Descriptor;

      -----------------------
      -- Remove_Descriptor --
      -----------------------

      procedure Remove_Descriptor
        (Process : Process_Access;
         Kind    : Pipe_Kinds)
      is
         Index : constant Natural := Process.Index (Kind);
      begin
         if Index > 0 then
            fds (Index) := fds (Last);
            List (Index) := List (Last);
            Last := Last - 1;
            List (Index).Process.Index (List (Index).Kind) := Index;
            List.Set_Length (Ada.Containers.Count_Type (Last));
            Process.Index (Kind) := 0;
         end if;
      end Remove_Descriptor;

      ----------
      -- Wait --
      ----------

      procedure Wait
        (Timeout     : Interfaces.C.int;
         IO_Callback : access procedure
           (Process : Process_Access;
            Kind    : Pipe_Kinds);
         End_Callback : access procedure
           (Process : Process_Access;
            Kind    : Pipe_Kinds))
      is
         use type Interfaces.C.unsigned_short;

         Launch_Index : Natural;
         Process      : Process_Access;

         Kind  : Pipe_Kinds;
         Index : Positive := 2;
         --  Wait for an event in the poll
         Count : Interfaces.C.int := Posix.poll
           (fds.all, Interfaces.C.unsigned_long (Last), Timeout);
      begin
         --  Check if ve have wake up call
         if fds (1).revents /= 0 then
            declare
               Data   : Ada.Streams.Stream_Element_Array (1 .. 16);
               Ignore : Interfaces.C.size_t;
            begin
               Ignore := Posix.read (fds (1).fd, Data, Data'Length);
               Count  := Count - 1;
            end;
         end if;

         while Index <= Last loop
            if fds (Index).revents = 0 then
               Index := Index + 1;
            else
               Count := Count - 1;
               Kind := List (Index).Kind;
               Process := List (Index).Process;
               Launch_Index := Process.Index (Launch);

               if fds (Index).revents /= 0
                 and then Kind /= Launch
                 and then Launch_Index > Index
               then
                  --  Process Launch events first
                  Swap (Index, Launch_Index);
                  Kind := Launch;
               end if;

               if (fds (Index).revents and fds (Index).events) /= 0 then
                  IO_Callback (Process, Kind);
                  fds (Index).revents :=
                    fds (Index).revents - fds (Index).events;
                  fds (Index).events := 0;  --  Do nothing until users action
               end if;

               if fds (Index).revents /= 0 then
                  --  Some error happend
                  End_Callback (Process, Kind);
                  --  Don't listen this fd since error
                  Remove_Descriptor (Process, Kind);
               else
                  Index := Index + 1;
               end if;

               exit when Count = 0;
            end if;
         end loop;
      end Wait;

      -------------
      -- Wake_Up --
      -------------

      procedure Wake_Up is
      begin
         if wake /= 0 then
            declare
               use type Interfaces.C.size_t;
               Result : constant Interfaces.C.size_t :=
                 Posix.write (wake, (1 => 0), 1);
            begin
               if Result /= 1 then
                  raise Program_Error with GNAT.OS_Lib.Errno_Message;
               end if;
            end;
         end if;
      end Wake_Up;

      ----------------
      -- Watch_Pipe --
      ----------------

      procedure Watch_Pipe
        (Self : Process_Access;
         Kind : Standard_Pipe)
      is
         Event_Map : constant array (Standard_Pipe) of
           Interfaces.C.unsigned_short :=
             (Stdin => Posix.POLLOUT,
              others => Posix.POLLIN);
         Index : constant Natural := Self.Index (Kind);
      begin
         if Index > 0 then
            fds (Index).events := Event_Map (Kind);
         end if;
      end Watch_Pipe;

   end Poll;

   --------------------
   -- Check_Children --
   --------------------

   procedure Check_Children is
      use type Interfaces.C.unsigned;

      status : aliased Interfaces.C.unsigned := 0;
      pid    : constant Interfaces.C.int :=
        Posix.waitpid (-1, status'Unchecked_Access, Posix.WNOHANG);

      Cursor  : constant Process_Maps.Cursor := Map.Find (pid);
      Process : Process_Access;
   begin
      if Process_Maps.Has_Element (Cursor) then
         Process := Process_Maps.Element (Cursor);
         Process.Exit_Code := Integer (status / 256 and 16#FF#);
         Process.Status := Not_Running;
         Process.Listener.Finished (Process.Exit_Code);
      end if;
   end Check_Children;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : Process_Access;
      Kind : Standard_Pipe)
   is
      Ignore : Interfaces.C.int := Posix.close (Self.pipe (Kind));
   begin
      Poll.Remove_Descriptor (Self, Kind);
   end Do_Close_Pipe;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (Value : Command) is
   begin
      Queue.Enqueue (Value);
      Poll.Wake_Up;
   end Enqueue;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Interfaces.C.int) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (abs Value);
   end Hash;

   ----------------
   -- Loop_Cycle --
   ----------------

   procedure Loop_Cycle (Timeout : Integer) is
      use type Ada.Containers.Count_Type;
      Command : Monitor.Command;
   begin
      select
         SIGCHLD.Wait;
         Check_Children;
      else
         null;
      end select;

      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Command);

         case Command.Kind is
            when Start =>
               Start_Process (Process_Access (Command.Process));
            when Close_Pipe =>
               Do_Close_Pipe (Command.Process, Command.Pipe);
            when Watch_Pipe =>
               Poll.Watch_Pipe (Command.Process, Command.Pipe);
         end case;
      end loop;

      Poll.Wait
        (Interfaces.C.int (Timeout),
         My_IO_Callback'Access,
         My_End_Callback'Access);
   end Loop_Cycle;

   ---------------------
   -- My_End_Callback --
   ---------------------

   procedure My_End_Callback
     (Process : Process_Access;
      Kind    : Pipe_Kinds) is
   begin
      if Kind = Launch then
         if Process.Exit_Code = -1 then
            Process.Status := Running;
            Process.Listener.Started;
            Process.Listener.Standard_Input_Available;
         else
            Process.Listener.Error_Occurred (Process.Exit_Code);
         end if;
      end if;
   end My_End_Callback;

   --------------------
   -- My_IO_Callback --
   --------------------

   procedure My_IO_Callback
     (Process : Process_Access;
      Kind    : Pipe_Kinds)
   is
   begin
      case Kind is
         when Stdin =>
            Process.Listener.Standard_Input_Available;
         when Stdout =>
            Process.Listener.Standard_Output_Available;
         when Stderr =>
            Process.Listener.Standard_Error_Available;
         when Launch =>
            declare
               use type Ada.Streams.Stream_Element_Offset;
               use type Interfaces.C.size_t;

               Count      : Interfaces.C.size_t;
               errno      : Integer := 0;
               Error_Dump : Ada.Streams.Stream_Element_Array
                 (1 .. errno'Size / 8)
                 with Import, Convention => Ada, Address => errno'Address;
            begin
               Count := Posix.read
                 (Process.pipe (Kind),
                  Error_Dump,
                  Error_Dump'Length);

               if Count = Error_Dump'Length then
                  Process.Exit_Code := errno;
               end if;
            end;
      end case;
   end My_IO_Callback;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process (Self : Process_Access) is
      use Ada.Strings.Unbounded;
      use type Interfaces.C.Strings.chars_ptr;
      use type Ada.Streams.Stream_Element_Offset;

      use all type Posix.Pipe_Ends;

      procedure Send_Errno with No_Return;
      --  Put errno into Launch pipe end abort process
      procedure Prepare_Arguments (argv : out Posix.chars_ptr_array);
      --  Allocate argumnets
      procedure Free (argv : out Posix.chars_ptr_array);
      --  Deallocate argumnets

      --------------------
      -- Free_Arguments --
      --------------------

      procedure Free (argv : out Posix.chars_ptr_array) is
      begin
         for J in argv'Range loop
            Interfaces.C.Strings.Free (argv (J));
         end loop;
      end Free;

      -----------------------
      -- Prepare_Arguments --
      -----------------------

      procedure Prepare_Arguments (argv : out Posix.chars_ptr_array) is
      begin
         argv (0) := Interfaces.C.Strings.New_String
           (To_String (Self.Program));

         for J in 1 .. Self.Arguments.Last_Index loop
            argv (J) := Interfaces.C.Strings.New_String
              (Self.Arguments.Element (J));
         end loop;

         argv (argv'Last) := Interfaces.C.Strings.Null_Ptr;
      end Prepare_Arguments;

      std : array (Pipe_Kinds) of Posix.Fd_Pair;
      Child_Ends : constant array (std'Range) of Posix.Pipe_Ends :=
        (Posix.Read_End, others => Posix.Write_End);
      Parent_Ends : constant array (std'Range) of Posix.Pipe_Ends :=
        (Posix.Write_End, others => Posix.Read_End);
      Dup : constant array (Stdin .. Stderr) of Interfaces.C.int := (0, 1, 2);
      r : Interfaces.C.int;
      pragma Unreferenced (r);

      ----------------
      -- Send_Errno --
      ----------------

      procedure Send_Errno is
         count : Interfaces.C.size_t;
         pragma Unreferenced (count);
         errno : Integer;
         Error_Dump : Ada.Streams.Stream_Element_Array (1 .. errno'Size / 8)
           with Import, Convention => Ada, Address => errno'Address;
      begin
         errno := GNAT.OS_Lib.Errno;
         count := Posix.write
           (std (Launch) (Child_Ends (Launch)),
            Error_Dump,
            Error_Dump'Length);
         GNAT.OS_Lib.OS_Exit (127);
      end Send_Errno;

      pid  : Interfaces.C.int;
      dir  : Interfaces.C.Strings.chars_ptr :=
        (if Length (Self.Directory) = 0 then Interfaces.C.Strings.Null_Ptr
           else Interfaces.C.Strings.New_String
             (To_String (Self.Directory)));

      argv : Posix.chars_ptr_array (0 .. Natural (Self.Arguments.Length) + 1);
      envp : Posix.chars_ptr_array :=
        Spawn.Environments.Internal.Raw (Self.Environment);
   begin
      --  Create pipes for children's strio
      if (for some X of std => Posix.pipe2 (X, Pipe_Flags) /= 0) then
         Self.Listener.Error_Occurred (GNAT.OS_Lib.Errno);
         Interfaces.C.Strings.Free (dir);
         return;
      end if;

      Prepare_Arguments (argv);

      pid := Posix.fork;

      if pid = -1 then
         --  Fork failed
         Self.Listener.Error_Occurred (GNAT.OS_Lib.Errno);
         Free (argv);
         Free (envp);
         Interfaces.C.Strings.Free (dir);
         return;
      elsif pid = 0 then  --  Child process
         --  Close unused ends
         if (for some X in std'Range =>
               Posix.close (std (X) (Parent_Ends (X))) /= 0)
         then
            Send_Errno;
         --  Copy fd to standard numbers
         elsif (for some X in Dup'Range =>
                  Posix.dup2 (std (X) (Child_Ends (X)), Dup (X)) = -1)
         then
            Send_Errno;
         --  Change directory if needed
         elsif dir /= Interfaces.C.Strings.Null_Ptr
           and then Posix.chdir (dir) /= 0
         then
            Send_Errno;
         else  --  Replace executable
            r := Posix.execve (argv (0), argv, envp);
            Send_Errno;
         end if;
      end if;

      --  Parent process
      Free (argv);
      Free (envp);
      Interfaces.C.Strings.Free (dir);

      --  Close unused ends
      if (for some X in std'Range =>
            Posix.close (std (X) (Child_Ends (X))) /= 0)
      then
         Self.Listener.Error_Occurred (GNAT.OS_Lib.Errno);
         return;
      end if;

      --  Make stdio non-blocking
      if (for some X in Standard_Pipe =>
            Posix.fcntl
              (std (X) (Parent_Ends (X)),
               Posix.F_SETFL,
               Posix.O_NONBLOCK) /= 0)
      then
         Self.Listener.Error_Occurred (GNAT.OS_Lib.Errno);
         return;
      end if;

      Self.pid := pid;
      Map.Insert (pid, Self);

      for X in Self.pipe'Range loop
         Self.pipe (X) := std (X) (Parent_Ends (X));
         Poll.Add_Descriptor (Self, X, std (X) (Parent_Ends (X)));
      end loop;
   end Start_Process;

   procedure Initialize;
   --  Do low level initialization if needed

   procedure Dummy is null;
   --  This is to be used in Initialize procedure

   procedure Initialize is separate;

begin
   Initialize;
end Spawn.Processes.Monitor;
