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

with Interfaces.C;

with Spawn.Environments.Internal;

with Glib.Error;
with Glib.IO_Channels;
with Glib.Main;
with Glib.Spawns;
with Gtkada.Types;

package body Spawn.Processes is

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Standard_Pipe);

   procedure Do_Start_Process (Self : aliased in out Process'Class);

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe);

   function IO_Watch is new Glib.IO_Channels.Generic_Add_Watch
     (User_Data => Internal.Process_Reference);

   function Child_Watch is new Glib.Spawns.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Collback
     (pid    : Glib.Spawns.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   function My_IO_Callback
     (source    : Glib.IO_Channels.Channel;
      condition : Glib.IO_Channels.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
        with Convention => C;

   type Process_Access is access all Process'Class;

   Map : constant array (Standard_Pipe) of Glib.IO_Channels.GIOCondition :=
     (Stdin  => Glib.IO_Channels.G_IO_OUT,
      Stdout => Glib.IO_Channels.G_IO_IN,
      Stderr => Glib.IO_Channels.G_IO_IN);

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
      Do_Close_Pipe (Self, Stderr);
   end Close_Standard_Error;

   --------------------------
   -- Close_Standard_Input --
   --------------------------

   procedure Close_Standard_Input (Self : in out Process'Class) is
   begin
      Do_Close_Pipe (Self, Stdin);
   end Close_Standard_Input;

   ---------------------------
   -- Close_Standard_Output --
   ---------------------------

   procedure Close_Standard_Output (Self : in out Process'Class) is
   begin
      Do_Close_Pipe (Self, Stdout);
   end Close_Standard_Output;

   -------------------
   -- Do_Close_Pipe --
   -------------------

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Standard_Pipe)
   is
      use type Glib.IO_Channels.Channel;
      use type Glib.IO_Channels.GIOStatus;
      Pipe  : Internal.Pipe_Record renames Self.pipe (Kind);
      Error : aliased Glib.Error.GError;
   begin
      if Pipe.Channel /= null then
         if Glib.IO_Channels.Shutdown (Pipe.Channel, 1, Error'Access)
           = Glib.IO_Channels.G_IO_STATUS_NORMAL
         then
            Glib.IO_Channels.Unref (Pipe.Channel);
            Pipe.Channel := null;
         else
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
         end if;
      end if;
   end Do_Close_Pipe;

   -------------
   -- Do_Read --
   -------------

   procedure Do_Read
     (Self : in out Process'Class;
      Data : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset;
      Kind : Standard_Pipe)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Pipe   : Internal.Pipe_Record renames Self.pipe (Kind);
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize := 0;
      Status : constant Glib.IO_Channels.GIOStatus :=
        Glib.IO_Channels.Read_Chars
          (channel    => Pipe.Channel,
           buf        => Data,
           count      => Data'Length,
           bytes_read => Count'Access,
           error      => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event not in Glib.Main.No_Source_Id;
   begin
      case Status is
         when Glib.IO_Channels.G_IO_STATUS_EOF =>
            --  Reading is completed, so no watching is required
            Last := Data'First - 1;

         when Glib.IO_Channels.G_IO_STATUS_NORMAL =>
            --  Read success, so no watching is required
            Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

         when Glib.IO_Channels.G_IO_STATUS_AGAIN =>
            --  No data to read, so start to watching again
            pragma Assert (Count in 0);
            Last := Data'First - 1;

            if In_Callback then
               --  Ask IO_Callback to continue watching
               Pipe.Watch := True;
            else
               --  Start watching here
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (Kind),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;
         when Glib.IO_Channels.G_IO_STATUS_ERROR =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
         when others =>
            raise Program_Error;
      end case;
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      use Ada.Strings.Unbounded;
      use type Interfaces.C.size_t;
      use type Glib.IO_Channels.GIOStatus;

      procedure Prepare_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array);
      --  Allocate argumnets

      -----------------------
      -- Prepare_Arguments --
      -----------------------

      procedure Prepare_Arguments (argv : out Gtkada.Types.Chars_Ptr_Array) is
      begin
         argv (0) := Gtkada.Types.New_String
           (To_String (Self.Program));

         for J in 1 .. Self.Arguments.Last_Index loop
            argv (Interfaces.C.size_t (J)) := Gtkada.Types.New_String
              (Self.Arguments.Element (J));
         end loop;

         argv (argv'Last) := Gtkada.Types.Null_Ptr;
      end Prepare_Arguments;

      dir  : Gtkada.Types.Chars_Ptr :=
        (if Length (Self.Directory) = 0 then Gtkada.Types.Null_Ptr
           else Gtkada.Types.New_String
             (To_String (Self.Directory)));

      argv : Gtkada.Types.Chars_Ptr_Array
        (0 .. Interfaces.C.size_t (Self.Arguments.Length) + 1);

      envp : Gtkada.Types.Chars_Ptr_Array :=
        Spawn.Environments.Internal.Raw (Self.Environment);

      Error : aliased Glib.Error.GError;
   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Prepare_Arguments (argv);

      if Glib.Spawns.g_spawn_async_with_pipes
        (working_directory => dir,
         argv              => argv,
         envp              => envp,
         flags             => Glib.Spawns.G_SPAWN_DO_NOT_REAP_CHILD,
         child_pid         => Self.pid'Access,
         standard_input    => Self.pipe (Stdin).FD'Access,
         standard_output   => Self.pipe (Stdout).FD'Access,
         standard_error    => Self.pipe (Stderr).FD'Access,
         error             => Error'Access) in 0
      then
         Self.Listener.Error_Occurred (Integer (Glib.Error.Get_Code (Error)));
         return;
      end if;

      Gtkada.Types.Free (argv);
      Gtkada.Types.Free (envp);
      Gtkada.Types.Free (dir);

      --  Create IO Channels and make them non-blocking
      for J in Standard_Pipe loop
         declare
            Pipe : Spawn.Internal.Pipe_Record renames Self.pipe (J);
         begin
            Pipe.Channel := Glib.IO_Channels.Unix_New (Pipe.FD);

            if Glib.IO_Channels.Set_Flags
              (Pipe.Channel,
               Glib.IO_Channels.G_IO_FLAG_NONBLOCK,
               Error'Access) /= Glib.IO_Channels.G_IO_STATUS_NORMAL
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            elsif Glib.IO_Channels.Set_Encoding
              (Pipe.Channel,
               null,
               Error'Access) /= Glib.IO_Channels.G_IO_STATUS_NORMAL
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            end if;

            Glib.IO_Channels.Set_Buffered (Pipe.Channel, 0);

            if J /= Stdin then
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (J),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;
         end;
      end loop;

      Self.Event := Child_Watch
        (Self.pid,
         My_Death_Collback'Access,
         Self.Reference'Access);

      Self.Status := Running;
      Self.Listener.Started;
      Self.Listener.Standard_Input_Available;
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
     (pid    : Glib.Spawns.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
   is
      use type Glib.Gboolean;

      Process : constant Process_Access := Process_Access (data.Self);
      Error   : aliased Glib.Error.GError;
   begin
      for J in Standard_Pipe loop
         Do_Close_Pipe (Process.all, J);
      end loop;

      Glib.Spawns.g_spawn_close_pid (pid);

      if Glib.Spawns.g_spawn_check_exit_status
        (status, Error'Access) = 0
      then
         Process.Exit_Code := Integer (Glib.Error.Get_Code (Error));
      else
         Process.Exit_Code := 0;
      end if;

      Process.Status := Not_Running;
      Process.Listener.Finished (Process.Exit_Code);
   end My_Death_Collback;

   --------------------
   -- My_IO_Callback --
   --------------------

   function My_IO_Callback
     (source    : Glib.IO_Channels.Channel;
      condition : Glib.IO_Channels.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      use type Glib.IO_Channels.Channel;
      Process : constant Process_Access := Process_Access (data.Self);
      Watch   : Glib.Gboolean := 0;
   begin
      case condition is
         when Glib.IO_Channels.G_IO_IN =>
            if Process.pipe (Stdout).Channel = source then
               pragma Assert
                 (Process.pipe (Stdout).Event not in Glib.Main.No_Source_Id);

               Process.pipe (Stdout).Watch := False;
               Process.Listener.Standard_Output_Available;

               if Process.pipe (Stdout).Watch then
                  Watch := 1;
               else
                  Process.pipe (Stdout).Event := Glib.Main.No_Source_Id;
                  Watch := 0;
               end if;
            else
               pragma Assert
                 (Process.pipe (Stderr).Event not in Glib.Main.No_Source_Id);

               Process.pipe (Stderr).Watch := False;
               Process.Listener.Standard_Error_Available;

               if Process.pipe (Stderr).Watch then
                  Watch := 1;
               else
                  Process.pipe (Stderr).Event := Glib.Main.No_Source_Id;
                  Watch := 0;
               end if;
            end if;
         when Glib.IO_Channels.G_IO_OUT =>
            pragma Assert
              (Process.pipe (Stdin).Event not in Glib.Main.No_Source_Id);
            Process.pipe (Stdout).Watch := False;
            Process.Listener.Standard_Input_Available;

            if Process.pipe (Stdout).Watch then
               Watch := 1;
            else
               Process.pipe (Stdout).Event := Glib.Main.No_Source_Id;
               Watch := 0;
            end if;
         when others =>
            raise Program_Error;
      end case;

      return Watch;
   end My_IO_Callback;

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
      use type Ada.Streams.Stream_Element_Offset;
      Pipe   : Internal.Pipe_Record renames Self.pipe (Stdin);
      Error  : aliased Glib.Error.GError;
      Count  : aliased Glib.Gsize;

      Status : constant Glib.IO_Channels.GIOStatus :=
        Glib.IO_Channels.Write_Chars
          (channel    => Pipe.Channel,
           buf        => Data,
           count      => Data'Length,
           bytes_read => Count'Access,
           error      => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event not in Glib.Main.No_Source_Id;
   begin
      case Status is
         when Glib.IO_Channels.G_IO_STATUS_NORMAL =>
            Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;


         when Glib.IO_Channels.G_IO_STATUS_AGAIN =>
            --  No space in the buffer to write, so start watching again
            pragma Assert (Count in 0);
            Last := Data'First - 1;

            if In_Callback then
               --  Ask IO_Callback to continue watching
               Pipe.Watch := True;
            else
               --  Start watching here
               Pipe.Event := IO_Watch
                 (Pipe.Channel,
                  Map (Stdin),
                  My_IO_Callback'Access,
                  Self.Reference'Access);
            end if;

         when Glib.IO_Channels.G_IO_STATUS_ERROR =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));

         when others =>
            raise Program_Error;
      end case;
   end Write_Standard_Input;

end Spawn.Processes;
