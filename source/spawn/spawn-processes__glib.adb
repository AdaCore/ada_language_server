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

with Interfaces.C;

with Spawn.Environments.Internal;

with Glib.Error;
with Glib.IOChannel;
with Glib.Main;
with Glib.Spawn;
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

   function IO_Watch is new Glib.IOChannel.Generic_Add_Watch
     (User_Data => Internal.Process_Reference);

   function Child_Watch is new Glib.Main.Generic_Child_Add_Watch
     (User_Data => Internal.Process_Reference);

   procedure My_Death_Collback
     (pid    : Glib.Spawn.GPid;
      status : Glib.Gint;
      data   : access Internal.Process_Reference)
        with Convention => C;

   function My_IO_Callback
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
        with Convention => C;

   type Process_Access is access all Process'Class;

   Map : constant array (Standard_Pipe) of Glib.IOChannel.GIOCondition :=
     (Stdin  => Glib.IOChannel.G_Io_Out,
      Stdout => Glib.IOChannel.G_Io_In,
      Stderr => Glib.IOChannel.G_Io_In);

   function Spawn_Async_With_Pipes is
     new Glib.Spawn.Generic_Spawn_Async_With_Pipes
       (User_Data => Integer);

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
      use type Glib.IOChannel.Giochannel;
      use type Glib.IOChannel.GIOStatus;
      Pipe  : Internal.Pipe_Record renames Self.pipe (Kind);
      Error : aliased Glib.Error.GError;
   begin
      if Pipe.Channel /= null then
         if Glib.IOChannel.Shutdown (Pipe.Channel, 1, Error'Access)
           = Glib.IOChannel.G_Io_Status_Normal
         then
            Glib.IOChannel.Unref (Pipe.Channel);
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
      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Read_Chars
          (Self       => Pipe.Channel,
           Buf        => Data,
           Bytes_Read => Count'Access,
           Error      => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event not in Glib.Main.No_Source_Id;
   begin
      case Status is
         when Glib.IOChannel.G_Io_Status_Eof =>
            --  Reading is completed, so no watching is required
            Last := Data'First - 1;

         when Glib.IOChannel.G_Io_Status_Normal =>
            --  Read success, so no watching is required
            Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;

         when Glib.IOChannel.G_Io_Status_Again =>
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
         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));
      end case;
   end Do_Read;

   ----------------------
   -- Do_Start_Process --
   ----------------------

   procedure Do_Start_Process (Self : aliased in out Process'Class) is
      use Ada.Strings.Unbounded;
      use type Interfaces.C.size_t;
      use type Glib.IOChannel.GIOStatus;

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

      argv : aliased Gtkada.Types.Chars_Ptr_Array :=
        (0 .. Interfaces.C.size_t (Self.Arguments.Length) + 1 => <>);

      envp : aliased Gtkada.Types.Chars_Ptr_Array :=
        Spawn.Environments.Internal.Raw (Self.Environment);

      Error : aliased Glib.Error.GError;
   begin
      Self.Reference.Self := Self'Unchecked_Access;
      Prepare_Arguments (argv);

      if Spawn_Async_With_Pipes
        (Working_Directory => dir,
         Argv              => argv'Access,
         Envp              => envp'Access,
         Flags             => Glib.Spawn.G_Spawn_Do_Not_Reap_Child,
         Child_Setup       => null,
         Data              => null,
         Child_Pid         => Self.pid'Access,
         Standard_Input    => Self.pipe (Stdin).FD'Access,
         Standard_Output   => Self.pipe (Stdout).FD'Access,
         Standard_Error    => Self.pipe (Stderr).FD'Access,
         Error             => Error'Access) in 0
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
            Pipe.Channel := Glib.IOChannel.Giochannel_Unix_New (Pipe.FD);

            if Glib.IOChannel.Set_Flags
              (Pipe.Channel,
               Glib.IOChannel.G_Io_Flag_Nonblock,
               Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            elsif Glib.IOChannel.Set_Encoding
              (Pipe.Channel,
               "",
               Error'Access) /= Glib.IOChannel.G_Io_Status_Normal
            then
               Self.Listener.Error_Occurred
                 (Integer (Glib.Error.Get_Code (Error)));
               return;
            end if;

            Glib.IOChannel.Set_Buffered (Pipe.Channel, False);

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
--           raise Program_Error;
         --  XXX commented out temporary to avoid finalization issues in GPS
         null;
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
      Process : constant Process_Access := Process_Access (data.Self);
      Error   : aliased Glib.Error.GError;
   begin
      for J in Standard_Pipe loop
         Do_Close_Pipe (Process.all, J);
      end loop;

      Glib.Spawn.Spawn_Close_Pid (pid);

      if Glib.Spawn.Spawn_Check_Exit_Status
        (status, Error'Access)
      then
         Process.Exit_Code := 0;
      else
         Process.Exit_Code := Integer (Glib.Error.Get_Code (Error));
      end if;

      Process.Status := Not_Running;
      Process.Listener.Finished (Process.Exit_Code);
   end My_Death_Collback;

   --------------------
   -- My_IO_Callback --
   --------------------

   function My_IO_Callback
     (source    : Glib.IOChannel.Giochannel;
      condition : Glib.IOChannel.GIOCondition;
      data      : access Internal.Process_Reference) return Glib.Gboolean
   is
      use type Glib.IOChannel.Giochannel;
      Process : constant Process_Access := Process_Access (data.Self);
      Watch   : Glib.Gboolean := 0;
   begin
      case condition is
         when Glib.IOChannel.G_Io_In =>
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
         when Glib.IOChannel.G_Io_Out =>
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

      Status : constant Glib.IOChannel.GIOStatus :=
        Glib.IOChannel.Write_Chars
          (Self          => Pipe.Channel,
           Buf           => Data,
           Bytes_Written => Count'Access,
           Error         => Error'Access);

      In_Callback : constant Boolean :=
        Pipe.Event not in Glib.Main.No_Source_Id;
   begin
      case Status is
         when Glib.IOChannel.G_Io_Status_Normal =>
            Last := Data'First + Ada.Streams.Stream_Element_Offset (Count) - 1;


         when Glib.IOChannel.G_Io_Status_Again =>
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

         when Glib.IOChannel.G_Io_Status_Error =>
            Self.Listener.Error_Occurred
              (Integer (Glib.Error.Get_Code (Error)));

         when others =>
            raise Program_Error;
      end case;
   end Write_Standard_Input;

end Spawn.Processes;
