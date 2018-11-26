------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with System;

with Glib;
with Glib.Error;
with Glib.Main;
with Gtkada.Bindings;
with Gtkada.Types;

package Glib.Spawns is

   type GPid is new Interfaces.C.ptrdiff_t;

   function g_get_environ return Gtkada.Bindings.chars_ptr_array_access
     with Import, Convention => C, External_Name => "g_get_environ";
   --  Gets the list of environment variables for the current process.
   --
   --  The list is NULL terminated and each item in the list is of the form
   --  'NAME=VALUE'.
   --
   --  This is equivalent to direct access to the 'environ' global variable,
   --  except portable.
   --
   --  The return value is freshly allocated and it should be freed with
   --  g_strfreev() when it is no longer needed.

   type GSpawnFlags is new Interfaces.C.int;

   G_SPAWN_DO_NOT_REAP_CHILD : constant GSpawnFlags := 2;
   --  the child will not be automatically reaped; you must use
   --  g_child_watch_add() yourself (or call waitpid() or handle
   --  SIGCHLD yourself), or the child will become a zombie.

   function g_spawn_async_with_pipes
     (working_directory : Gtkada.Types.Chars_Ptr;
      argv              : Gtkada.Types.Chars_Ptr_Array;
      envp              : Gtkada.Types.Chars_Ptr_Array;
      flags             : GSpawnFlags;
      child_setup       : System.Address := System.Null_Address;
      user_data         : System.Address := System.Null_Address;
      child_pid         : access Glib.Spawns.GPid;
      standard_input    : access Interfaces.C.int;
      standard_output   : access Interfaces.C.int;
      standard_error    : access Interfaces.C.int;
      error             : access Glib.Error.GError)
      return Glib.Gboolean
        with Import, Convention => C,
             External_Name => "g_spawn_async_with_pipes";
   --
   --  Executes a child program asynchronously (your program will not block
   --  waiting for the child to exit).
   --
   --  See full descriptin in Glib documentation.

   procedure g_spawn_close_pid
     (pid : Glib.Spawns.GPid)
       with Import, Convention => C, External_Name => "g_spawn_close_pid";
   --
   --  On some platforms, notably Windows, the GPid type represents a resource
   --  which must be closed to prevent resource leaking. g_spawn_close_pid()
   --  is provided for this purpose. It should be used on all platforms, even
   --  though it doesn't do anything under UNIX.

   generic
      type User_Data is limited private;
   function Generic_Child_Add_Watch
     (pid      : Glib.Spawns.GPid;
      callback : access procedure
        (pid    : Glib.Spawns.GPid;
         status : Glib.Gint;
         data   : access User_Data);
      data   : access User_Data) return Glib.Main.G_Source_Id
        with Import, Convention => C, External_Name => "g_child_watch_add";
   --  Sets a function to be called when the child indicated by pid exits, at a
   --  default priority, G_PRIORITY_DEFAULT.
   --
   --  If you obtain pid from g_spawn_async() or g_spawn_async_with_pipes()
   --  you will need to pass G_SPAWN_DO_NOT_REAP_CHILD as flag to the spawn
   --  function for the child watching to work.
   --
   --  Note that on platforms where GPid must be explicitly closed (see
   --  g_spawn_close_pid()) pid must not be closed while the source is still
   --  active. Typically, you will want to call g_spawn_close_pid() in the
   --  callback function for the source.
   --
   --  GLib supports only a single callback per process id. On POSIX platforms,
   --  the same restrictions mentioned for g_child_watch_source_new() apply to
   --  this function.
   --
   --  This internally creates a main loop source using
   --  g_child_watch_source_new() and attaches it to the main loop context
   --  using g_source_attach(). You can do these steps manually if you need
   --  greater control.

   function g_spawn_check_exit_status
     (exit_status : Glib.Gint;
      error       : access Glib.Error.GError)
      return Glib.Gboolean
        with Import, Convention => C,
             External_Name => "g_spawn_check_exit_status";
   --
   --  Set error if exit_status indicates the child exited abnormally (e.g.
   --  with a nonzero exit code, or via a fatal signal).
   --
   --  The g_spawn_sync() and g_child_watch_add() family of APIs return an exit
   --  status for subprocesses encoded in a platform-specific way. On Unix,
   --  this is guaranteed to be in the same format waitpid() returns, and on
   --  Windows it is guaranteed to be the result of GetExitCodeProcess().

end Glib.Spawns;
