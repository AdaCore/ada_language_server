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

with Ada.Streams;

with Glib.Error;
with Glib.Main;

package Glib.IO_Channels is

   type Channel_Record (<>) is limited private;
   type Channel is access all Channel_Record
     with Convention => C;

   function Unix_New (fd : Interfaces.C.int) return Channel
     with Import, Convention => C, External_Name => "g_io_channel_unix_new";
   --  Creates a new GIOChannel given a file descriptor. On UNIX systems this
   --  works for plain files, pipes, and sockets.
   --  The returned GIOChannel has a reference count of 1.
   --
   --  The default encoding for GIOChannel is UTF-8. If your application is
   --  reading output from a command using via pipe, you may need to set the
   --  encoding to the encoding of the current locale (see g_get_charset())
   --  with the g_io_channel_set_encoding() function. By default, the fd passed
   --  will not be closed when the final reference to the GIOChannel data
   --  structure is dropped.
   --
   --  If you want to read raw binary data without interpretation, then call
   --  the g_io_channel_set_encoding() function with NULL for the encoding
   --  argument.
   --
   --  This function is available in GLib on Windows, too, but you should avoid
   --  using it on Windows. The domain of file descriptors and sockets overlap.
   --  There is no way for GLib to know which one you mean in case the argument
   --  you pass to this function happens to be both a valid file descriptor and
   --  socket. If that happens a warning is issued, and GLib assumes that it is
   --  the file descriptor you mean.

   type GIOCondition is new Interfaces.C.int;

   --  A bitwise combination representing a condition to watch for on an event
   --  source.

   G_IO_IN : constant GIOCondition := 1;
   --  There is data to read.
   G_IO_OUT : constant GIOCondition := 2;
   --  Data can be written (without blocking).
   G_IO_PRI  : constant GIOCondition := 4;
   --  There is urgent data to read.
   G_IO_ERR : constant GIOCondition := 8;
   --  Error condition.
   G_IO_HUP : constant GIOCondition := 16;
   --  Hung up (the connection has been broken, usually for pipes and sockets).
   G_IO_NVAL : constant GIOCondition := 32;
   --  Invalid request. The file descriptor is not open.

   generic
      type User_Data is limited private;
   function Generic_Add_Watch
     (channel    : IO_Channels.Channel;
      condition  : GIOCondition;
      callback   : access function
         (source    : IO_Channels.Channel;
          condition : GIOCondition;
          data      : access User_Data) return Glib.Gboolean;  --  Conv???
      data       : access User_Data) return Glib.Main.G_Source_Id
     with Import, Convention => C, External_Name => "g_io_add_watch";
   --
   --  Adds the GIOChannel into the default main loop context with the default
   --  priority.
   --
   --  the callback should return FALSE if the event source should be removed

   type GIOFlags is new Interfaces.C.int;
   G_IO_FLAG_NONBLOCK : constant GIOFlags := 2;
   --  turns on nonblocking mode, corresponds to O_NONBLOCK/O_NDELAY

   type GIOStatus is new Interfaces.C.int;
   G_IO_STATUS_ERROR  : constant GIOStatus := 0;
   G_IO_STATUS_NORMAL : constant GIOStatus := 1;
   G_IO_STATUS_EOF    : constant GIOStatus := 2;
   G_IO_STATUS_AGAIN  : constant GIOStatus := 3;

   function Set_Flags
     (channel : IO_Channels.Channel;
      flags   : GIOFlags;
      error   : access Glib.Error.GError) return GIOStatus
       with Import, Convention => C, External_Name => "g_io_channel_set_flags";
   --  Sets the (writeable) flags in channel to (flags & G_IO_FLAG_SET_MASK).

   function Read_Chars
     (channel    : IO_Channels.Channel;
      buf        : out Ada.Streams.Stream_Element_Array;
      count      : Glib.Gsize;
      bytes_read : access Glib.Gsize;
      error      : access Glib.Error.GError) return GIOStatus
       with Import, Convention => C,
            External_Name => "g_io_channel_read_chars";
   --  Replacement for g_io_channel_read() with the new API.

   function Write_Chars
     (channel    : IO_Channels.Channel;
      buf        : Ada.Streams.Stream_Element_Array;
      count      : Glib.Gsize;
      bytes_read : access Glib.Gsize;
      error      : access Glib.Error.GError) return GIOStatus
       with Import, Convention => C,
            External_Name => "g_io_channel_write_chars";
   --  Replacement for g_io_channel_write() with the new API.
   --
   --  On seekable channels with encodings other than NULL or UTF-8,
   --  generic mixing of reading and writing is not allowed. A call to
   --  g_io_channel_write_chars() may only be made on a channel from which
   --  data has been read in the cases described in the documentation for
   --  g_io_channel_set_encoding().

   function Set_Encoding
     (channel  : IO_Channels.Channel;
      encoding : access Gchar;
      error    : access Glib.Error.GError) return GIOStatus
       with Import, Convention => C,
            External_Name => "g_io_channel_set_encoding";
   --  Sets the encoding for the input/output of the channel. The internal
   --  encoding is always UTF-8. The default encoding for the external file
   --  is UTF-8.
   --
   --  The encoding NULL is safe to use with binary data.

   procedure Set_Buffered
     (channel  : IO_Channels.Channel;
      buffered : Glib.Gboolean)
       with Import, Convention => C,
            External_Name => "g_io_channel_set_buffered";
   --  The buffering state can only be set if the channel's encoding is NULL.
   --  For any other encoding, the channel must be buffered.
   --
   --  A buffered channel can only be set unbuffered if the channel's
   --  internal buffers have been flushed. Newly created channels or channels
   --  which have returned G_IO_STATUS_EOF not require such a flush. For
   --  write-only channels, a call to g_io_channel_flush() is sufficient.
   --  For all other channels, the buffers may be flushed by a call to
   --  g_io_channel_seek_position(). This includes the possibility of seeking
   --  with seek type G_SEEK_CUR and an offset of zero. Note that this means
   --  that socket-based channels cannot be set unbuffered once they have had
   --  data read from them.
   --
   --  On unbuffered channels, it is safe to mix read and write calls from the
   --  new and old APIs, if this is necessary for maintaining old code.
   --
   --  The default state of the channel is buffered.

   function Shutdown
     (channel  : IO_Channels.Channel;
      flush    : Glib.Gboolean;
      error    : access Glib.Error.GError) return GIOStatus
       with Import, Convention => C, External_Name => "g_io_channel_shutdown";
   --  Close an IO channel. Any pending data to be written will be flushed if
   --  flush is TRUE. The channel will not be freed until the last reference
   --  is dropped using g_io_channel_unref().

   procedure Unref (channel : IO_Channels.Channel)
     with Import, Convention => C, External_Name => "g_io_channel_unref";
   --  Decrements the reference count of a GIOChannel.

private

   type Channel_Record is limited null record;

end Glib.IO_Channels;
