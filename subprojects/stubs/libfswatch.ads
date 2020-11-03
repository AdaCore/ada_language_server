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

--  This package contains stubs for the libfswatch API

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Libfswatch is

   Libfswatch_Error : exception;
   --  Used to report any library exception

   type Event_Flags is
     (No_Op,
      Platform_Specific,
      Created,
      Updated,
      Removed,
      Renamed,
      Owner_Modified,
      Attribute_Modified,
      Moved_From,
      Moved_To,
      Is_File,
      Is_Dir,
      Is_Sym_Link,
      Link,
      Overflow);

   package Event_Flags_Vectors is
     new Ada.Containers.Vectors (Natural, Event_Flags);

   type Event is record
      Path  : Unbounded_String;
      Flags : Event_Flags_Vectors.Vector;
      --  TODO: add a platform-independent time representation
   end record;

   package Event_Vectors is new Ada.Containers.Vectors (Natural, Event);
   type Event_Flags_Array is array (Natural range <>) of Event_Flags;

   type Root_Event_Monitor is abstract tagged private;

   procedure Callback (Self   : in out Root_Event_Monitor;
                       Events : Event_Vectors.Vector) is abstract;

   procedure Blocking_Monitor
     (Monitor        : in out Root_Event_Monitor'Class;
      Paths          : File_Array;
      Events_Allowed : Event_Flags_Array := (1 .. 0 => No_Op)) is null;

   procedure Stop_Monitor (Monitor : in out Root_Event_Monitor'Class) is null;
   --  Interrupt the monitoring. This is thread-safe.

private

   type Root_Event_Monitor is abstract tagged null record;

   type Event_Filter is new Integer;
   No_Filter : constant Event_Filter := 0;

end Libfswatch;
