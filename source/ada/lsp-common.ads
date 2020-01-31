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

--  A place for commonly used utilities, such as trace or debug functions.

with Ada.Exceptions;
with GNATCOLL.Traces;
with LSP.Messages;
with LSP.Types;           use LSP.Types;
with Libadalang.Analysis; use Libadalang.Analysis;

package LSP.Common is

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Parent => True, others => False));
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Child => True, others => False));
   --  Convenient constants

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "");
   --  Log an exception in the given traces, with an optional message

   function Get_Hover_Text (Node : Ada_Node'Class) return LSP_String;
   --  Return a pretty printed version of the node's text to be displayed on
   --  hover requests, removing unnecessary indentation whitespaces if needed
   --  and attaching extra information in some cases.

end LSP.Common;
