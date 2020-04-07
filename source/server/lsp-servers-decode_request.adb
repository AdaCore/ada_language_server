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

with Ada.Tags; use Ada.Tags;
with Ada.Strings.UTF_Encoding;
with Ada.Tags.Generic_Dispatching_Constructor;

with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Server_Requests; use LSP.Messages.Server_Requests;

function LSP.Servers.Decode_Request
   (Document : GNATCOLL.JSON.JSON_Value)
    return LSP.Messages.Server_Requests.Server_Request'Class
is
   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Messages.Server_Requests.Server_Request,
      Parameters  => LSP.JSON_Streams.JSON_Stream,
      Constructor => LSP.Messages.Server_Requests.Decode);

   JS : aliased LSP.JSON_Streams.JSON_Stream (Is_Server_Side => True);
   JSON_Array : GNATCOLL.JSON.JSON_Array;

   Method     : LSP.Types.LSP_String;
   Tag        : Ada.Tags.Tag;

begin
   GNATCOLL.JSON.Append (JSON_Array, Document);
   JS.Set_JSON_Document (JSON_Array);
   JS.Start_Object;

   LSP.Types.Read_String (JS, +"method", Method);
   Tag := LSP.Messages.Server_Requests.Method_To_Tag (Method);

   if Tag in Ada.Tags.No_Tag then
      raise Unknown_Method;
   else
      return Constructor (Tag, JS'Access);
   end if;
end LSP.Servers.Decode_Request;
