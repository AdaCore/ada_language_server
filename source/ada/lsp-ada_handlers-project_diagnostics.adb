------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with VSS.Strings;

package body LSP.Ada_Handlers.Project_Diagnostics is

   Single_Project_Found_Message : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("Unique project in root directory was found and " &
         "loaded, but it wasn't explicitly configured.");

   No_Project_Found_Message : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("No project found in root directory. " &
        "Please create a project file and add it to the configuration.");

   Multiple_Projects_Found_Message : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("No project was loaded, because more than one project file has been " &
        "found in the root directory. Please change configuration to point " &
        "a correct project file.");

   Invalid_Project_Configured_Message : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String
       ("Project file has error and can't be loaded.");

   --------------------
   -- Get_Diagnostic --
   --------------------

   overriding procedure Get_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector)
   is
      Item : LSP.Messages.Diagnostic;
   begin
      Self.Last_Status := Self.Handler.Project_Status;
      Item.span := ((0, 0), (0, 0));
      Item.source := (True, "project");
      Item.severity := (True, LSP.Messages.Error);

      case Self.Last_Status is
         when Valid_Project_Configured =>
            null;
         when Single_Project_Found =>
            Item.message := Single_Project_Found_Message;
            Item.severity := (True, LSP.Messages.Hint);
            Errors.Append (Item);
         when No_Project_Found =>
            Item.message := No_Project_Found_Message;
            Errors.Append (Item);
         when Multiple_Projects_Found =>
            Item.message := Multiple_Projects_Found_Message;
            Errors.Append (Item);
         when Invalid_Project_Configured =>
            Item.message := Invalid_Project_Configured_Message;
            Errors.Append (Item);
      end case;
   end Get_Diagnostic;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context)
      return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Self.Last_Status /= Self.Handler.Project_Status;
   end Has_New_Diagnostic;

end LSP.Ada_Handlers.Project_Diagnostics;
