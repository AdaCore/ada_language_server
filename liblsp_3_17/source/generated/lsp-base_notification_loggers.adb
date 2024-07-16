--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Base_Notification_Loggers is

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Base_Notification_Logger;
      Value : LSP.Structures.CancelParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'$/cancelRequest'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CancelRequest_Notification;

   overriding procedure On_Progress_Notification
     (Self  : in out Base_Notification_Logger;
      Value : LSP.Structures.ProgressParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'$/progress'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Progress_Notification;

end LSP.Base_Notification_Loggers;
