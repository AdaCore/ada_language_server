------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Push_Writers;
with VSS.JSON.Streams;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with LSP.Client_Notifications;
with LSP.Client_Notification_Readers;
with LSP.Client_Request_Readers;
with LSP.Client_Requests;
with LSP.Client_Response_Readers;
with LSP.Client_Responses;
with LSP.Enumerations;
with LSP.Errors;
with LSP.JSON_Streams;
with LSP.Server_Notification_Writers;
with LSP.Server_Notifications;
with LSP.Server_Request_Writers;
with LSP.Server_Requests;
with LSP.Server_Response_Writers;
with LSP.Server_Responses;

package body LSP.Clients is

   -------------------------
   -- Allocate_Request_Id --
   -------------------------

   function Allocate_Request_Id
     (Self : in out Client'Class)
      return LSP.Structures.Integer_Or_Virtual_String
   is
      Id : VSS.Strings.Virtual_String := Self.Request_Id_Prefix;

   begin
      Self.Request_Id := Self.Request_Id + 1;

      if not Id.Is_Empty then
         return (True, Self.Request_Id);

      else
         declare
            Image : constant Wide_Wide_String :=
              Integer'Wide_Wide_Image (Self.Request_Id);

         begin
            Id.Append ('-');
            Id.Append
              (VSS.Strings.To_Virtual_String
                 (Image (Image'First + 1 .. Image'Last)));

            return (False, Id);
         end;
      end if;
   end Allocate_Request_Id;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Client) return VSS.Strings.Virtual_String is
   begin
      return Self.Error_Message;
   end Error_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Client'Class) is
   begin
      null;
   end Initialize;

   ----------------
   -- On_Message --
   ----------------

   overriding procedure On_Message
     (Self    : in out Client;
      Message : not null LSP.Server_Messages.Server_Message_Access)
   is
      Stream : aliased
        VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;
      Output : aliased VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;

   begin
      Output.Set_Stream (Stream'Unchecked_Access);
      Output.Start_Document;

      if Message.all in LSP.Server_Notifications.Server_Notification'Class then
         declare
            Writer : LSP.Server_Notification_Writers.Server_Notification_Writer
              (Output'Access);

         begin
            LSP.Server_Notifications.Server_Notification'Class
              (Message.all).Visit_Server_Receiver (Writer);
         end;

      elsif Message.all in LSP.Server_Requests.Server_Request'Class then
         declare
            Request : LSP.Server_Requests.Server_Request'Class
              renames LSP.Server_Requests.Server_Request'Class
                        (Message.all);

            Writer : LSP.Server_Request_Writers.Server_Request_Writer
                       (Output'Access);

         begin
            Request.Visit_Server_Receiver (Writer);

            --  XXX Should request id be allocated here ???
            Self.Request_Map.Insert (Request.Id, Request.Method);
         end;

      elsif Message.all in LSP.Server_Responses.Server_Response'Class then
         declare
            Writer : LSP.Server_Response_Writers.Server_Response_Writer
              (Output'Access);

         begin
            LSP.Server_Responses.Server_Response'Class
              (Message.all).Visit_Server_Receiver (Writer);
         end;

      else
         raise Program_Error;
      end if;

      Output.End_Document;

      Self.Send_Buffer (Stream.Buffer);
   end On_Message;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self    : in out Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      procedure Look_Ahead
        (Id       : out LSP.Structures.Integer_Or_Virtual_String;
         Method   : out LSP.Structures.Virtual_String_Optional;
         Token    : out LSP.Structures.Integer_Or_Virtual_String;
         Is_Error : in out Boolean);

      Memory : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Id       : out LSP.Structures.Integer_Or_Virtual_String;
         Method   : out LSP.Structures.Virtual_String_Optional;
         Token    : out Structures.Integer_Or_Virtual_String;
         Is_Error : in out Boolean)
      is
         use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

         R  : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS : aliased LSP.JSON_Streams.JSON_Stream (False, R'Unchecked_Access);

      begin
         R.Set_Stream (Memory'Unchecked_Access);
         R.Read_Next;
         pragma Assert (R.Is_Start_Document);
         R.Read_Next;
         pragma Assert (R.Is_Start_Object);
         R.Read_Next;

         while not R.Is_End_Object loop
            pragma Assert (R.Is_Key_Name);

            declare
               Key : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String (R.Key_Name);

            begin
               R.Read_Next;

               if Key = "id" then
                  case R.Element_Kind is
                     when String_Value =>
                        Id :=
                          (Is_Integer     => False,
                           Virtual_String => R.String_Value);

                     when Number_Value =>
                        Id :=
                          (Is_Integer => True,
                           Integer    =>
                              Integer (R.Number_Value.Integer_Value));

                     when others =>
                        raise Constraint_Error;
                  end case;

                  R.Read_Next;

               elsif Key = "method" then
                  pragma Assert (R.Is_String_Value);
                  Method := R.String_Value;
                  R.Read_Next;

               elsif Key = "error" then
                  Is_Error := True;
                  JS.Skip_Value;

               elsif Key = "params" then
                  --  parse 'params' object to get 'token' value
                  --  from a notification if any

                  pragma Assert (R.Is_Start_Object);
                  R.Read_Next;

                  while not R.Is_End_Object loop
                     pragma Assert (R.Is_Key_Name);
                     declare
                        Key : constant String :=
                          VSS.Strings.Conversions.To_UTF_8_String (R.Key_Name);
                     begin
                        R.Read_Next;

                        if Key = "token" then
                           case R.Element_Kind is
                              when String_Value =>
                                 Token :=
                                   (Is_Integer     => False,
                                    Virtual_String => R.String_Value);

                              when Number_Value =>
                                 Token :=
                                   (Is_Integer => True,
                                    Integer    =>
                                       Integer (R.Number_Value.Integer_Value));

                              when others =>
                                 raise Constraint_Error;
                           end case;
                           R.Read_Next;

                        else
                           JS.Skip_Value;
                        end if;
                     end;
                  end loop;
                  R.Read_Next;

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         Memory.Rewind;
      end Look_Ahead;

      Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => Reader'Unchecked_Access);
      Id     : LSP.Structures.Integer_Or_Virtual_String;
      Method : LSP.Structures.Virtual_String_Optional;
      Token  : LSP.Structures.Integer_Or_Virtual_String :=
        (Is_Integer => False, Virtual_String => <>);

      Is_Error : Boolean := False;

   begin
      Self.Error_Message.Clear;
      --  First, cleanup error message from previous value.

      Memory.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Id, Method, Token, Is_Error);
      Reader.Set_Stream (Memory'Unchecked_Access);
      Stream.R.Read_Next;
      pragma Assert (Stream.R.Is_Start_Document);
      Stream.R.Read_Next;
      pragma Assert (Stream.R.Is_Start_Object);

      if Id.Is_Integer or else not Id.Virtual_String.Is_Null then
         if not Method.Is_Null then
            --   Request from the server

            begin
               declare
                  Message : LSP.Client_Requests.Client_Request'Class :=
                    LSP.Client_Request_Readers.Read_Request
                      (Reader, Method);

               begin
                  Message.Visit_Client_Receiver (Self.Request_Handler.all);
               end;

            exception
               when Program_Error =>
                  declare
                     Template : VSS.Strings.Templates.Virtual_String_Template :=
                       "Unknown method: '{}'";
                     Error    : constant LSP.Errors.ResponseError :=
                       (code    => LSP.Enumerations.MethodNotFound,
                        message =>
                          Template.Format
                            (VSS.Strings.Formatters.Strings.Image (Method)));

                  begin
                     Self.Server_Factory.On_Error_Response (Id, Error);
                  end;
            end;

         else
            --  Response from server

            if Self.Request_Map.Contains (Id) then
               begin
                  declare
                     Message : LSP.Client_Responses.Client_Response'Class :=
                       LSP.Client_Response_Readers.Read_Response
                         (Reader, Self.Request_Map (Id));

                  begin
                     Message.Visit_Client_Receiver (Self.Response_Handler.all);
                  end;

               exception
                  when Program_Error =>
                     --  Should not happened.

                     null;
               end;

            else
               declare
                  Template : VSS.Strings.Templates.Virtual_String_Template :=
                    "Unknown request id '{}'";

               begin
                  Self.Error_Message :=
                    Template.Format
                      (Parameter =>
                         (if Id.Is_Integer
                          then VSS.Strings.Formatters.Integers.Image
                            (Id.Integer)
                          else VSS.Strings.Formatters.Strings.Image
                            (Id.Virtual_String)));

                  Success := False;
               end;
            end if;
         end if;

      elsif not Method.Is_Empty then
         --  Notification from server

         declare
            Message : LSP.Client_Notifications.Client_Notification'Class :=
              LSP.Client_Notification_Readers.Read_Notification
                (Reader, Method);

         begin
            Message.Visit_Client_Receiver (Self.Notification.all);
         end;
      end if;
   end On_Raw_Message;

   -----------------------
   -- Request_Id_Prefix --
   -----------------------

   function Request_Id_Prefix
     (Self : Client) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Request_Id_Prefix;

   ------------------------------
   -- Set_Notification_Handler --
   ------------------------------

   procedure Set_Notification_Handler
     (Self : in out Client'Class;
      To   : not null access
        Client_Notification_Receivers.Client_Notification_Receiver'Class) is
   begin
      Self.Notification := To;
   end Set_Notification_Handler;

   -------------------------
   -- Set_Request_Handler --
   -------------------------

   procedure Set_Request_Handler
     (Self : in out Client'Class;
      To   : not null access
        LSP.Client_Request_Receivers.Client_Request_Receiver'Class) is
   begin
      Self.Request_Handler := To;
   end Set_Request_Handler;

   --------------------------
   -- Set_Response_Handler --
   --------------------------

   procedure Set_Response_Handler
     (Self : in out Client'Class;
      To   : not null access
        LSP.Client_Response_Receivers.Client_Response_Receiver'Class) is
   begin
      Self.Response_Handler := To;
   end Set_Response_Handler;

end LSP.Clients;
