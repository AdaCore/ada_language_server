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

with Ada.Characters.Latin_1;
with Ada.Streams;            use Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;

with Spawn.Processes; use Spawn.Processes;

package body LSP.Raw_Clients is

   New_Line : constant String :=
     (Ada.Characters.Latin_1.CR, Ada.Characters.Latin_1.LF);

   ----------------------
   -- Can_Send_Message --
   ----------------------

   function Can_Send_Message (Self : Raw_Client'Class) return Boolean is
   begin
      return Self.Is_Server_Running and then Self.Standard_Input_Available;
   end Can_Send_Message;

   --------------------
   -- Error_Occurred --
   --------------------

   overriding procedure Error_Occurred
     (Self : in out Listener; Process_Error : Integer) is
   begin
      Self.Client.On_Error (GNAT.OS_Lib.Errno_Message (Process_Error));
   end Error_Occurred;

   ---------------
   -- Exit_Code --
   ---------------

   function Exit_Code
     (Self : Raw_Client'Class)
      return Spawn.Processes.Process_Exit_Code is
   begin
      return Self.Server.Exit_Code;
   end Exit_Code;

   -----------------------
   -- Is_Server_Running --
   -----------------------

   function Is_Server_Running (Self : Raw_Client'Class) return Boolean is
   begin
      return Self.Server.Status in Spawn.Processes.Running;
   end Is_Server_Running;

   -------------------------------
   -- On_Standard_Error_Message --
   -------------------------------

   procedure On_Standard_Error_Message
     (Self : in out Raw_Client;
      Text : String)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put (Text);
   end On_Standard_Error_Message;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer
     (Self : in out Raw_Client'Class;
      Text : VSS.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Image   : constant String := Ada.Streams.Stream_Element_Count'Image
        (Text.Length);

      Header  : constant String := "Content-Length:" & Image
        & New_Line & New_Line;
   begin
      Ada.Strings.Unbounded.Append (Self.To_Write, Header);
      Ada.Strings.Unbounded.Append
        (Self.To_Write,
         VSS.Stream_Element_Vectors.Conversions.Unchecked_To_String (Text));

      if Self.Standard_Input_Available then
         Self.Listener.Standard_Input_Available;
      end if;
   end Send_Buffer;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Self : in out Raw_Client'Class;
      Text : Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;

      Image   : constant String := Positive'Image (Length (Text));
      Header  : constant String := "Content-Length:" & Image
        & New_Line & New_Line;
   begin
      Ada.Strings.Unbounded.Append (Self.To_Write, Header);
      Ada.Strings.Unbounded.Append (Self.To_Write, Text);

      if Self.Standard_Input_Available then
         Self.Listener.Standard_Input_Available;
      end if;
   end Send_Message;

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
     (Self      : in out Raw_Client'Class;
      Arguments :        Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Server.Set_Arguments (Arguments);
   end Set_Arguments;

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self        : in out Raw_Client'Class;
      Environment :        Spawn.Environments.Process_Environment) is
   begin
      Self.Server.Set_Environment (Environment);
   end Set_Environment;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program
     (Self    : in out Raw_Client'Class;
      Program :        Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      Self.Server.Set_Program (Program);
   end Set_Program;

   ---------------------------
   -- Set_Working_Directory --
   ---------------------------

   procedure Set_Working_Directory
     (Self      : in out Raw_Client'Class;
      Directory :        Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      Self.Server.Set_Working_Directory (Directory);
   end Set_Working_Directory;

   ------------------------------
   -- Standard_Error_Available --
   ------------------------------

   overriding procedure Standard_Error_Available (Self : in out Listener) is
   begin
      loop
         declare
            Raw  : Ada.Streams.Stream_Element_Array (1 .. 1024);
            Last : Ada.Streams.Stream_Element_Count;
            Text : String (1 .. 1024) with Import, Address => Raw'Address;
         begin
            Self.Client.Server.Read_Standard_Error (Raw, Last);
            exit when Last in 0;
            Self.Client.On_Standard_Error_Message (Text (1 .. Natural (Last)));
         end;
      end loop;
   end Standard_Error_Available;

   ------------------------------
   -- Standard_Input_Available --
   ------------------------------

   overriding procedure Standard_Input_Available (Self : in out Listener) is
      Client       : Raw_Clients.Raw_Client'Class renames Self.Client.all;
      To_Write_Len : constant Natural :=
        Ada.Strings.Unbounded.Length (Client.To_Write);
      --  Total length of the output data

      Rest_Length  : Natural := To_Write_Len - Client.Written;
      --  Number of bytes to be written, excluding already sent bytes
   begin
      while Rest_Length > 0 loop
         declare
            Size  : constant Positive := Positive'Min (Rest_Length, 1024);
            --  Restrict output to reasonable size to avoid stack overflow
            Slice : constant String := Ada.Strings.Unbounded.Slice
              (Client.To_Write, Client.Written + 1, Client.Written + Size);
            Raw   : constant Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Count (Size))
                with Import, Address => Slice'Address;
            Last  : Natural;

         begin
            Client.Server.Write_Standard_Input
              (Raw, Ada.Streams.Stream_Element_Count (Last));

            Client.Written := Client.Written + Last;
            Rest_Length := Rest_Length - Last;

            if Last /= Natural (Raw'Last) then
               --  Standard_Input is busy now. Let's wait for the next call of
               --  Standard_Input_Available
               Client.Standard_Input_Available := False;

               return;
            end if;
         end;
      end loop;

      Client.Written := 0;
      Client.To_Write := Ada.Strings.Unbounded.Null_Unbounded_String;
      Client.Standard_Input_Available := True;
   end Standard_Input_Available;

   -------------------------------
   -- Standard_Output_Available --
   -------------------------------

   overriding procedure Standard_Output_Available (Self : in out Listener) is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;

      procedure Parse_Headers
        (Buffer : String; Content_Length : out Positive);
      --  Parse headers in Buffer and return Content-Length

      Client : Raw_Clients.Raw_Client'Class renames Self.Client.all;

      -------------------
      -- Parse_Headers --
      -------------------

      procedure Parse_Headers
        (Buffer         : String;
         Content_Length : out Positive)
      is
         function Skip (Pattern : String) return Boolean;
         --  Find Pattern in current position of Buffer and skip it

         Next : Natural := Buffer'First;
         --  Current position in Buffer

         ----------
         -- Skip --
         ----------

         function Skip (Pattern : String) return Boolean is
         begin
            if Next + Pattern'Length - 1 <= Buffer'Last
              and then Buffer (Next .. Next + Pattern'Length - 1) = Pattern
            then
               Next := Next + Pattern'Length;
               return True;
            else
               return False;
            end if;
         end Skip;

      begin
         while Next < Buffer'Last loop
            if Skip ("Content-Type: ") then
               Next := Index (Buffer, New_Line);
               pragma Assert (Next /= 0);
            elsif Skip ("Content-Length: ") then
               declare
                  From : constant Positive := Next;
               begin
                  Next := Index (Buffer, New_Line);
                  pragma Assert (Next /= 0);
                  Content_Length := Positive'Value (Buffer (From .. Next - 1));
               end;
            else
               raise Constraint_Error with "Unexpected header:" & Buffer;
            end if;

            Next := Next + New_Line'Length;
         end loop;
      end Parse_Headers;

   begin
      loop
         declare
            Raw  : Ada.Streams.Stream_Element_Array (1 .. 1024);
            Last : Ada.Streams.Stream_Element_Count;
            Text : String (1 .. Raw'Length)
              with Import, Address => Raw'Address;
            Start : Natural;
         begin
            Client.Server.Read_Standard_Output (Raw, Last);

            exit when Last in 0;

            Append (Client.Buffer, Text (1 .. Positive (Last)));

            loop
               if Client.To_Read = 0 then
                  --  Look for end of header list
                  Start := Index (Client.Buffer, New_Line & New_Line);

                  if Start /= 0 then
                     Parse_Headers
                       (Slice (Client.Buffer, 1, Start + 1),
                        Client.To_Read);

                     Delete
                       (Client.Buffer, 1, Start + 2 * New_Line'Length - 1);
                  end if;
               end if;

               exit when Client.To_Read = 0
                 or else Length (Client.Buffer) < Client.To_Read;

               declare
                  Text    : constant Ada.Strings.Unbounded.Unbounded_String :=
                    Head (Client.Buffer, Client.To_Read);
                  Success : Boolean := True;

               begin
                  Client.On_Raw_Message (Text, Success);

                  if not Success then
                     raise Program_Error
                       with VSS.Strings.Conversions.To_UTF_8_String
                              (Client.Error_Message);
                  end if;

                  Delete (Client.Buffer, 1, Client.To_Read);
                  Client.To_Read := 0;
               end;
            end loop;
         end;
      end loop;
   end Standard_Output_Available;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Raw_Client'Class) is
   begin
      Self.Server.Set_Listener (Self.Listener'Unchecked_Access);
      Self.Server.Start;
   end Start;

   -------------
   -- Started --
   -------------

   overriding procedure Started (Self : in out Listener) is
   begin
      Self.Client.On_Started;
   end Started;

   --------------
   -- Finished --
   --------------

   overriding procedure Finished
     (Self        : in out Listener;
      Exit_Status : Spawn.Processes.Process_Exit_Status;
      Exit_Code   : Spawn.Processes.Process_Exit_Code)
   is
      pragma Unreferenced (Exit_Code);

   begin
      Self.Client.Standard_Input_Available := False;
      Self.Client.On_Finished;
   end Finished;

   ------------------------
   -- Exception_Occurred --
   ------------------------

   overriding procedure Exception_Occurred
     (Self       : in out Listener;
      Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Self.Client.On_Exception (Occurrence);
   end Exception_Occurred;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Raw_Client'Class) is
   begin
      Self.Server.Close_Standard_Input;
   end Stop;

end LSP.Raw_Clients;
