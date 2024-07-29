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

with VSS.Strings;
with VSS.String_Vectors;

with LSP_Gen.Configurations;
with LSP_Gen.Progress_Reports;
with LSP_Gen.Puts; use LSP_Gen.Puts;
with LSP_Gen.Requests;
with LSP_Gen.Responses;

package body LSP_Gen.Factories is

   Work_Done_List : VSS.String_Vectors.Virtual_String_Vector renames
     LSP_Gen.Dependencies.Work_Done_List;

   procedure Write_Client_Factories
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map);

   procedure Write_Server_Factories
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map);

   -----------
   -- Write --
   -----------

   procedure Write
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map) is
   begin
      Write_Client_Factories (Model, Done);
      Write_Server_Factories (Model, Done);
   end Write;

   ----------------------------
   -- Write_Client_Factories --
   ----------------------------

   procedure Write_Client_Factories
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      Name : constant VSS.Strings.Virtual_String := "Client_Message_Factory";
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Client_Message_Receivers;");
      Put_Line ("with LSP.Client_Messages;");
      Put_Line ("with LSP.Errors;");
      Put_Line ("with LSP.Structures;");

      New_Line;

      Put_Line ("package LSP.Client_Message_Factories is");
      Put_Line ("pragma Preelaborate;");
      New_Line;
      Put_Line ("type Client_Message_Factory is abstract limited new");
      Put_Line ("LSP.Client_Message_Receivers.Client_Message_Receiver ");
      Put_Line ("with null record;");
      New_Line;

      Put_Line ("procedure On_Message");
      Put_Line ("(Self  : in out Client_Message_Factory;");
      Put_Line ("Value : LSP.Client_Messages.Client_Message_Access)");
      Put_Line (" is abstract;");
      New_Line;

      Put_Line ("overriding procedure On_Error_Response");
      Put_Line ("(Self  : in out Client_Message_Factory;");
      Put_Line ("Id    : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError);");
      New_Line;

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Server then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Notification");
            Put ("(Self : in out ");
            Put (Name);

            if Model.Notification (J).params.Is_Set then
               Put_Line (";");
               Put ("Value : LSP.Structures.");
               Put (Model.Notification (J).params.Value.Union.reference.name);
            end if;

            Put_Line (");");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Server then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Request");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

            if Model.Request (J).params.Is_Set then
               Put_Line (";");
               Put ("Value : LSP.Structures.");
               Put (LSP_Gen.Requests.Param_Type (Model, J));
            end if;

            Put_Line (");");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Response");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
            Put ("Value : LSP.Structures.");
            Put (LSP_Gen.Responses.Result_Type (Model, Done, J));
            Put_Line (");");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Partial_Result");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Token : LSP.Structures.ProgressToken;");
            Put ("Value : LSP.Structures.");
            Put (LSP_Gen.Progress_Reports.Result_Type (Model, Done, J));
            Put_Line (");");
            New_Line;
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("overriding procedure On_");
         Put (Work_Done);
         Put_Line ("_Work_Done");
         Put ("(Self : in out ");
         Put (Name);
         Put_Line (";");
         Put_Line ("Token : LSP.Structures.ProgressToken;");
         Put ("Value : LSP.Structures.WorkDone");
         Put (Work_Done);
         Put_Line (");");
         New_Line;
      end loop;

      Put_Line ("end;");
      New_Line;

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Client_Responses.Errors;");

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Server then
            Put ("with LSP.Client_Notifications.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Server then
            Put ("with LSP.Client_Requests.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client then
            Put ("with LSP.Client_Responses.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("with LSP.Progress_Reports.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("with LSP.Progress_Reports.");
         Put (Work_Done);
         Put_Line (";");
         New_Line;
      end loop;

      New_Line;
      Put_Line ("package body LSP.Client_Message_Factories is");
      New_Line;
      Put_Line ("overriding procedure On_Error_Response");
      Put_Line ("(Self  : in out Client_Message_Factory;");
      Put_Line ("Id    : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError) is");
      Put_Line ("begin");
      Put_Line ("Client_Message_Factory'Class (Self).On_Message");
      Put_Line ("(new LSP.Client_Responses.Errors.Response'");
      Put_Line ("(Id => Id, Error => Value));");
      Put_Line ("end;");
      New_Line;

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Server then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Notification");
            Put ("(Self : in out ");
            Put (Name);

            if Model.Notification (J).params.Is_Set then
               Put_Line (";");
               Put ("Value : LSP.Structures.");
               Put (Model.Notification (J).params.Value.Union.reference.name);
            end if;

            Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Client_Message_Factory'Class (Self).On_Message");
            Put ("(new LSP.Client_Notifications.");
            Put (Model.Message_Name (J));
            Put_Line (".Notification'");
            Put_Line ("(Params => Value));");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Server then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Request");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

            if Model.Request (J).params.Is_Set then
               Put_Line (";");
               Put ("Value : LSP.Structures.");
               Put (LSP_Gen.Requests.Param_Type (Model, J));
            end if;

            Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Client_Message_Factory'Class (Self).On_Message");
            Put ("(new LSP.Client_Requests.");
            Put (Model.Message_Name (J));
            Put_Line (".Request'");
            Put ("(Id => Id");

            if Model.Request (J).params.Is_Set then
               Put (", Params => Value");
            end if;

            Put_Line ("));");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Response");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
            Put ("Value : LSP.Structures.");
            Put (LSP_Gen.Responses.Result_Type (Model, Done, J));
            Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Client_Message_Factory'Class (Self).On_Message");
            Put ("(new LSP.Client_Responses.");
            Put (Model.Message_Name (J));
            Put_Line (".Response'");
            Put_Line ("(Id => Id, Result => Value));");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Partial_Result");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Token : LSP.Structures.ProgressToken;");
            Put ("Value : LSP.Structures.");
            Put (LSP_Gen.Progress_Reports.Result_Type (Model, Done, J));
            Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Client_Message_Factory'Class (Self).On_Message");
            Put ("(new LSP.Progress_Reports.");
            Put (Model.Message_Name (J));
            Put_Line (".Partial_Result'");
            Put_Line ("(Token => Token, Params => Value));");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("overriding procedure On_");
         Put (Work_Done);
         Put_Line ("_Work_Done");
         Put ("(Self : in out ");
         Put (Name);
         Put_Line (";");
         Put_Line ("Token : LSP.Structures.ProgressToken;");
         Put ("Value : LSP.Structures.WorkDone");
         Put (Work_Done);
         Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Client_Message_Factory'Class (Self).On_Message");
            Put ("(new LSP.Progress_Reports.");
            Put (Work_Done);
            Put_Line (".Work_Done'");
            Put_Line ("(Token => Token, Params => Value));");
            Put_Line ("end;");
            New_Line;
      end loop;

      Put_Line ("end;");
      New_Line;
   end Write_Client_Factories;

   ----------------------------
   -- Write_Server_Factories --
   ----------------------------

   procedure Write_Server_Factories
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;
      use type VSS.Strings.Virtual_String;

      Name : constant VSS.Strings.Virtual_String := "Server_Message_Factory";

   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Server_Message_Consumers;");
      Put_Line ("with LSP.Server_Message_Receivers;");
      Put_Line ("with LSP.Errors;");
      Put_Line ("with LSP.Structures;");

      New_Line;

      Put_Line ("package LSP.Server_Message_Factories");
      Put_Line ("  with Preelaborate");
      Put_Line ("is");
      New_Line;
      Put_Line ("type " & Name & " (Consumer : not null access");
      Put_Line ("LSP.Server_Message_Consumers.Server_Message_Consumer'Class)");
      Put_Line (" is limited new");
      Put_Line ("LSP.Server_Message_Receivers.Server_Message_Receiver ");
      Put_Line ("with null record;");
      New_Line;

      Put_Line ("overriding procedure On_Error_Response");
      Put_Line ("(Self : in out Server_Message_Factory;");
      Put_Line ("Id    : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError);");
      New_Line;

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Client then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Notification");
            Put ("(Self : in out ");
            Put (Name);

            if Model.Notification (J).params.Is_Set then
               Put_Line (";");
               Put ("Params : LSP.Structures.");
               Put (Model.Notification (J).params.Value.Union.reference.name);
            end if;

            Put_Line (");");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         case  Model.Message_Direction (J) is
            when From_Client =>
               Put ("overriding procedure On_");
               Put (Model.Message_Name (J));
               Put_Line ("_Request");
               Put ("(Self : in out ");
               Put (Name);
               Put_Line (";");
               Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

               if Model.Request (J).params.Is_Set then
                  Put_Line (";");
                  Put ("Value : LSP.Structures.");
                  Put (LSP_Gen.Requests.Param_Type (Model, J));
               end if;

               Put_Line (");");
               New_Line;

            when From_Server =>
               Put ("overriding procedure On_");
               Put (Model.Message_Name (J));
               Put_Line ("_Response");
               Put ("(Self : in out ");
               Put (Name);
               Put_Line (";");
               Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
               Put ("Value : LSP.Structures.");
               Put (LSP_Gen.Responses.Result_Type (Model, Done, J));
               Put_Line (");");
               New_Line;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Put_Line ("end LSP.Server_Message_Factories;");
      New_Line;

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Server_Responses.Errors;");

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Client then
            Put ("with LSP.Server_Notifications.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Client then
            Put ("with LSP.Server_Requests.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From_Server then
            Put ("with LSP.Server_Responses.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      New_Line;
      Put_Line ("package body LSP.Server_Message_Factories is");
      New_Line;
      Put_Line ("overriding procedure On_Error_Response");
      Put_Line ("(Self : in out Server_Message_Factory;");
      Put_Line ("Id    : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError) is");
      Put_Line ("begin");
      Put_Line ("Self.Consumer.On_Message");
      Put_Line ("(new LSP.Server_Responses.Errors.Response'");
      Put_Line ("(Id => Id, Error => Value));");
      Put_Line ("end;");
      New_Line;

      for J of Model.Notifications loop
         if Model.Message_Direction (J) = From_Client then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Notification");
            Put ("(Self : in out ");
            Put (Name);

            if Model.Notification (J).params.Is_Set then
               Put_Line (";");
               Put ("Params : LSP.Structures.");
               Put (Model.Notification (J).params.Value.Union.reference.name);
            end if;

            Put_Line (") is");
            Put_Line ("begin");
            Put_Line ("Self.Consumer.On_Message");
            Put ("(new LSP.Server_Notifications.");
            Put (Model.Message_Name (J));
            Put_Line (".Notification");
            if Model.Notification (J).params.Is_Set then
               Put_Line ("'(Params => Params)");
            end if;
            Put_Line (");");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      for J of Model.Requests loop
         case Model.Message_Direction (J) is
            when From_Client =>
               Put ("overriding procedure On_");
               Put (Model.Message_Name (J));
               Put_Line ("_Request");
               Put ("(Self : in out ");
               Put (Name);
               Put_Line (";");
               Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

               if Model.Request (J).params.Is_Set then
                  Put_Line (";");
                  Put ("Value : LSP.Structures.");
                  Put (LSP_Gen.Requests.Param_Type (Model, J));
               end if;

               Put_Line (") is");
               Put_Line ("begin");
               Put_Line ("Self.Consumer.On_Message");
               Put ("(new LSP.Server_Requests.");
               Put (Model.Message_Name (J));
               Put_Line (".Request'");
               Put ("(Id => Id");

               if Model.Request (J).params.Is_Set then
                  Put (", Params => Value");
               end if;

               Put (", Canceled => False");
               Put_Line ("));");
               Put_Line ("end;");
               New_Line;

            when From_Server =>
               Put ("overriding procedure On_");
               Put (Model.Message_Name (J));
               Put_Line ("_Response");
               Put ("(Self : in out ");
               Put (Name);
               Put_Line (";");
               Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
               Put ("Value : LSP.Structures.");
               Put (LSP_Gen.Responses.Result_Type (Model, Done, J));
               Put_Line (") is");
               Put_Line ("begin");
               Put_Line ("Self.Consumer.On_Message");
               Put ("(new LSP.Server_Responses.");
               Put (Model.Message_Name (J));
               Put_Line (".Response'");
               Put_Line ("(Id => Id, Result => Value));");
               Put_Line ("end;");
               New_Line;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Put_Line ("end LSP.Server_Message_Factories;");
      New_Line;
   end Write_Server_Factories;

end LSP_Gen.Factories;
