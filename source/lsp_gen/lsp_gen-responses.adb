------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with LSP_Gen.Configurations;
with LSP_Gen.Entities;
with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Responses is

   use type VSS.Strings.Virtual_String;

   procedure Write_Receivers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   procedure Write_Readers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   procedure Write_Writers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   procedure Write_Response_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      From  : LSP_Gen.Configurations.Message_Direction;
      Done  : LSP_Gen.Dependencies.Dependency_Map);

   function Prefix
     (From : LSP_Gen.Configurations.Message_Direction)
      return VSS.Strings.Virtual_String is
        (VSS.Strings.To_Virtual_String
          (case From is
              when LSP_Gen.Configurations.From_Both => "Base",
              when LSP_Gen.Configurations.From_Client => "Client",
              when LSP_Gen.Configurations.From_Server => "Server"));

   -----------------
   -- Result_Type --
   -----------------

   function Result_Type
     (Model  : LSP_Gen.Meta_Models.Meta_Model;
      Done   : LSP_Gen.Dependencies.Dependency_Map;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      Result : constant LSP_Gen.Entities.AType :=
        Model.Request (Method).result;
      Found  : constant LSP_Gen.Dependencies.Dependency_Maps.Cursor :=
        Done.Find (Result);
   begin
      return LSP_Gen.Dependencies.Dependency_Maps.Element (Found).Short_Name;
   end Result_Type;

   --------------------------
   -- Write_Response_Types --
   --------------------------

   procedure Write_Response_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      From  : LSP_Gen.Configurations.Message_Direction;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;
      Kind : constant VSS.Strings.Virtual_String := Prefix (From);
      Name : constant VSS.Strings.Virtual_String := Kind & "_Response";
   begin
      for J of Model.Requests loop
         if Model.Message_Direction (J) = From then
            Put_Lines (Model.License_Header, "--  ");
            New_Line;
            Put_Line ("with LSP.Structures;");
            New_Line;
            Put ("package LSP.");
            Put (Name);
            Put ("s.");
            Put (Model.Message_Name (J));
            Put_Line (" is");
            Put_Line ("pragma Preelaborate;");
            New_Line;

            Put ("type Response is new LSP.");
            Put (Name);
            Put ("s.");
            Put (Name);
            Put_Line (" with record");

            Put ("Result : LSP.Structures.");
            Put (Result_Type (Model, Done, J));
            Put_Line (";");

            Put_Line ("end record;");
            New_Line;

            Put ("overriding procedure Visit_");
            Put (Kind);
            Put_Line ("_Receiver");
            Put_Line ("(Self  : Response;");
            Put ("Value : in out LSP.");
            Put (Name);
            Put ("_Receivers.");
            Put (Name);
            Put_Line ("_Receiver'Class);");
            New_Line;

            Put_Line ("end;");
            New_Line;

            --  Package body
            Put_Lines (Model.License_Header, "--  ");
            New_Line;
            Put ("package body LSP.");
            Put (Name);
            Put ("s.");
            Put (Model.Message_Name (J));
            Put_Line (" is");
            New_Line;

            Put ("overriding procedure Visit_");
            Put (Kind);
            Put_Line ("_Receiver");
            Put_Line ("(Self  : Response;");
            Put ("Value : in out LSP.");
            Put (Name);
            Put ("_Receivers.");
            Put (Name);
            Put_Line ("_Receiver'Class) is");
            Put_Line ("begin");
            Put ("Value.On_");
            Put (Model.Message_Name (J));
            Put ("_Response (Self.Id");

            Put (", Self.Result");

            Put_Line (");");
            Put_Line ("end;");
            New_Line;

            Put_Line ("end;");
            New_Line;

         end if;
      end loop;
   end Write_Response_Types;

   -------------------
   -- Write_Readers --
   -------------------

   procedure Write_Readers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      Kind : constant VSS.Strings.Virtual_String := Prefix (From);
      First : Boolean := True;
      Index : Positive := 1;
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with Minimal_Perfect_Hash;");
      Put_Line ("with LSP.Inputs;");
      Put_Line ("with LSP.Input_Tools;");
      Put_Line ("with LSP.Structures;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) in From | From_Both then
            Put ("with LSP.");
            Put (Prefix (Model.Message_Direction (J)));
            Put ("_Responses.");
            Put (Model.Message_Name (J));
            Put_Line (";");
         end if;
      end loop;

      New_Line;

      Put ("package body LSP.");
      Put (Kind);
      Put_Line ("_Response_Readers is");
      New_Line;

      Put_Line ("package Method_Map is new Minimal_Perfect_Hash ([");
      for J of Model.Requests loop
         if Model.Message_Direction (J) in From | From_Both then
            if First then
               First := False;
            else
               Put (", ");
            end if;

            Put ("""");
            Put (J);
            Put ("""");
         end if;
      end loop;

      Put_Line ("]);");
      New_Line;
      Put_Line ("procedure Initialize is begin Method_Map.Initialize; end;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) in From | From_Both then
            Put ("procedure Read_");
            Put (Model.Message_Name (J));

            Put_Line (" is new LSP.Input_Tools.Read_Response");
            Put ("(LSP.Structures.");
            Put (Result_Type (Model, Done, J));
            Put (", LSP.Inputs.Read_");
            Put (Result_Type (Model, Done, J));
            Put_Line (");");

            New_Line;
         end if;
      end loop;

      Put_Line ("function Read_Response (Input : ");
      Put_Line ("in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;");
      Put_Line ("Method : VSS.Strings.Virtual_String)");
      Put ("return LSP.");
      Put (Kind);
      Put ("_Responses.");
      Put (Kind);
      Put_Line ("_Response'Class is");
      Put_Line ("Index : constant Natural := Method_Map.Get_Index (Method);");
      Put_Line ("begin");
      Put_Line ("case Index is");

      for J of Model.Requests loop
         if Model.Message_Direction (J) in From | From_Both then
            Put ("when ");
            Put (Index);
            Index := Index + 1;
            Put (" =>  --  ");
            Put_Line (J);

            Put ("return Result : LSP.");
            Put (Prefix (Model.Message_Direction (J)));
            Put ("_Responses.");
            Put (Model.Message_Name (J));
            Put_Line (".Response do");
            Put ("Read_");
            Put (Model.Message_Name (J));
            Put_Line (" (Input, Result.Id, Result.Result);");
            Put_Line ("end return;");
            New_Line;
         end if;
      end loop;

      Put_Line ("when others =>");
      Put_Line ("return raise Program_Error with ""Unknown method"";");
      Put_Line ("end case;");
      Put_Line ("end;");

      Put_Line ("end;");

   end Write_Readers;

   ---------------------
   -- Write_Receivers --
   ---------------------

   procedure Write_Receivers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      Kind : constant VSS.Strings.Virtual_String := Prefix (From);
      Name : constant VSS.Strings.Virtual_String :=
        Kind & "_Response_Receiver";
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Errors;");
      Put_Line ("with LSP.Structures;");
      New_Line;

      Put ("package LSP.");
      Put (Name);
      Put_Line ("s is");
      Put_Line ("pragma Preelaborate;");
      New_Line;
      Put ("type ");
      Put (Name);
      Put_Line (" is limited interface;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From then
            Put ("procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Response");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

            Put_Line (";");
            Put ("Value : LSP.Structures.");
            Put (Result_Type (Model, Done, J));

            Put_Line (") is null;");
            Put_Lines
              (Model.Request (J).documentation.Split_Lines, "   --  ");
            New_Line;
         end if;
      end loop;

      Put_Line ("procedure On_Error_Response");
      Put ("(Self : in out ");
      Put (Name);
      Put_Line (";");
      Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

      Put_Line (";");
      Put_Line ("Value : LSP.Errors.ResponseError) is null;");
      New_Line;

      Put_Line ("end;");
   end Write_Receivers;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;
   begin
      Write_Readers (Model, Done, From_Server);
      Write_Readers (Model, Done, From_Client);
      Write_Writers (Model, Done, From_Server);
      Write_Writers (Model, Done, From_Client);
      Write_Receivers (Model, Done, From_Client);
      Write_Receivers (Model, Done, From_Server);
      Write_Response_Types (Model, From_Client, Done);
      Write_Response_Types (Model, From_Server, Done);
   end Write;

   -------------------
   -- Write_Writers --
   -------------------

   procedure Write_Writers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      Kind : constant VSS.Strings.Virtual_String := Prefix (From);
      Name : constant VSS.Strings.Virtual_String :=
        Kind & "_Response_Writer";
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with VSS.JSON.Content_Handlers;");
      New_Line;
      Put_Line ("with LSP.Errors;");
      Put_Line ("with LSP.Structures;");
      Put ("with LSP.");
      Put (Kind);
      Put_Line ("_Response_Receivers;");
      New_Line;
      Put ("package LSP.");
      Put (Name);
      Put_Line ("s is");
      Put_Line ("pragma Preelaborate;");
      New_Line;
      Put ("type ");
      Put_Line (Name);

      Put ("(Output : access VSS.JSON.Content_Handlers");
      Put_Line (".JSON_Content_Handler'Class)");
      Put ("is new ");

      Put ("LSP.");
      Put (Kind);
      Put ("_Response_Receivers.");
      Put (Kind);
      Put_Line ("_Response_Receiver");
      Put_Line ("with null record;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Response");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
            Put ("Value : LSP.Structures.");
            Put (Result_Type (Model, Done, J));

            Put_Line (");");
            New_Line;
         end if;
      end loop;

      Put_Line ("overriding procedure On_Error_Response");
      Put ("(Self : in out ");
      Put (Name);
      Put_Line (";");
      Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError);");
      New_Line;

      Put_Line ("end;");

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with Interfaces;");
      New_Line;
      Put_Line ("with LSP.Output_Tools;");
      Put_Line ("with LSP.Outputs;");
      New_Line;

      Put ("package body LSP.");
      Put (Name);
      Put_Line ("s is");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From then
            Put ("overriding procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Response");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String");

            Put_Line (";");
            Put ("Value : LSP.Structures.");
            Put (Result_Type (Model, Done, J));

            Put_Line (") is");
            Put_Line ("begin");
            Put_Line
              ("LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);");

            declare
               Tipe : constant VSS.Strings.Virtual_String :=
                 Result_Type (Model, Done, J);
            begin
               Put_Line ("Self.Output.Key_Name (""result"");");
               Put ("LSP.Outputs.Write_");
               Put (Tipe);
               Put_Line (" (Self.Output.all, Value);");
            end;

            Put_Line ("Self.Output.End_Object;");
            Put_Line ("end;");
            New_Line;
         end if;
      end loop;

      Put_Line ("overriding procedure On_Error_Response");
      Put ("(Self : in out ");
      Put (Name);
      Put_Line (";");
      Put_Line ("Id : LSP.Structures.Integer_Or_Virtual_String;");
      Put_Line ("Value : LSP.Errors.ResponseError) is");
      Put_Line ("begin");
      Put_Line
        ("LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);");
      Put_Line ("Self.Output.Key_Name (""error"");");
      Put_Line ("Self.Output.Start_Object;");
      Put_Line ("Self.Output.Key_Name (""code"");");
      Put ("Self.Output.Integer_Value (Interfaces.Integer_64'Val");
      Put_Line (" (Value.code));");
      Put_Line ("Self.Output.Key_Name (""message"");");
      Put_Line ("Self.Output.String_Value (Value.message);");
      Put_Line ("Self.Output.End_Object;");
      Put_Line ("Self.Output.End_Object;");
      Put_Line ("end;");
      New_Line;

      Put_Line ("end;");

   end Write_Writers;

end LSP_Gen.Responses;
