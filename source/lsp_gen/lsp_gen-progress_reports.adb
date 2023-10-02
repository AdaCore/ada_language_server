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

with VSS.String_Vectors;

with LSP_Gen.Configurations;
with LSP_Gen.Entities;
with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Progress_Reports is

   use type VSS.Strings.Virtual_String;

   procedure Write_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      From  : LSP_Gen.Configurations.Message_Direction;
      Done  : LSP_Gen.Dependencies.Dependency_Map);

   procedure Write_Receivers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   procedure Write_Writers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   procedure Write_Readers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction);

   Work_Done_List : VSS.String_Vectors.Virtual_String_Vector renames
     LSP_Gen.Dependencies.Work_Done_List;

   -----------------
   -- Result_Type --
   -----------------

   function Result_Type
     (Model  : LSP_Gen.Meta_Models.Meta_Model;
      Done   : LSP_Gen.Dependencies.Dependency_Map;
      Method : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      Result : constant LSP_Gen.Entities.AType :=
        Model.Request (Method).partialResult.Value;
      Found  : constant LSP_Gen.Dependencies.Dependency_Maps.Cursor :=
        Done.Find (Result);
   begin
      return LSP_Gen.Dependencies.Dependency_Maps.Element (Found).Short_Name;
   end Result_Type;

   -----------
   -- Write --
   -----------

   procedure Write
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;
   begin
      Write_Types (Model, From_Client, Done);
      Write_Receivers (Model, Done, From_Client);
      Write_Writers (Model, Done, From_Client);
      Write_Readers (Model, Done, From_Client);
   end Write;

   -------------------
   -- Write_Readers --
   -------------------

   procedure Write_Readers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

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
         if Model.Message_Direction (J) = From
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
      end loop;

      New_Line;

      Put ("package body LSP.Progress_Report_Readers is");
      New_Line;

      Put_Line ("package Method_Map is new Minimal_Perfect_Hash ([");
      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
           and then Model.Request (J).partialResult.Is_Set
         then
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

      for Work_Done of Work_Done_List loop
         Put (", ");
         Put ("""WorkDone");
         Put (Work_Done);
         Put ("""");
      end loop;

      Put_Line ("]);");
      New_Line;
      Put_Line ("procedure Initialize is begin Method_Map.Initialize; end;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("procedure Read_");
            Put (Model.Message_Name (J));

            Put_Line (" is new LSP.Input_Tools.Read_Progress_Report");
            Put ("(LSP.Structures.");
            Put (Result_Type (Model, Done, J));
            Put (", LSP.Inputs.Read_");
            Put (Result_Type (Model, Done, J));
            Put_Line (");");

            New_Line;
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("procedure Read_");
         Put (Work_Done);

         Put_Line (" is new LSP.Input_Tools.Read_Progress_Report");
         Put ("(LSP.Structures.WorkDone");
         Put (Work_Done);
         Put (", LSP.Inputs.Read_WorkDone");
         Put (Work_Done);
         Put_Line (");");

         New_Line;
      end loop;

      Put_Line ("function Read_Progress_Report (Input : ");
      Put_Line ("in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;");
      Put_Line ("Method : VSS.Strings.Virtual_String)");
      Put_Line ("return LSP.Progress_Reports.Progress_Report'Class is");
      Put_Line ("Index : constant Natural := Method_Map.Get_Index (Method);");
      Put_Line ("begin");
      Put_Line ("case Index is");

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("when ");
            Put (Index);
            Index := Index + 1;
            Put (" =>  --  ");
            Put_Line (J);

            Put ("return Result : LSP.Progress_Reports.");
            Put (Model.Message_Name (J));
            Put_Line (".Partial_Result do");
            Put ("Read_");
            Put (Model.Message_Name (J));
            Put_Line (" (Input");

            Put_Line (", Result.Token, Result.Params);");
            Put_Line ("end return;");
            New_Line;
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("when ");
         Put (Index);
         Index := Index + 1;
         Put (" =>  --  WorkDone");
         Put_Line (Work_Done);

         Put ("return Result : LSP.Progress_Reports.");
         Put (Work_Done);
         Put_Line (".Work_Done do");
         Put ("Read_");
         Put (Work_Done);
         Put_Line (" (Input");

         Put_Line (", Result.Token, Result.Params);");
         Put_Line ("end return;");
         New_Line;
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

      Name : constant VSS.Strings.Virtual_String :=
        "Progress_Report_Receiver";
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
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
         if Model.Message_Direction (J) = From
           and then Model.Request (J).partialResult.Is_Set
         then
            Put ("procedure On_");
            Put (Model.Message_Name (J));
            Put_Line ("_Partial_Result");
            Put ("(Self : in out ");
            Put (Name);
            Put_Line (";");
            Put_Line ("Token : LSP.Structures.ProgressToken;");
            Put ("Value : LSP.Structures.");
            Put (Result_Type (Model, Done, J));

            Put_Line (") is null;");
            Put_Lines
              (Model.Request (J).documentation.Split_Lines, "   --  ");
            New_Line;
         end if;
      end loop;

      for Work_Done of Work_Done_List loop
         Put ("procedure On_");
         Put (Work_Done);
         Put_Line ("_Work_Done");
         Put ("(Self : in out ");
         Put (Name);
         Put_Line (";");
         Put_Line ("Token : LSP.Structures.ProgressToken;");
         Put ("Value : LSP.Structures.WorkDone");
         Put (Work_Done);
         Put_Line (") is null;");
         New_Line;
      end loop;

      Put_Line ("end;");
   end Write_Receivers;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      From  : LSP_Gen.Configurations.Message_Direction;
      Done  : LSP_Gen.Dependencies.Dependency_Map)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      procedure Write_Type
        (Name   : VSS.Strings.Virtual_String;
         Param  : VSS.Strings.Virtual_String;
         Kind  : VSS.Strings.Virtual_String);
      --  Kind is WorkDone or Partial_Result

      procedure Write_Type
        (Name  : VSS.Strings.Virtual_String;
         Param : VSS.Strings.Virtual_String;
         Kind  : VSS.Strings.Virtual_String) is
      begin
         Put_Lines (Model.License_Header, "--  ");
         New_Line;
         Put_Line ("with LSP.Structures;");
         New_Line;
         Put ("package LSP.Progress_Reports.");
         Put (Name);
         Put_Line (" is");
         Put_Line ("pragma Preelaborate;");
         New_Line;

         Put ("type ");
         Put (Kind);
         Put (" is new LSP.Progress_Report");
         Put_Line ("s.Progress_Report with record");

         Put ("Params : LSP.Structures.");
         Put (Param);
         Put_Line (";");

         Put_Line ("end record;");
         New_Line;

         Put ("overriding procedure Visit_");
         Put ("Receiver");
         Put ("(Self  : ");
         Put (Kind);
         Put_Line (";");
         Put ("Value : in out LSP.Progress_Report_Receivers.");
         Put_Line ("Progress_Report_Receiver'Class);");
         New_Line;

         Put_Line ("end;");
         New_Line;

         --  Package body
         Put_Lines (Model.License_Header, "--  ");
         New_Line;
         Put ("package body LSP.Progress_Reports.");
         Put (Name);
         Put_Line (" is");
         New_Line;

         Put_Line ("overriding procedure Visit_Receiver");
         Put ("(Self  : ");
         Put (Kind);
         Put_Line (";");
         Put ("Value : in out LSP.Progress_Report_Receivers.");
         Put_Line ("Progress_Report_Receiver'Class) is");
         Put_Line ("begin");
         Put ("Value.On_");
         Put (Name);
         Put ("_");
         Put (Kind);
         Put (" (Self.Token");

         Put (", Self.Params");

         Put_Line (");");
         Put_Line ("end;");
         New_Line;

         Put_Line ("end;");
         New_Line;
      end Write_Type;

   begin
      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
           and then Model.Request (J).partialResult.Is_Set
         then
            Write_Type
              (Model.Message_Name (J),
               Result_Type (Model, Done, J),
               "Partial_Result");
         end if;
      end loop;

      for Name of Work_Done_List loop
         Write_Type (Name, "WorkDone" & Name, "Work_Done");
      end loop;
   end Write_Types;

   -------------------
   -- Write_Writers --
   -------------------

   procedure Write_Writers
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      From  : LSP_Gen.Configurations.Message_Direction)
   is
      use all type LSP_Gen.Configurations.Message_Direction;

      Name : constant VSS.Strings.Virtual_String :=
        "Progress_Report_Writer";
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Structures;");
      Put_Line ("with VSS.JSON.Content_Handlers;");
      Put_Line ("with LSP.Progress_Report_Receivers;");
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
      Put_Line ("LSP.Progress_Report_Receivers.Progress_Report_Receiver");
      Put_Line ("with null record;");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
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
            Put (Result_Type (Model, Done, J));
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

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with LSP.Output_Tools;");
      Put_Line ("with LSP.Outputs;");
      New_Line;

      Put ("package body LSP.");
      Put (Name);
      Put_Line ("s is");
      New_Line;

      for J of Model.Requests loop
         if Model.Message_Direction (J) = From
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
            Put (Result_Type (Model, Done, J));
            Put_Line (") is");
            Put_Line ("begin");
            Put
              ("LSP.Output_Tools.Write_Start_Progress_Report"
               & " (Self.Output.all, Token);");

            Put ("LSP.Outputs.Write_");
            Put (Result_Type (Model, Done, J));
            Put_Line (" (Self.Output.all, Value);");

            Put_Line ("Self.Output.End_Object;");
            Put_Line ("Self.Output.End_Object;");
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
         Put
           ("LSP.Output_Tools.Write_Start_Progress_Report"
            & " (Self.Output.all, Token);");

         Put ("LSP.Outputs.Write_WorkDone");
         Put (Work_Done);
         Put_Line (" (Self.Output.all, Value);");

         Put_Line ("Self.Output.End_Object;");
         Put_Line ("Self.Output.End_Object;");
         Put_Line ("end;");
         New_Line;
      end loop;

      Put_Line ("end;");

   end Write_Writers;

end LSP_Gen.Progress_Reports;
