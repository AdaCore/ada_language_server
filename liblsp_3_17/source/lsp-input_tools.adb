--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Streams;

with LSP.Inputs;
with Minimal_Perfect_Hash;

package body LSP.Input_Tools is

   package Notification_Propery_Names is new Minimal_Perfect_Hash
     (["jsonrpc", "method", "params", "id", "result"]);

   ---------------------------------------------------
   -- Look_For_MarkupContent_Or_MarkedString_Vector --
   ---------------------------------------------------

   procedure Look_For_MarkupContent_Or_MarkedString_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector) is
   begin
      raise Program_Error with "Unimplemented";
   end Look_For_MarkupContent_Or_MarkedString_Vector;

   -----------------
   -- Read_LSPAny --
   -----------------

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny) is
   begin
      Read_LSPAny_Class (Handler, Value);
   end Read_LSPAny;

   -----------------------
   -- Read_LSPAny_Class --
   -----------------------

   procedure Read_LSPAny_Class
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.JSON_Event_Vectors.Vector'Class)
   is
      use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;
      Level : Natural := 0;
   begin
      while Handler.Element_Kind
        in VSS.JSON.Streams.Valid_JSON_Stream_Element_Kind
      loop
         Value.Append (Handler.Element);

         Level := Level +
           (case Handler.Element_Kind is
               when Start_Array | Start_Object => 1,
               when End_Array | End_Object => -1,
               when others => 0);

         Handler.Read_Next;

         exit when Level = 0;
      end loop;
   end Read_LSPAny_Class;

   -----------------------
   -- Read_Notification --
   -----------------------

   procedure Read_Notification
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Parameter_Type)
   is
      use type VSS.Strings.Virtual_String;

      package PN renames Notification_Propery_Names;
   begin

      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;

            case PN.Get_Index (Key) is
               when 1 =>  --  jsonrpc
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "2.0");
                  Handler.Read_Next;
               when 2 =>  --  method
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = Method);
                  Handler.Read_Next;
               when 3 =>  --  params
                  Read_Parameter (Handler, Value);
               when others =>
                  --  ignore anything else?
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Notification;

   -----------------------
   -- Read_Null_Request --
   -----------------------

   procedure Read_Null_Request
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method  : VSS.Strings.Virtual_String;
      Id      : out LSP.Structures.Integer_Or_Virtual_String)
   is
      use type VSS.Strings.Virtual_String;

      package PN renames Notification_Propery_Names;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;

            case PN.Get_Index (Key) is
               when 1 =>  --  jsonrpc
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "2.0");
                  Handler.Read_Next;
               when 2 =>  --  method
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = Method);
                  Handler.Read_Next;
               when 4 =>  --  id
                  LSP.Inputs.Read_Integer_Or_Virtual_String (Handler, Id);
               when others =>
                  --  ignore anything else?
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Null_Request;

   --------------------------
   -- Read_Progress_Report --
   --------------------------

   procedure Read_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Token   : out LSP.Structures.ProgressToken;
      Value   : out Result_Type)
   is
      use type VSS.Strings.Virtual_String;

      package PN renames Notification_Propery_Names;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;

            case PN.Get_Index (Key) is
               when 1 =>  --  jsonrpc
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "2.0");
                  Handler.Read_Next;
               when 2 =>  --  method
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "$/progress");
                  Handler.Read_Next;
               when 3 =>  --  params
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while Handler.Is_Key_Name loop
                     declare
                        Field : constant VSS.Strings.Virtual_String :=
                          Handler.Key_Name;
                     begin
                        Handler.Read_Next;

                        if Field = "token" then
                           LSP.Inputs.Read_ProgressToken
                             (Handler, Token);
                        elsif Field = "value" then
                           Read_Progress_Report (Handler, Value);
                        else
                           --  ignore anything else?
                           Handler.Skip_Current_Value;
                        end if;
                     end;
                  end loop;

                  pragma Assert (Handler.Is_End_Object);
                  Handler.Read_Next;
               when others =>
                  --  ignore anything else?
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Progress_Report;

   ------------------
   -- Read_Request --
   ------------------

   procedure Read_Request
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : out LSP.Structures.Integer_Or_Virtual_String;
      Value   : out Parameter_Type)
   is
      use type VSS.Strings.Virtual_String;

      package PN renames Notification_Propery_Names;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;

            case PN.Get_Index (Key) is
               when 1 =>  --  jsonrpc
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "2.0");
                  Handler.Read_Next;
               when 2 =>  --  method
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = Method);
                  Handler.Read_Next;
               when 3 =>  --  params
                  Read_Parameter (Handler, Value);
               when 4 =>  --  id
                  LSP.Inputs.Read_Integer_Or_Virtual_String (Handler, Id);
               when others =>
                  --  ignore anything else?
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Request;

   -------------------
   -- Read_Response --
   -------------------

   procedure Read_Response
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : out LSP.Structures.Integer_Or_Virtual_String;
      Value   : out Result_Type)
   is
      use type VSS.Strings.Virtual_String;

      package PN renames Notification_Propery_Names;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;

            case PN.Get_Index (Key) is
               when 1 =>  --  jsonrpc
                  pragma Assert (Handler.Is_String_Value and then
                                 Handler.String_Value = "2.0");
                  Handler.Read_Next;
               when 4 =>  --  id
                  LSP.Inputs.Read_Integer_Or_Virtual_String (Handler, Id);
               when 5 =>  --  result
                  Read_Result (Handler, Value);
               when others =>
                  --  ignore anything else?
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Response;

end LSP.Input_Tools;
