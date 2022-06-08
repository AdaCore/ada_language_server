--  Automatically generated, do not edit.

with Interfaces;

with VSS.Strings.Conversions;

with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;                    use LSP.Types;

package body LSP.Message_IO is

   pragma Style_Checks ("M175");

   use type Interfaces.Integer_64;
   use type VSS.Strings.Virtual_String;

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "jsonrpc" then
               LSP.Types.Read_String (S, V.jsonrpc);
            elsif Key = "id" then
               LSP_Number_Or_String'Read (S, V.id);
            elsif Key = "method" then
               LSP.Types.Read_String (S, V.method);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RequestMessage;

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Write_String (S, V.jsonrpc);
      JS.Key ("id");
      LSP_Number_Or_String'Write (S, V.id);
      JS.Key ("method");
      LSP.Types.Write_String (S, V.method);
      JS.End_Object;
   end Write_RequestMessage;

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "jsonrpc" then
               LSP.Types.Read_String (S, V.jsonrpc);
            elsif Key = "method" then
               LSP.Types.Read_String (S, V.method);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NotificationMessage;

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("jsonrpc");
      LSP.Types.Write_String (S, V.jsonrpc);
      JS.Key ("method");
      LSP.Types.Write_String (S, V.method);
      JS.End_Object;
   end Write_NotificationMessage;

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CancelParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "id" then
               LSP_Number_Or_String'Read (S, V.id);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CancelParams;

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CancelParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("id");
      LSP_Number_Or_String'Write (S, V.id);
      JS.End_Object;
   end Write_CancelParams;

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "line" then
               Line_Number'Read (S, V.line);
            elsif Key = "character" then
               LSP.Types.Read_UTF16_Code_Unit_Count (JS, V.character);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Position;

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Position)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("line");
      Line_Number'Write (S, V.line);
      JS.Key ("character");
      LSP.Types.Write_UTF16_Code_Unit_Count (JS, V.character);
      JS.End_Object;
   end Write_Position;

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "start" then
               Position'Read (S, V.first);
            elsif Key = "end" then
               Position'Read (S, V.last);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Span;

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Write (S, V.first);
      JS.Key ("end");
      Position'Write (S, V.last);
      JS.End_Object;
   end Write_Span;

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "" then
         V := Empty;
      elsif Text = "quickfix" then
         V := QuickFix;
      elsif Text = "refactor" then
         V := Refactor;
      elsif Text = "refactor.extract" then
         V := RefactorExtract;
      elsif Text = "refactor.inline" then
         V := RefactorInline;
      elsif Text = "refactor.rewrite" then
         V := RefactorRewrite;
      elsif Text = "source" then
         V := Source;
      elsif Text = "source.organizeImports" then
         V := SourceOrganizeImports;
      else
         V := CodeActionKind'First;
      end if;
   end Read_CodeActionKind;

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : CodeActionKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : CodeActionKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when Empty =>
               return "";
            when QuickFix =>
               return "quickfix";
            when Refactor =>
               return "refactor";
            when RefactorExtract =>
               return "refactor.extract";
            when RefactorInline =>
               return "refactor.inline";
            when RefactorRewrite =>
               return "refactor.rewrite";
            when Source =>
               return "source";
            when SourceOrganizeImports =>
               return "source.organizeImports";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_CodeActionKind;

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "reference" then
         V := Simple;
      elsif Text = "access" then
         V := Access_Ref;
      elsif Text = "write" then
         V := Write;
      elsif Text = "call" then
         V := Static_Call;
      elsif Text = "dispatching call" then
         V := Dispatching_Call;
      elsif Text = "parent" then
         V := Parent;
      elsif Text = "child" then
         V := Child;
      elsif Text = "overriding" then
         V := Overriding_Decl;
      else
         V := AlsReferenceKind'First;
      end if;
   end Read_AlsReferenceKind;

   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : AlsReferenceKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : AlsReferenceKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when Simple =>
               return "reference";
            when Access_Ref =>
               return "access";
            when Write =>
               return "write";
            when Static_Call =>
               return "call";
            when Dispatching_Call =>
               return "dispatching call";
            when Parent =>
               return "parent";
            when Child =>
               return "child";
            when Overriding_Decl =>
               return "overriding";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_AlsReferenceKind;

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsDisplayMethodAncestryOnNavigationPolicy)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "Never" then
         V := Never;
      elsif Text = "Usage_And_Abstract_Only" then
         V := Usage_And_Abstract_Only;
      elsif Text = "Definition_Only" then
         V := Definition_Only;
      elsif Text = "Always" then
         V := Always;
      else
         V := AlsDisplayMethodAncestryOnNavigationPolicy'First;
      end if;
   end Read_AlsDisplayMethodAncestryOnNavigationPolicy;

   procedure Write_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsDisplayMethodAncestryOnNavigationPolicy)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : AlsDisplayMethodAncestryOnNavigationPolicy)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : AlsDisplayMethodAncestryOnNavigationPolicy)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when Never =>
               return "Never";
            when Usage_And_Abstract_Only =>
               return "Usage_And_Abstract_Only";
            when Definition_Only =>
               return "Definition_Only";
            when Always =>
               return "Always";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_AlsDisplayMethodAncestryOnNavigationPolicy;

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "alsKind" then
               AlsReferenceKind_Set'Read (S, V.alsKind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Location;

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_Location;

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "originSelectionRange" then
               Optional_Span'Read (S, V.originSelectionRange);
            elsif Key = "targetUri" then
               LSP.Types.Read_String (S, V.targetUri);
            elsif Key = "targetRange" then
               Span'Read (S, V.targetRange);
            elsif Key = "targetSelectionRange" then
               Span'Read (S, V.targetSelectionRange);
            elsif Key = "alsKind" then
               AlsReferenceKind_Set'Read (S, V.alsKind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LocationLink;

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("originSelectionRange");
      Optional_Span'Write (S, V.originSelectionRange);
      JS.Key ("targetUri");
      LSP.Types.Write_String (S, V.targetUri);
      JS.Key ("targetRange");
      Span'Write (S, V.targetRange);
      JS.Key ("targetSelectionRange");
      Span'Write (S, V.targetSelectionRange);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_LocationLink;

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticSeverity'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_DiagnosticSeverity;

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((DiagnosticSeverity'Pos (V)) + 1);
   end Write_DiagnosticSeverity;

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DiagnosticTag'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_DiagnosticTag;

   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((DiagnosticTag'Pos (V)) + 1);
   end Write_DiagnosticTag;

   procedure Read_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeDescription)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "href" then
               URI'Read (S, V.href);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeDescription;

   procedure Write_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeDescription)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("href");
      URI'Write (S, V.href);
      JS.End_Object;
   end Write_CodeDescription;

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "location" then
               LSP.Messages.Location'Read (S, V.location);
            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DiagnosticRelatedInformation;

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.End_Object;
   end Write_DiagnosticRelatedInformation;

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "severity" then
               Optional_DiagnosticSeverity'Read (S, V.severity);
            elsif Key = "code" then
               LSP_Number_Or_String'Read (S, V.code);
            elsif Key = "codeDescription" then
               Optional_CodeDescription'Read (S, V.codeDescription);
            elsif Key = "source" then
               Optional_Virtual_String'Read (S, V.source);
            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);
            elsif Key = "tags" then
               Optional_DiagnosticTagSet'Read (S, V.tags);
            elsif Key = "relatedInformation" then
               DiagnosticRelatedInformation_Vector'Read (S, V.relatedInformation);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Diagnostic;

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("severity");
      Optional_DiagnosticSeverity'Write (S, V.severity);
      JS.Key ("code");
      LSP_Number_Or_String'Write (S, V.code);
      JS.Key ("codeDescription");
      Optional_CodeDescription'Write (S, V.codeDescription);
      JS.Key ("source");
      Optional_Virtual_String'Write (S, V.source);
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.Key ("tags");
      Optional_DiagnosticTagSet'Write (S, V.tags);
      JS.Key ("relatedInformation");
      DiagnosticRelatedInformation_Vector'Write (S, V.relatedInformation);
      JS.End_Object;
   end Write_Diagnostic;

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "newText" then
               LSP.Types.Read_String (S, V.newText);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextEdit;

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("newText");
      LSP.Types.Write_String (S, V.newText);
      JS.End_Object;
   end Write_TextEdit;

   procedure Read_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotatedTextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "newText" then
               LSP.Types.Read_String (S, V.newText);
            elsif Key = "annotationId" then
               Optional_Virtual_String'Read (S, V.annotationId);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AnnotatedTextEdit;

   procedure Write_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotatedTextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("newText");
      LSP.Types.Write_String (S, V.newText);
      JS.Key ("annotationId");
      Optional_Virtual_String'Write (S, V.annotationId);
      JS.End_Object;
   end Write_AnnotatedTextEdit;

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentIdentifier;

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.End_Object;
   end Write_TextDocumentIdentifier;

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "version" then
               LSP_Number'Read (S, V.version);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_VersionedTextDocumentIdentifier;

   procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("version");
      LSP_Number'Write (S, V.version);
      JS.End_Object;
   end Write_VersionedTextDocumentIdentifier;

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out OptionalVersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "version" then
               Nullable_Number'Read (S, V.version);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_OptionalVersionedTextDocumentIdentifier;

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : OptionalVersionedTextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("version");
      Nullable_Number'Write (S, V.version);
      JS.End_Object;
   end Write_OptionalVersionedTextDocumentIdentifier;

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               OptionalVersionedTextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "edits" then
               AnnotatedTextEdit_Vector'Read (S, V.edits);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentEdit;

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      OptionalVersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("edits");
      AnnotatedTextEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_TextDocumentEdit;

   procedure Read_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileResourceChangeKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "create" then
         V := create;
      elsif Text = "rename" then
         V := rename;
      elsif Text = "delete" then
         V := delete;
      else
         V := FileResourceChangeKind'First;
      end if;
   end Read_FileResourceChangeKind;

   procedure Write_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileResourceChangeKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : FileResourceChangeKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : FileResourceChangeKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when create =>
               return "create";
            when rename =>
               return "rename";
            when delete =>
               return "delete";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_FileResourceChangeKind;

   procedure Read_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CreateFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "overwrite" then
               Optional_Boolean'Read (S, V.overwrite);
            elsif Key = "ignoreIfExists" then
               Optional_Boolean'Read (S, V.ignoreIfExists);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CreateFileOptions;

   procedure Write_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CreateFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("overwrite");
      Optional_Boolean'Write (S, V.overwrite);
      JS.Key ("ignoreIfExists");
      Optional_Boolean'Write (S, V.ignoreIfExists);
      JS.End_Object;
   end Write_CreateFileOptions;

   procedure Read_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CreateFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               FileResourceChangeKind'Read (S, V.kind);
            elsif Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "options" then
               CreateFileOptions'Read (S, V.options);
            elsif Key = "annotationId" then
               Optional_Virtual_String'Read (S, V.annotationId);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CreateFile;

   procedure Write_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CreateFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      FileResourceChangeKind'Write (S, V.kind);
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("options");
      CreateFileOptions'Write (S, V.options);
      JS.Key ("annotationId");
      Optional_Virtual_String'Write (S, V.annotationId);
      JS.End_Object;
   end Write_CreateFile;

   procedure Read_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "overwrite" then
               Optional_Boolean'Read (S, V.overwrite);
            elsif Key = "ignoreIfExists" then
               Optional_Boolean'Read (S, V.ignoreIfExists);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameFileOptions;

   procedure Write_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("overwrite");
      Optional_Boolean'Write (S, V.overwrite);
      JS.Key ("ignoreIfExists");
      Optional_Boolean'Write (S, V.ignoreIfExists);
      JS.End_Object;
   end Write_RenameFileOptions;

   procedure Read_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               FileResourceChangeKind'Read (S, V.kind);
            elsif Key = "oldUri" then
               LSP.Types.Read_LSP_URI (S, V.oldUri);
            elsif Key = "newUri" then
               LSP.Types.Read_LSP_URI (S, V.newUri);
            elsif Key = "options" then
               RenameFileOptions'Read (S, V.options);
            elsif Key = "annotationId" then
               Optional_Virtual_String'Read (S, V.annotationId);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameFile;

   procedure Write_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      FileResourceChangeKind'Write (S, V.kind);
      JS.Key ("oldUri");
      LSP.Types.Write_LSP_URI (S, V.oldUri);
      JS.Key ("newUri");
      LSP.Types.Write_LSP_URI (S, V.newUri);
      JS.Key ("options");
      RenameFileOptions'Write (S, V.options);
      JS.Key ("annotationId");
      Optional_Virtual_String'Write (S, V.annotationId);
      JS.End_Object;
   end Write_RenameFile;

   procedure Read_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeleteFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "recursive" then
               Optional_Boolean'Read (S, V.recursive);
            elsif Key = "ignoreIfNotExists" then
               Optional_Boolean'Read (S, V.ignoreIfNotExists);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DeleteFileOptions;

   procedure Write_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeleteFileOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("recursive");
      Optional_Boolean'Write (S, V.recursive);
      JS.Key ("ignoreIfNotExists");
      Optional_Boolean'Write (S, V.ignoreIfNotExists);
      JS.End_Object;
   end Write_DeleteFileOptions;

   procedure Read_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeleteFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               FileResourceChangeKind'Read (S, V.kind);
            elsif Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "options" then
               DeleteFileOptions'Read (S, V.options);
            elsif Key = "annotationId" then
               Optional_Virtual_String'Read (S, V.annotationId);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DeleteFile;

   procedure Write_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeleteFile)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      FileResourceChangeKind'Write (S, V.kind);
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("options");
      DeleteFileOptions'Write (S, V.options);
      JS.Key ("annotationId");
      Optional_Virtual_String'Write (S, V.annotationId);
      JS.End_Object;
   end Write_DeleteFile;

   procedure Read_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ChangeAnnotation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               LSP.Types.Read_String (S, V.label);
            elsif Key = "needsConfirmation" then
               Optional_Boolean'Read (S, V.needsConfirmation);
            elsif Key = "description" then
               Optional_Virtual_String'Read (S, V.description);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ChangeAnnotation;

   procedure Write_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ChangeAnnotation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write_String (S, V.label);
      JS.Key ("needsConfirmation");
      Optional_Boolean'Write (S, V.needsConfirmation);
      JS.Key ("description");
      Optional_Virtual_String'Write (S, V.description);
      JS.End_Object;
   end Write_ChangeAnnotation;

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "languageId" then
               LSP.Types.Read_String (S, V.languageId);
            elsif Key = "version" then
               Version_Id'Read (S, V.version);
            elsif Key = "text" then
               LSP.Types.Read_String (S, V.text);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentItem;

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("languageId");
      LSP.Types.Write_String (S, V.languageId);
      JS.Key ("version");
      Version_Id'Write (S, V.version);
      JS.Key ("text");
      LSP.Types.Write_String (S, V.text);
      JS.End_Object;
   end Write_TextDocumentItem;

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentPositionParams;

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.End_Object;
   end Write_TextDocumentPositionParams;

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_dynamicRegistration;

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.End_Object;
   end Write_dynamicRegistration;

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResourceOperationKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "create" then
         V := create;
      elsif Text = "rename" then
         V := rename;
      elsif Text = "delete" then
         V := delete;
      else
         V := ResourceOperationKind'First;
      end if;
   end Read_ResourceOperationKind;

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResourceOperationKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : ResourceOperationKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : ResourceOperationKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when create =>
               return "create";
            when rename =>
               return "rename";
            when delete =>
               return "delete";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_ResourceOperationKind;

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FailureHandlingKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "abort" then
         V := abortApplying;
      elsif Text = "transactional" then
         V := transactional;
      elsif Text = "undo" then
         V := undo;
      elsif Text = "textOnlyTransactional" then
         V := textOnlyTransactional;
      else
         V := FailureHandlingKind'First;
      end if;
   end Read_FailureHandlingKind;

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FailureHandlingKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : FailureHandlingKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : FailureHandlingKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when abortApplying =>
               return "abort";
            when transactional =>
               return "transactional";
            when undo =>
               return "undo";
            when textOnlyTransactional =>
               return "textOnlyTransactional";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_FailureHandlingKind;

   procedure Read_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotationSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "groupsOnLabel" then
               Optional_Boolean'Read (S, V.groupsOnLabel);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_AnnotationSupport;

   procedure Write_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotationSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("groupsOnLabel");
      Optional_Boolean'Write (S, V.groupsOnLabel);
      JS.End_Object;
   end Write_AnnotationSupport;

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentChanges" then
               Optional_Boolean'Read (S, V.documentChanges);
            elsif Key = "resourceOperations" then
               Optional_ResourceOperationKindSet'Read (S, V.resourceOperations);
            elsif Key = "failureHandling" then
               Optional_FailureHandlingKind'Read (S, V.failureHandling);
            elsif Key = "normalizesLineEndings" then
               Optional_Boolean'Read (S, V.normalizesLineEndings);
            elsif Key = "changeAnnotationSupport" then
               Optional_AnnotationSupport'Read (S, V.changeAnnotationSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceEditClientCapabilities;

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentChanges");
      Optional_Boolean'Write (S, V.documentChanges);
      JS.Key ("resourceOperations");
      Optional_ResourceOperationKindSet'Write (S, V.resourceOperations);
      JS.Key ("failureHandling");
      Optional_FailureHandlingKind'Write (S, V.failureHandling);
      JS.Key ("normalizesLineEndings");
      Optional_Boolean'Write (S, V.normalizesLineEndings);
      JS.Key ("changeAnnotationSupport");
      Optional_AnnotationSupport'Write (S, V.changeAnnotationSupport);
      JS.End_Object;
   end Write_WorkspaceEditClientCapabilities;

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := SymbolKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_SymbolKind;

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((SymbolKind'Pos (V)) + 1);
   end Write_SymbolKind;

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out symbolKindCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               Optional_SymbolKindSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_symbolKindCapabilities;

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : symbolKindCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_SymbolKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_symbolKindCapabilities;

   procedure Read_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := SymbolTag'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_SymbolTag;

   procedure Write_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((SymbolTag'Pos (V)) + 1);
   end Write_SymbolTag;

   procedure Read_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out tagSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               SymbolTagSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_tagSupportCapability;

   procedure Write_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : tagSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      SymbolTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_tagSupportCapability;

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Als_Visibility)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := Als_Visibility'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_Als_Visibility;

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Als_Visibility)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((Als_Visibility'Pos (V)) + 1);
   end Write_Als_Visibility;

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "symbolKind" then
               Optional_symbolKindCapabilities'Read (S, V.symbolKind);
            elsif Key = "tagSupport" then
               Optional_tagSupportCapability'Read (S, V.tagSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceSymbolClientCapabilities;

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Write (S, V.symbolKind);
      JS.Key ("tagSupport");
      Optional_tagSupportCapability'Write (S, V.tagSupport);
      JS.End_Object;
   end Write_WorkspaceSymbolClientCapabilities;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensWorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "refreshSupport" then
               Optional_Boolean'Read (S, V.refreshSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensWorkspaceClientCapabilities;

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensWorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("refreshSupport");
      Optional_Boolean'Write (S, V.refreshSupport);
      JS.End_Object;
   end Write_SemanticTokensWorkspaceClientCapabilities;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensWorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "refreshSupport" then
               Optional_Boolean'Read (S, V.refreshSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeLensWorkspaceClientCapabilities;

   procedure Write_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensWorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("refreshSupport");
      Optional_Boolean'Write (S, V.refreshSupport);
      JS.End_Object;
   end Write_CodeLensWorkspaceClientCapabilities;

   procedure Read_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "didCreate" then
               Optional_Boolean'Read (S, V.didCreate);
            elsif Key = "willCreate" then
               Optional_Boolean'Read (S, V.willCreate);
            elsif Key = "didRename" then
               Optional_Boolean'Read (S, V.didRename);
            elsif Key = "willRename" then
               Optional_Boolean'Read (S, V.willRename);
            elsif Key = "didDelete" then
               Optional_Boolean'Read (S, V.didDelete);
            elsif Key = "willDelete" then
               Optional_Boolean'Read (S, V.willDelete);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationsClientCapabilities;

   procedure Write_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("didCreate");
      Optional_Boolean'Write (S, V.didCreate);
      JS.Key ("willCreate");
      Optional_Boolean'Write (S, V.willCreate);
      JS.Key ("didRename");
      Optional_Boolean'Write (S, V.didRename);
      JS.Key ("willRename");
      Optional_Boolean'Write (S, V.willRename);
      JS.Key ("didDelete");
      Optional_Boolean'Write (S, V.didDelete);
      JS.Key ("willDelete");
      Optional_Boolean'Write (S, V.willDelete);
      JS.End_Object;
   end Write_FileOperationsClientCapabilities;

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "applyEdit" then
               Optional_Boolean'Read (S, V.applyEdit);
            elsif Key = "workspaceEdit" then
               WorkspaceEditClientCapabilities'Read (S, V.workspaceEdit);
            elsif Key = "didChangeConfiguration" then
               DidChangeConfigurationClientCapabilities'Read (S, V.didChangeConfiguration);
            elsif Key = "didChangeWatchedFiles" then
               DidChangeWatchedFilesClientCapabilities'Read (S, V.didChangeWatchedFiles);
            elsif Key = "symbol" then
               Optional_WorkspaceSymbolClientCapabilities'Read (S, V.symbol);
            elsif Key = "executeCommand" then
               ExecuteCommandClientCapabilities'Read (S, V.executeCommand);
            elsif Key = "workspaceFolders" then
               Optional_Boolean'Read (S, V.workspaceFolders);
            elsif Key = "configuration" then
               Optional_Boolean'Read (S, V.configuration);
            elsif Key = "semanticTokens" then
               Optional_SemanticTokensWorkspaceClientCapabilities'Read (S, V.semanticTokens);
            elsif Key = "codeLens" then
               Optional_CodeLensWorkspaceClientCapabilities'Read (S, V.codeLens);
            elsif Key = "fileOperations" then
               Optional_FileOperationsClientCapabilities'Read (S, V.fileOperations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceClientCapabilities;

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("applyEdit");
      Optional_Boolean'Write (S, V.applyEdit);
      JS.Key ("workspaceEdit");
      WorkspaceEditClientCapabilities'Write (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      DidChangeConfigurationClientCapabilities'Write (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      DidChangeWatchedFilesClientCapabilities'Write (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      Optional_WorkspaceSymbolClientCapabilities'Write (S, V.symbol);
      JS.Key ("executeCommand");
      ExecuteCommandClientCapabilities'Write (S, V.executeCommand);
      JS.Key ("workspaceFolders");
      Optional_Boolean'Write (S, V.workspaceFolders);
      JS.Key ("configuration");
      Optional_Boolean'Write (S, V.configuration);
      JS.Key ("semanticTokens");
      Optional_SemanticTokensWorkspaceClientCapabilities'Write (S, V.semanticTokens);
      JS.Key ("codeLens");
      Optional_CodeLensWorkspaceClientCapabilities'Write (S, V.codeLens);
      JS.Key ("fileOperations");
      Optional_FileOperationsClientCapabilities'Write (S, V.fileOperations);
      JS.End_Object;
   end Write_WorkspaceClientCapabilities;

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "plaintext" then
         V := plaintext;
      elsif Text = "markdown" then
         V := markdown;
      else
         V := MarkupKind'First;
      end if;
   end Read_MarkupKind;

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : MarkupKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : MarkupKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when plaintext =>
               return "plaintext";
            when markdown =>
               return "markdown";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_MarkupKind;

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               MarkupKind'Read (S, V.kind);
            elsif Key = "value" then
               LSP.Types.Read_String (S, V.value);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_MarkupContent;

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      MarkupKind'Write (S, V.kind);
      JS.Key ("value");
      LSP.Types.Write_String (S, V.value);
      JS.End_Object;
   end Write_MarkupContent;

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "includeText" then
               Optional_Boolean'Read (S, V.includeText);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SaveOptions;

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeText");
      Optional_Boolean'Write (S, V.includeText);
      JS.End_Object;
   end Write_SaveOptions;

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := TextDocumentSyncKind'Val (JS.R.Number_Value.Integer_Value - 0);
      JS.R.Read_Next;
   end Read_TextDocumentSyncKind;

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((TextDocumentSyncKind'Pos (V)) + 0);
   end Write_TextDocumentSyncKind;

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "openClose" then
               Optional_Boolean'Read (S, V.openClose);
            elsif Key = "change" then
               Optional_TextDocumentSyncKind'Read (S, V.change);
            elsif Key = "willSave" then
               Optional_Boolean'Read (S, V.willSave);
            elsif Key = "willSaveWaitUntil" then
               Optional_Boolean'Read (S, V.willSaveWaitUntil);
            elsif Key = "save" then
               Optional_SaveOptions'Read (S, V.save);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentSyncOptions;

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("openClose");
      Optional_Boolean'Write (S, V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Write (S, V.change);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("save");
      Optional_SaveOptions'Write (S, V.save);
      JS.End_Object;
   end Write_TextDocumentSyncOptions;

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "willSave" then
               Optional_Boolean'Read (S, V.willSave);
            elsif Key = "willSaveWaitUntil" then
               Optional_Boolean'Read (S, V.willSaveWaitUntil);
            elsif Key = "didSave" then
               Optional_Boolean'Read (S, V.didSave);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentSyncClientCapabilities;

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("didSave");
      Optional_Boolean'Write (S, V.didSave);
      JS.End_Object;
   end Write_TextDocumentSyncClientCapabilities;

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemTag'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_CompletionItemTag;

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTag)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((CompletionItemTag'Pos (V)) + 1);
   end Write_CompletionItemTag;

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               CompletionItemTagSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionItemTagSupport;

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CompletionItemTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_CompletionItemTagSupport;

   procedure Read_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out resolveSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "properties" then
               LSP.Types.Read_String_Vector (S, V.properties);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_resolveSupportCapability;

   procedure Write_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : resolveSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("properties");
      LSP.Types.Write_String_Vector (S, V.properties);
      JS.End_Object;
   end Write_resolveSupportCapability;

   procedure Read_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextMode)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := InsertTextMode'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_InsertTextMode;

   procedure Write_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextMode)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((InsertTextMode'Pos (V)) + 1);
   end Write_InsertTextMode;

   procedure Read_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out insertTextModeSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               InsertTextModeSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_insertTextModeSupportCapability;

   procedure Write_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : insertTextModeSupportCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      InsertTextModeSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_insertTextModeSupportCapability;

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "snippetSupport" then
               Optional_Boolean'Read (S, V.snippetSupport);
            elsif Key = "commitCharactersSupport" then
               Optional_Boolean'Read (S, V.commitCharactersSupport);
            elsif Key = "documentationFormat" then
               MarkupKind_Vector'Read (S, V.documentationFormat);
            elsif Key = "deprecatedSupport" then
               Optional_Boolean'Read (S, V.deprecatedSupport);
            elsif Key = "preselectSupport" then
               Optional_Boolean'Read (S, V.preselectSupport);
            elsif Key = "tagSupport" then
               Optional_CompletionItemTagSupport'Read (S, V.tagSupport);
            elsif Key = "insertReplaceSupport" then
               Optional_Boolean'Read (S, V.insertReplaceSupport);
            elsif Key = "resolveSupport" then
               Optional_resolveSupportCapability'Read (S, V.resolveSupport);
            elsif Key = "insertTextModeSupport" then
               Optional_insertTextModeSupportCapability'Read (S, V.insertTextModeSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_completionItemCapability;

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("snippetSupport");
      Optional_Boolean'Write (S, V.snippetSupport);
      JS.Key ("commitCharactersSupport");
      Optional_Boolean'Write (S, V.commitCharactersSupport);
      JS.Key ("documentationFormat");
      MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("deprecatedSupport");
      Optional_Boolean'Write (S, V.deprecatedSupport);
      JS.Key ("preselectSupport");
      Optional_Boolean'Write (S, V.preselectSupport);
      JS.Key ("tagSupport");
      Optional_CompletionItemTagSupport'Write (S, V.tagSupport);
      JS.Key ("insertReplaceSupport");
      Optional_Boolean'Write (S, V.insertReplaceSupport);
      JS.Key ("resolveSupport");
      Optional_resolveSupportCapability'Write (S, V.resolveSupport);
      JS.Key ("insertTextModeSupport");
      Optional_insertTextModeSupportCapability'Write (S, V.insertTextModeSupport);
      JS.End_Object;
   end Write_completionItemCapability;

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionItemKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_CompletionItemKind;

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((CompletionItemKind'Pos (V)) + 1);
   end Write_CompletionItemKind;

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKindSetCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               Optional_CompletionItemKindSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionItemKindSetCapabilities;

   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKindSetCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      Optional_CompletionItemKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_CompletionItemKindSetCapabilities;

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "completionItem" then
               Optional_completionItemCapability'Read (S, V.completionItem);
            elsif Key = "completionItemKind" then
               Optional_CompletionItemKindSetCapabilities'Read (S, V.completionItemKind);
            elsif Key = "contextSupport" then
               Optional_Boolean'Read (S, V.contextSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionClientCapabilities;

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("completionItem");
      Optional_completionItemCapability'Write (S, V.completionItem);
      JS.Key ("completionItemKind");
      Optional_CompletionItemKindSetCapabilities'Write (S, V.completionItemKind);
      JS.Key ("contextSupport");
      Optional_Boolean'Write (S, V.contextSupport);
      JS.End_Object;
   end Write_CompletionClientCapabilities;

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "contentFormat" then
               Optional_MarkupKind_Vector'Read (S, V.contentFormat);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_HoverClientCapabilities;

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("contentFormat");
      Optional_MarkupKind_Vector'Write (S, V.contentFormat);
      JS.End_Object;
   end Write_HoverClientCapabilities;

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "labelOffsetSupport" then
               Optional_Boolean'Read (S, V.labelOffsetSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_parameterInformation_Capability;

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("labelOffsetSupport");
      Optional_Boolean'Write (S, V.labelOffsetSupport);
      JS.End_Object;
   end Write_parameterInformation_Capability;

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentationFormat" then
               Optional_MarkupKind_Vector'Read (S, V.documentationFormat);
            elsif Key = "parameterInformation" then
               Optional_parameterInformation_Capability'Read (S, V.parameterInformation);
            elsif Key = "activeParameterSupport" then
               Optional_Boolean'Read (S, V.activeParameterSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_signatureInformation_Capability;

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentationFormat");
      Optional_MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("parameterInformation");
      Optional_parameterInformation_Capability'Write (S, V.parameterInformation);
      JS.Key ("activeParameterSupport");
      Optional_Boolean'Write (S, V.activeParameterSupport);
      JS.End_Object;
   end Write_signatureInformation_Capability;

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "signatureInformation" then
               Optional_signatureInformation_Capability'Read (S, V.signatureInformation);
            elsif Key = "contextSupport" then
               Optional_Boolean'Read (S, V.contextSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelpClientCapabilities;

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("signatureInformation");
      Optional_signatureInformation_Capability'Write (S, V.signatureInformation);
      JS.Key ("contextSupport");
      Optional_Boolean'Write (S, V.contextSupport);
      JS.End_Object;
   end Write_SignatureHelpClientCapabilities;

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "symbolKind" then
               Optional_symbolKindCapabilities'Read (S, V.symbolKind);
            elsif Key = "labelSupport" then
               Optional_Boolean'Read (S, V.labelSupport);
            elsif Key = "hierarchicalDocumentSymbolSupport" then
               Optional_Boolean'Read (S, V.hierarchicalDocumentSymbolSupport);
            elsif Key = "tagSupport" then
               Optional_tagSupportCapability'Read (S, V.tagSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentSymbolClientCapabilities;

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("symbolKind");
      Optional_symbolKindCapabilities'Write (S, V.symbolKind);
      JS.Key ("labelSupport");
      Optional_Boolean'Write (S, V.labelSupport);
      JS.Key ("hierarchicalDocumentSymbolSupport");
      Optional_Boolean'Write (S, V.hierarchicalDocumentSymbolSupport);
      JS.Key ("tagSupport");
      Optional_tagSupportCapability'Write (S, V.tagSupport);
      JS.End_Object;
   end Write_DocumentSymbolClientCapabilities;

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "linkSupport" then
               Optional_Boolean'Read (S, V.linkSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DeclarationClientCapabilities;

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("linkSupport");
      Optional_Boolean'Write (S, V.linkSupport);
      JS.End_Object;
   end Write_DeclarationClientCapabilities;

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionKindCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               CodeActionKindSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_codeActionKindCapability;

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionKindCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CodeActionKindSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_codeActionKindCapability;

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionLiteralSupport_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "codeActionKind" then
               codeActionKindCapability'Read (S, V.codeActionKind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_codeActionLiteralSupport_Capability;

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionLiteralSupport_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("codeActionKind");
      codeActionKindCapability'Write (S, V.codeActionKind);
      JS.End_Object;
   end Write_codeActionLiteralSupport_Capability;

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "codeActionLiteralSupport" then
               Optional_codeActionLiteralSupport_Capability'Read (S, V.codeActionLiteralSupport);
            elsif Key = "isPreferredSupport" then
               Optional_Boolean'Read (S, V.isPreferredSupport);
            elsif Key = "disabledSupport" then
               Optional_Boolean'Read (S, V.disabledSupport);
            elsif Key = "dataSupport" then
               Optional_Boolean'Read (S, V.dataSupport);
            elsif Key = "resolveSupport" then
               Optional_resolveSupportCapability'Read (S, V.resolveSupport);
            elsif Key = "honorsChangeAnnotations" then
               Optional_Boolean'Read (S, V.honorsChangeAnnotations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeActionClientCapabilities;

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("codeActionLiteralSupport");
      Optional_codeActionLiteralSupport_Capability'Write (S, V.codeActionLiteralSupport);
      JS.Key ("isPreferredSupport");
      Optional_Boolean'Write (S, V.isPreferredSupport);
      JS.Key ("disabledSupport");
      Optional_Boolean'Write (S, V.disabledSupport);
      JS.Key ("dataSupport");
      Optional_Boolean'Write (S, V.dataSupport);
      JS.Key ("resolveSupport");
      Optional_resolveSupportCapability'Write (S, V.resolveSupport);
      JS.Key ("honorsChangeAnnotations");
      Optional_Boolean'Write (S, V.honorsChangeAnnotations);
      JS.End_Object;
   end Write_CodeActionClientCapabilities;

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "tooltipSupport" then
               Optional_Boolean'Read (S, V.tooltipSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentLinkClientCapabilities;

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("tooltipSupport");
      Optional_Boolean'Write (S, V.tooltipSupport);
      JS.End_Object;
   end Write_DocumentLinkClientCapabilities;

   procedure Read_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PrepareSupportDefaultBehavior)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := PrepareSupportDefaultBehavior'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_PrepareSupportDefaultBehavior;

   procedure Write_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PrepareSupportDefaultBehavior)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((PrepareSupportDefaultBehavior'Pos (V)) + 1);
   end Write_PrepareSupportDefaultBehavior;

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "prepareSupport" then
               Optional_Boolean'Read (S, V.prepareSupport);
            elsif Key = "prepareSupportDefaultBehavior" then
               Optional_PrepareSupportDefaultBehavior'Read (S, V.prepareSupportDefaultBehavior);
            elsif Key = "honorsChangeAnnotations" then
               Optional_Boolean'Read (S, V.honorsChangeAnnotations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameClientCapabilities;

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("prepareSupport");
      Optional_Boolean'Write (S, V.prepareSupport);
      JS.Key ("prepareSupportDefaultBehavior");
      Optional_PrepareSupportDefaultBehavior'Write (S, V.prepareSupportDefaultBehavior);
      JS.Key ("honorsChangeAnnotations");
      Optional_Boolean'Write (S, V.honorsChangeAnnotations);
      JS.End_Object;
   end Write_RenameClientCapabilities;

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "valueSet" then
               DiagnosticTagSet'Read (S, V.valueSet);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DiagnosticTagSupport;

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      DiagnosticTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_DiagnosticTagSupport;

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "relatedInformation" then
               Optional_Boolean'Read (S, V.relatedInformation);
            elsif Key = "tagSupport" then
               Optional_DiagnosticTagSupport'Read (S, V.tagSupport);
            elsif Key = "versionSupport" then
               Optional_Boolean'Read (S, V.versionSupport);
            elsif Key = "codeDescriptionSupport" then
               Optional_Boolean'Read (S, V.codeDescriptionSupport);
            elsif Key = "dataSupport" then
               Optional_Boolean'Read (S, V.dataSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PublishDiagnosticsClientCapabilities;

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("relatedInformation");
      Optional_Boolean'Write (S, V.relatedInformation);
      JS.Key ("tagSupport");
      Optional_DiagnosticTagSupport'Write (S, V.tagSupport);
      JS.Key ("versionSupport");
      Optional_Boolean'Write (S, V.versionSupport);
      JS.Key ("codeDescriptionSupport");
      Optional_Boolean'Write (S, V.codeDescriptionSupport);
      JS.Key ("dataSupport");
      Optional_Boolean'Write (S, V.dataSupport);
      JS.End_Object;
   end Write_PublishDiagnosticsClientCapabilities;

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "rangeLimit" then
               Optional_Number'Read (S, V.rangeLimit);
            elsif Key = "lineFoldingOnly" then
               Optional_Boolean'Read (S, V.lineFoldingOnly);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FoldingRangeClientCapabilities;

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("rangeLimit");
      Optional_Number'Write (S, V.rangeLimit);
      JS.Key ("lineFoldingOnly");
      Optional_Boolean'Write (S, V.lineFoldingOnly);
      JS.End_Object;
   end Write_FoldingRangeClientCapabilities;

   procedure Read_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenTypes)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "namespace" then
         V := namespace;
      elsif Text = "type" then
         V := a_type;
      elsif Text = "class" then
         V := class;
      elsif Text = "enum" then
         V := enum;
      elsif Text = "interface" then
         V := an_interface;
      elsif Text = "struct" then
         V := struct;
      elsif Text = "typeParameter" then
         V := typeParameter;
      elsif Text = "parameter" then
         V := parameter;
      elsif Text = "variable" then
         V := variable;
      elsif Text = "property" then
         V := property;
      elsif Text = "enumMember" then
         V := enumMember;
      elsif Text = "event" then
         V := event;
      elsif Text = "function" then
         V := a_function;
      elsif Text = "method" then
         V := method;
      elsif Text = "macro" then
         V := macro;
      elsif Text = "keyword" then
         V := keyword;
      elsif Text = "modifier" then
         V := modifier;
      elsif Text = "comment" then
         V := comment;
      elsif Text = "string" then
         V := a_string;
      elsif Text = "number" then
         V := number;
      elsif Text = "regexp" then
         V := regexp;
      elsif Text = "operator" then
         V := operator;
      else
         V := SemanticTokenTypes'First;
      end if;
   end Read_SemanticTokenTypes;

   procedure Write_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenTypes)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : SemanticTokenTypes)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : SemanticTokenTypes)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when namespace =>
               return "namespace";
            when a_type =>
               return "type";
            when class =>
               return "class";
            when enum =>
               return "enum";
            when an_interface =>
               return "interface";
            when struct =>
               return "struct";
            when typeParameter =>
               return "typeParameter";
            when parameter =>
               return "parameter";
            when variable =>
               return "variable";
            when property =>
               return "property";
            when enumMember =>
               return "enumMember";
            when event =>
               return "event";
            when a_function =>
               return "function";
            when method =>
               return "method";
            when macro =>
               return "macro";
            when keyword =>
               return "keyword";
            when modifier =>
               return "modifier";
            when comment =>
               return "comment";
            when a_string =>
               return "string";
            when number =>
               return "number";
            when regexp =>
               return "regexp";
            when operator =>
               return "operator";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_SemanticTokenTypes;

   procedure Read_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenModifiers)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "declaration" then
         V := declaration;
      elsif Text = "definition" then
         V := definition;
      elsif Text = "readonly" then
         V := readonly;
      elsif Text = "static" then
         V := static;
      elsif Text = "deprecated" then
         V := deprecated;
      elsif Text = "abstract" then
         V := an_abstract;
      elsif Text = "async" then
         V := async;
      elsif Text = "modification" then
         V := modification;
      elsif Text = "documentation" then
         V := documentation;
      elsif Text = "defaultLibrary" then
         V := defaultLibrary;
      else
         V := SemanticTokenModifiers'First;
      end if;
   end Read_SemanticTokenModifiers;

   procedure Write_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenModifiers)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : SemanticTokenModifiers)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : SemanticTokenModifiers)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when declaration =>
               return "declaration";
            when definition =>
               return "definition";
            when readonly =>
               return "readonly";
            when static =>
               return "static";
            when deprecated =>
               return "deprecated";
            when an_abstract =>
               return "abstract";
            when async =>
               return "async";
            when modification =>
               return "modification";
            when documentation =>
               return "documentation";
            when defaultLibrary =>
               return "defaultLibrary";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_SemanticTokenModifiers;

   procedure Read_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TokenFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "relative" then
         V := relative;
      else
         V := TokenFormat'First;
      end if;
   end Read_TokenFormat;

   procedure Write_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TokenFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : TokenFormat)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : TokenFormat)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when relative =>
               return "relative";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_TokenFormat;

   procedure Read_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensFullCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "delta" then
               Optional_Boolean'Read (S, V.diff);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensFullCapabilities;

   procedure Write_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensFullCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("delta");
      Optional_Boolean'Write (S, V.diff);
      JS.End_Object;
   end Write_SemanticTokensFullCapabilities;

   procedure Read_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRequestCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               Optional_Boolean'Read (S, V.span);
            elsif Key = "full" then
               Optional_SemanticTokensFullCapabilities'Read (S, V.full);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensRequestCapabilities;

   procedure Write_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRequestCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Boolean'Write (S, V.span);
      JS.Key ("full");
      Optional_SemanticTokensFullCapabilities'Write (S, V.full);
      JS.End_Object;
   end Write_SemanticTokensRequestCapabilities;

   procedure Read_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "dynamicRegistration" then
               Optional_Boolean'Read (S, V.dynamicRegistration);
            elsif Key = "requests" then
               SemanticTokensRequestCapabilities'Read (S, V.requests);
            elsif Key = "tokenTypes" then
               SemanticTokenTypes_Vector'Read (S, V.tokenTypes);
            elsif Key = "tokenModifiers" then
               SemanticTokenModifiers_Vector'Read (S, V.tokenModifiers);
            elsif Key = "formats" then
               TokenFormatSet'Read (S, V.formats);
            elsif Key = "overlappingTokenSupport" then
               Optional_Boolean'Read (S, V.overlappingTokenSupport);
            elsif Key = "multilineTokenSupport" then
               Optional_Boolean'Read (S, V.multilineTokenSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensClientCapabilities;

   procedure Write_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("requests");
      SemanticTokensRequestCapabilities'Write (S, V.requests);
      JS.Key ("tokenTypes");
      SemanticTokenTypes_Vector'Write (S, V.tokenTypes);
      JS.Key ("tokenModifiers");
      SemanticTokenModifiers_Vector'Write (S, V.tokenModifiers);
      JS.Key ("formats");
      TokenFormatSet'Write (S, V.formats);
      JS.Key ("overlappingTokenSupport");
      Optional_Boolean'Write (S, V.overlappingTokenSupport);
      JS.Key ("multilineTokenSupport");
      Optional_Boolean'Write (S, V.multilineTokenSupport);
      JS.End_Object;
   end Write_SemanticTokensClientCapabilities;

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "synchronization" then
               TextDocumentSyncClientCapabilities'Read (S, V.synchronization);
            elsif Key = "completion" then
               CompletionClientCapabilities'Read (S, V.completion);
            elsif Key = "hover" then
               Optional_HoverClientCapabilities'Read (S, V.hover);
            elsif Key = "signatureHelp" then
               Optional_SignatureHelpClientCapabilities'Read (S, V.signatureHelp);
            elsif Key = "declaration" then
               Optional_DeclarationClientCapabilities'Read (S, V.declaration);
            elsif Key = "definition" then
               Optional_DefinitionClientCapabilities'Read (S, V.definition);
            elsif Key = "typeDefinition" then
               Optional_TypeDefinitionClientCapabilities'Read (S, V.typeDefinition);
            elsif Key = "implementation" then
               Optional_ImplementationClientCapabilities'Read (S, V.implementation);
            elsif Key = "references" then
               ReferenceClientCapabilities'Read (S, V.references);
            elsif Key = "documentHighlight" then
               DocumentHighlightClientCapabilities'Read (S, V.documentHighlight);
            elsif Key = "documentSymbol" then
               Optional_DocumentSymbolClientCapabilities'Read (S, V.documentSymbol);
            elsif Key = "codeAction" then
               Optional_CodeActionClientCapabilities'Read (S, V.codeAction);
            elsif Key = "codeLens" then
               CodeLensClientCapabilities'Read (S, V.codeLens);
            elsif Key = "documentLink" then
               Optional_DocumentLinkClientCapabilities'Read (S, V.documentLink);
            elsif Key = "colorProvider" then
               DocumentColorClientCapabilities'Read (S, V.colorProvider);
            elsif Key = "formatting" then
               DocumentFormattingClientCapabilities'Read (S, V.formatting);
            elsif Key = "rangeFormatting" then
               DocumentRangeFormattingClientCapabilities'Read (S, V.rangeFormatting);
            elsif Key = "onTypeFormatting" then
               DocumentOnTypeFormattingClientCapabilities'Read (S, V.onTypeFormatting);
            elsif Key = "rename" then
               Optional_RenameClientCapabilities'Read (S, V.rename);
            elsif Key = "publishDiagnostics" then
               Optional_PublishDiagnosticsClientCapabilities'Read (S, V.publishDiagnostics);
            elsif Key = "foldingRange" then
               Optional_FoldingRangeClientCapabilities'Read (S, V.foldingRange);
            elsif Key = "selectionRange" then
               SelectionRangeClientCapabilities'Read (S, V.selectionRange);
            elsif Key = "linkedEditingRange" then
               LinkedEditingRangeClientCapabilities'Read (S, V.linkedEditingRange);
            elsif Key = "callHierarchy" then
               CallHierarchyClientCapabilities'Read (S, V.callHierarchy);
            elsif Key = "semanticTokens" then
               Optional_SemanticTokensClientCapabilities'Read (S, V.semanticTokens);
            elsif Key = "moniker" then
               MonikerClientCapabilities'Read (S, V.moniker);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentClientCapabilities;

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      TextDocumentSyncClientCapabilities'Write (S, V.synchronization);
      JS.Key ("completion");
      CompletionClientCapabilities'Write (S, V.completion);
      JS.Key ("hover");
      Optional_HoverClientCapabilities'Write (S, V.hover);
      JS.Key ("signatureHelp");
      Optional_SignatureHelpClientCapabilities'Write (S, V.signatureHelp);
      JS.Key ("declaration");
      Optional_DeclarationClientCapabilities'Write (S, V.declaration);
      JS.Key ("definition");
      Optional_DefinitionClientCapabilities'Write (S, V.definition);
      JS.Key ("typeDefinition");
      Optional_TypeDefinitionClientCapabilities'Write (S, V.typeDefinition);
      JS.Key ("implementation");
      Optional_ImplementationClientCapabilities'Write (S, V.implementation);
      JS.Key ("references");
      ReferenceClientCapabilities'Write (S, V.references);
      JS.Key ("documentHighlight");
      DocumentHighlightClientCapabilities'Write (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      Optional_DocumentSymbolClientCapabilities'Write (S, V.documentSymbol);
      JS.Key ("codeAction");
      Optional_CodeActionClientCapabilities'Write (S, V.codeAction);
      JS.Key ("codeLens");
      CodeLensClientCapabilities'Write (S, V.codeLens);
      JS.Key ("documentLink");
      Optional_DocumentLinkClientCapabilities'Write (S, V.documentLink);
      JS.Key ("colorProvider");
      DocumentColorClientCapabilities'Write (S, V.colorProvider);
      JS.Key ("formatting");
      DocumentFormattingClientCapabilities'Write (S, V.formatting);
      JS.Key ("rangeFormatting");
      DocumentRangeFormattingClientCapabilities'Write (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      DocumentOnTypeFormattingClientCapabilities'Write (S, V.onTypeFormatting);
      JS.Key ("rename");
      Optional_RenameClientCapabilities'Write (S, V.rename);
      JS.Key ("publishDiagnostics");
      Optional_PublishDiagnosticsClientCapabilities'Write (S, V.publishDiagnostics);
      JS.Key ("foldingRange");
      Optional_FoldingRangeClientCapabilities'Write (S, V.foldingRange);
      JS.Key ("selectionRange");
      SelectionRangeClientCapabilities'Write (S, V.selectionRange);
      JS.Key ("linkedEditingRange");
      LinkedEditingRangeClientCapabilities'Write (S, V.linkedEditingRange);
      JS.Key ("callHierarchy");
      CallHierarchyClientCapabilities'Write (S, V.callHierarchy);
      JS.Key ("semanticTokens");
      Optional_SemanticTokensClientCapabilities'Write (S, V.semanticTokens);
      JS.Key ("moniker");
      MonikerClientCapabilities'Write (S, V.moniker);
      JS.End_Object;
   end Write_TextDocumentClientCapabilities;

   procedure Read_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "support" then
               LSP.Types.Read_Boolean (JS, V.support);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowDocumentClientCapabilities;

   procedure Write_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, "support", V.support);
      JS.End_Object;
   end Write_ShowDocumentClientCapabilities;

   procedure Read_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageActionItemCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "additionalPropertiesSupport" then
               Optional_Boolean'Read (S, V.additionalPropertiesSupport);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_MessageActionItemCapabilities;

   procedure Write_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageActionItemCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("additionalPropertiesSupport");
      Optional_Boolean'Write (S, V.additionalPropertiesSupport);
      JS.End_Object;
   end Write_MessageActionItemCapabilities;

   procedure Read_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "messageActionItem" then
               Optional_MessageActionItemCapabilities'Read (S, V.messageActionItem);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowMessageRequestClientCapabilities;

   procedure Write_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("messageActionItem");
      Optional_MessageActionItemCapabilities'Write (S, V.messageActionItem);
      JS.End_Object;
   end Write_ShowMessageRequestClientCapabilities;

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "showMessage" then
               Optional_ShowMessageRequestClientCapabilities'Read (S, V.showMessage);
            elsif Key = "showDocument" then
               Optional_ShowDocumentClientCapabilities'Read (S, V.showDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WindowClientCapabilities;

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("showMessage");
      Optional_ShowMessageRequestClientCapabilities'Write (S, V.showMessage);
      JS.Key ("showDocument");
      Optional_ShowDocumentClientCapabilities'Write (S, V.showDocument);
      JS.End_Object;
   end Write_WindowClientCapabilities;

   procedure Read_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkdownClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "parser" then
               LSP.Types.Read_String (S, V.parser);
            elsif Key = "version" then
               Optional_Virtual_String'Read (S, V.version);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_MarkdownClientCapabilities;

   procedure Write_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkdownClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("parser");
      LSP.Types.Write_String (S, V.parser);
      JS.Key ("version");
      Optional_Virtual_String'Write (S, V.version);
      JS.End_Object;
   end Write_MarkdownClientCapabilities;

   procedure Read_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegularExpressionsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "engine" then
               LSP.Types.Read_String (S, V.engine);
            elsif Key = "version" then
               Optional_Virtual_String'Read (S, V.version);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RegularExpressionsClientCapabilities;

   procedure Write_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegularExpressionsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("engine");
      LSP.Types.Write_String (S, V.engine);
      JS.Key ("version");
      Optional_Virtual_String'Write (S, V.version);
      JS.End_Object;
   end Write_RegularExpressionsClientCapabilities;

   procedure Read_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out GeneralClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "regularExpressions" then
               Optional_RegularExpressionsClientCapabilities'Read (S, V.regularExpressions);
            elsif Key = "markdown" then
               Optional_MarkdownClientCapabilities'Read (S, V.markdown);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_GeneralClientCapabilities;

   procedure Write_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : GeneralClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("regularExpressions");
      Optional_RegularExpressionsClientCapabilities'Write (S, V.regularExpressions);
      JS.Key ("markdown");
      Optional_MarkdownClientCapabilities'Write (S, V.markdown);
      JS.End_Object;
   end Write_GeneralClientCapabilities;

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workspace" then
               WorkspaceClientCapabilities'Read (S, V.workspace);
            elsif Key = "textDocument" then
               TextDocumentClientCapabilities'Read (S, V.textDocument);
            elsif Key = "window" then
               Optional_WindowClientCapabilities'Read (S, V.window);
            elsif Key = "general" then
               Optional_GeneralClientCapabilities'Read (S, V.general);
            elsif Key = "experimental" then
               Optional_LSP_Any'Read (S, V.experimental);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ClientCapabilities;

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Write (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Write (S, V.textDocument);
      JS.Key ("window");
      Optional_WindowClientCapabilities'Write (S, V.window);
      JS.Key ("general");
      Optional_GeneralClientCapabilities'Write (S, V.general);
      JS.Key ("experimental");
      Optional_LSP_Any'Write (S, V.experimental);
      JS.End_Object;
   end Write_ClientCapabilities;

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "name" then
               LSP.Types.Read_String (S, V.name);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceFolder;

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.End_Object;
   end Write_WorkspaceFolder;

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressCreateParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "token" then
               ProgressToken'Read (S, V.token);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkDoneProgressCreateParams;

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressCreateParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("token");
      ProgressToken'Write (S, V.token);
      JS.End_Object;
   end Write_WorkDoneProgressCreateParams;

   procedure Read_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Text_Progress_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Text_Progress_Params;

   procedure Write_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Text_Progress_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.End_Object;
   end Write_Text_Progress_Params;

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "name" then
               LSP.Types.Read_String (S, V.name);
            elsif Key = "version" then
               Optional_Virtual_String'Read (S, V.version);
            elsif Key = "log_filename" then
               Optional_Virtual_String'Read (S, V.log_filename);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ProgramInfo;

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.Key ("version");
      Optional_Virtual_String'Write (S, V.version);
      JS.Key ("log_filename");
      Optional_Virtual_String'Write (S, V.log_filename);
      JS.End_Object;
   end Write_ProgramInfo;

   procedure Read_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TraceValue)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "off" then
         V := off;
      elsif Text = "message" then
         V := messages_trace;
      elsif Text = "verbose" then
         V := verbose;
      else
         V := TraceValue'First;
      end if;
   end Read_TraceValue;

   procedure Write_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TraceValue)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : TraceValue)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : TraceValue)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when off =>
               return "off";
            when messages_trace =>
               return "message";
            when verbose =>
               return "verbose";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_TraceValue;

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "processId" then
               Optional_Number'Read (S, V.processId);
            elsif Key = "clientInfo" then
               Optional_ProgramInfo'Read (S, V.clientInfo);
            elsif Key = "locale" then
               Optional_Virtual_String'Read (S, V.locale);
            elsif Key = "rootPath" then
               Optional_Nullable_String'Read (S, V.rootPath);
            elsif Key = "rootUri" then
               Nullable_String'Read (S, V.rootUri);
            elsif Key = "capabilities" then
               ClientCapabilities'Read (S, V.capabilities);
            elsif Key = "trace" then
               Optional_TraceValue'Read (S, V.trace);
            elsif Key = "workspaceFolders" then
               Optional_WorkspaceFolder_Vector'Read (S, V.workspaceFolders);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeParams;

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("processId");
      Optional_Number'Write (S, V.processId);
      JS.Key ("clientInfo");
      Optional_ProgramInfo'Write (S, V.clientInfo);
      JS.Key ("locale");
      Optional_Virtual_String'Write (S, V.locale);
      JS.Key ("rootPath");
      Optional_Nullable_String'Write (S, V.rootPath);
      JS.Key ("rootUri");
      Nullable_String'Write (S, V.rootUri);
      JS.Key ("capabilities");
      ClientCapabilities'Write (S, V.capabilities);
      JS.Key ("trace");
      Optional_TraceValue'Write (S, V.trace);
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFolder_Vector'Write (S, V.workspaceFolders);
      JS.End_Object;
   end Write_InitializeParams;

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "triggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.triggerCharacters);
            elsif Key = "allCommitCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.allCommitCharacters);
            elsif Key = "resolveProvider" then
               LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionOptions;

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_Virtual_String_Vector'Write (S, V.allCommitCharacters);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CompletionOptions;

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "triggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.triggerCharacters);
            elsif Key = "retriggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.retriggerCharacters);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelpOptions;

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("triggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.retriggerCharacters);
      JS.End_Object;
   end Write_SignatureHelpOptions;

   procedure Read_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentRegistrationOptions;

   procedure Write_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.End_Object;
   end Write_TextDocumentRegistrationOptions;

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "id" then
               Optional_Virtual_String'Read (S, V.id);
            elsif Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TSW_RegistrationOptions;

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TSW_RegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("id");
      Optional_Virtual_String'Write (S, V.id);
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.End_Object;
   end Write_TSW_RegistrationOptions;

   procedure Read_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "label" then
               Optional_Virtual_String'Read (S, V.label);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentSymbolOptions;

   procedure Write_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("label");
      Optional_Virtual_String'Write (S, V.label);
      JS.End_Object;
   end Write_DocumentSymbolOptions;

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "codeActionKinds" then
               Optional_CodeActionKindSet'Read (S, V.codeActionKinds);
            elsif Key = "resolveProvider" then
               Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeActionOptions;

   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("codeActionKinds");
      Optional_CodeActionKindSet'Write (S, V.codeActionKinds);
      JS.Key ("resolveProvider");
      Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CodeActionOptions;

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "resolveProvider" then
               LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeLensOptions;

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CodeLensOptions;

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "firstTriggerCharacter" then
               LSP.Types.Read_String (S, V.firstTriggerCharacter);
            elsif Key = "moreTriggerCharacter" then
               Optional_Virtual_String_Vector'Read (S, V.moreTriggerCharacter);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentOnTypeFormattingOptions;

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("firstTriggerCharacter");
      LSP.Types.Write_String (S, V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_Virtual_String_Vector'Write (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingOptions;

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "prepareProvider" then
               LSP.Types.Optional_Boolean'Read (S, V.prepareProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameOptions;

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("prepareProvider");
      LSP.Types.Optional_Boolean'Write (S, V.prepareProvider);
      JS.End_Object;
   end Write_RenameOptions;

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "resolveProvider" then
               LSP.Types.Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentLinkOptions;

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("resolveProvider");
      LSP.Types.Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_DocumentLinkOptions;

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "commands" then
               LSP.Types.Read_String_Vector (S, V.commands);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExecuteCommandOptions;

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("commands");
      LSP.Types.Write_String_Vector (S, V.commands);
      JS.End_Object;
   end Write_ExecuteCommandOptions;

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "supported" then
               Optional_Boolean'Read (S, V.supported);
            elsif Key = "changeNotifications" then
               Optional_Boolean_Or_String'Read (S, V.changeNotifications);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceFoldersServerCapabilities;

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("supported");
      Optional_Boolean'Write (S, V.supported);
      JS.Key ("changeNotifications");
      Optional_Boolean_Or_String'Write (S, V.changeNotifications);
      JS.End_Object;
   end Write_WorkspaceFoldersServerCapabilities;

   procedure Read_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "file" then
         V := file;
      elsif Text = "folder" then
         V := folder;
      else
         V := FileOperationPatternKind'First;
      end if;
   end Read_FileOperationPatternKind;

   procedure Write_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : FileOperationPatternKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : FileOperationPatternKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when file =>
               return "file";
            when folder =>
               return "folder";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_FileOperationPatternKind;

   procedure Read_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "ignoreCase" then
               Optional_Boolean'Read (S, V.ignoreCase);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationPatternOptions;

   procedure Write_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("ignoreCase");
      Optional_Boolean'Write (S, V.ignoreCase);
      JS.End_Object;
   end Write_FileOperationPatternOptions;

   procedure Read_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPattern)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "glob" then
               LSP.Types.Read_String (S, V.glob);
            elsif Key = "matches" then
               Optional_FileOperationPatternKind'Read (S, V.matches);
            elsif Key = "options" then
               Optional_FileOperationPatternOptions'Read (S, V.options);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationPattern;

   procedure Write_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPattern)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("glob");
      LSP.Types.Write_String (S, V.glob);
      JS.Key ("matches");
      Optional_FileOperationPatternKind'Write (S, V.matches);
      JS.Key ("options");
      Optional_FileOperationPatternOptions'Write (S, V.options);
      JS.End_Object;
   end Write_FileOperationPattern;

   procedure Read_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationFilter)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "scheme" then
               Optional_Virtual_String'Read (S, V.scheme);
            elsif Key = "pattern" then
               FileOperationPattern'Read (S, V.pattern);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationFilter;

   procedure Write_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationFilter)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scheme");
      Optional_Virtual_String'Write (S, V.scheme);
      JS.Key ("pattern");
      FileOperationPattern'Write (S, V.pattern);
      JS.End_Object;
   end Write_FileOperationFilter;

   procedure Read_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "filters" then
               FileOperationFilter_Vector'Read (S, V.filters);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationRegistrationOptions;

   procedure Write_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("filters");
      FileOperationFilter_Vector'Write (S, V.filters);
      JS.End_Object;
   end Write_FileOperationRegistrationOptions;

   procedure Read_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "didCreate" then
               Optional_FileOperationRegistrationOptions'Read (S, V.didCreate);
            elsif Key = "willCreate" then
               Optional_FileOperationRegistrationOptions'Read (S, V.willCreate);
            elsif Key = "didRename" then
               Optional_FileOperationRegistrationOptions'Read (S, V.didRename);
            elsif Key = "willRename" then
               Optional_FileOperationRegistrationOptions'Read (S, V.willRename);
            elsif Key = "didDelete" then
               Optional_FileOperationRegistrationOptions'Read (S, V.didDelete);
            elsif Key = "willDelete" then
               Optional_FileOperationRegistrationOptions'Read (S, V.willDelete);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileOperationsServerCapabilities;

   procedure Write_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("didCreate");
      Optional_FileOperationRegistrationOptions'Write (S, V.didCreate);
      JS.Key ("willCreate");
      Optional_FileOperationRegistrationOptions'Write (S, V.willCreate);
      JS.Key ("didRename");
      Optional_FileOperationRegistrationOptions'Write (S, V.didRename);
      JS.Key ("willRename");
      Optional_FileOperationRegistrationOptions'Write (S, V.willRename);
      JS.Key ("didDelete");
      Optional_FileOperationRegistrationOptions'Write (S, V.didDelete);
      JS.Key ("willDelete");
      Optional_FileOperationRegistrationOptions'Write (S, V.willDelete);
      JS.End_Object;
   end Write_FileOperationsServerCapabilities;

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workspaceFolders" then
               Optional_WorkspaceFoldersServerCapabilities'Read (S, V.workspaceFolders);
            elsif Key = "fileOperations" then
               Optional_FileOperationsServerCapabilities'Read (S, V.fileOperations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_workspace_Options;

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFoldersServerCapabilities'Write (S, V.workspaceFolders);
      JS.Key ("fileOperations");
      Optional_FileOperationsServerCapabilities'Write (S, V.fileOperations);
      JS.End_Object;
   end Write_workspace_Options;

   procedure Read_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensLegend)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "tokenTypes" then
               LSP.Types.Read_String_Vector (S, V.tokenTypes);
            elsif Key = "tokenModifiers" then
               LSP.Types.Read_String_Vector (S, V.tokenModifiers);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensLegend;

   procedure Write_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensLegend)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("tokenTypes");
      LSP.Types.Write_String_Vector (S, V.tokenTypes);
      JS.Key ("tokenModifiers");
      LSP.Types.Write_String_Vector (S, V.tokenModifiers);
      JS.End_Object;
   end Write_SemanticTokensLegend;

   procedure Read_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            elsif Key = "legend" then
               SemanticTokensLegend'Read (S, V.legend);
            elsif Key = "range" then
               Optional_Boolean'Read (S, V.span);
            elsif Key = "full" then
               Optional_SemanticTokensFullCapabilities'Read (S, V.full);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensOptions;

   procedure Write_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.Key ("legend");
      SemanticTokensLegend'Write (S, V.legend);
      JS.Key ("range");
      Optional_Boolean'Write (S, V.span);
      JS.Key ("full");
      Optional_SemanticTokensFullCapabilities'Write (S, V.full);
      JS.End_Object;
   end Write_SemanticTokensOptions;

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocumentSync" then
               Optional_TextDocumentSyncOptions'Read (S, V.textDocumentSync);
            elsif Key = "completionProvider" then
               Optional_CompletionOptions'Read (S, V.completionProvider);
            elsif Key = "hoverProvider" then
               HoverOptions'Read (S, V.hoverProvider);
            elsif Key = "signatureHelpProvider" then
               Optional_SignatureHelpOptions'Read (S, V.signatureHelpProvider);
            elsif Key = "declarationProvider" then
               DeclarationOptions'Read (S, V.declarationProvider);
            elsif Key = "definitionProvider" then
               DefinitionOptions'Read (S, V.definitionProvider);
            elsif Key = "typeDefinitionProvider" then
               TypeDefinitionOptions'Read (S, V.typeDefinitionProvider);
            elsif Key = "implementationProvider" then
               ImplementationOptions'Read (S, V.implementationProvider);
            elsif Key = "referencesProvider" then
               ReferenceOptions'Read (S, V.referencesProvider);
            elsif Key = "documentHighlightProvider" then
               DocumentHighlightOptions'Read (S, V.documentHighlightProvider);
            elsif Key = "documentSymbolProvider" then
               Optional_DocumentSymbolOptions'Read (S, V.documentSymbolProvider);
            elsif Key = "codeActionProvider" then
               Optional_CodeActionOptions'Read (S, V.codeActionProvider);
            elsif Key = "codeLensProvider" then
               Optional_CodeLensOptions'Read (S, V.codeLensProvider);
            elsif Key = "documentLinkProvider" then
               Optional_DocumentLinkOptions'Read (S, V.documentLinkProvider);
            elsif Key = "colorProvider" then
               DocumentColorOptions'Read (S, V.colorProvider);
            elsif Key = "documentFormattingProvider" then
               DocumentFormattingOptions'Read (S, V.documentFormattingProvider);
            elsif Key = "documentRangeFormattingProvider" then
               DocumentRangeFormattingOptions'Read (S, V.documentRangeFormattingProvider);
            elsif Key = "documentOnTypeFormattingProvider" then
               Optional_DocumentOnTypeFormattingOptions'Read (S, V.documentOnTypeFormattingProvider);
            elsif Key = "renameProvider" then
               Optional_RenameOptions'Read (S, V.renameProvider);
            elsif Key = "foldingRangeProvider" then
               FoldingRangeOptions'Read (S, V.foldingRangeProvider);
            elsif Key = "executeCommandProvider" then
               Optional_ExecuteCommandOptions'Read (S, V.executeCommandProvider);
            elsif Key = "selectionRangeProvider" then
               SelectionRangeOptions'Read (S, V.selectionRangeProvider);
            elsif Key = "linkedEditingRangeProvider" then
               LinkedEditingRangeOptions'Read (S, V.linkedEditingRangeProvider);
            elsif Key = "semanticTokensProvider" then
               Optional_SemanticTokensOptions'Read (S, V.semanticTokensProvider);
            elsif Key = "monikerProvider" then
               MonikerOptions'Read (S, V.monikerProvider);
            elsif Key = "workspaceSymbolProvider" then
               WorkspaceSymbolOptions'Read (S, V.workspaceSymbolProvider);
            elsif Key = "workspace" then
               Optional_workspace_Options'Read (S, V.workspace);
            elsif Key = "callHierarchyProvider" then
               CallHierarchyOptions'Read (S, V.callHierarchyProvider);
            elsif Key = "alsShowDepsProvider" then
               Optional_Boolean'Read (S, V.alsShowDepsProvider);
            elsif Key = "alsReferenceKinds" then
               Optional_AlsReferenceKind_Set'Read (S, V.alsReferenceKinds);
            elsif Key = "alsCheckSyntaxProvider" then
               Optional_Boolean'Read (S, V.alsCheckSyntaxProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ServerCapabilities;

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Write (S, V.textDocumentSync);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Write (S, V.completionProvider);
      JS.Key ("hoverProvider");
      HoverOptions'Write (S, V.hoverProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Write (S, V.signatureHelpProvider);
      JS.Key ("declarationProvider");
      DeclarationOptions'Write (S, V.declarationProvider);
      JS.Key ("definitionProvider");
      DefinitionOptions'Write (S, V.definitionProvider);
      JS.Key ("typeDefinitionProvider");
      TypeDefinitionOptions'Write (S, V.typeDefinitionProvider);
      JS.Key ("implementationProvider");
      ImplementationOptions'Write (S, V.implementationProvider);
      JS.Key ("referencesProvider");
      ReferenceOptions'Write (S, V.referencesProvider);
      JS.Key ("documentHighlightProvider");
      DocumentHighlightOptions'Write (S, V.documentHighlightProvider);
      JS.Key ("documentSymbolProvider");
      Optional_DocumentSymbolOptions'Write (S, V.documentSymbolProvider);
      JS.Key ("codeActionProvider");
      Optional_CodeActionOptions'Write (S, V.codeActionProvider);
      JS.Key ("codeLensProvider");
      Optional_CodeLensOptions'Write (S, V.codeLensProvider);
      JS.Key ("documentLinkProvider");
      Optional_DocumentLinkOptions'Write (S, V.documentLinkProvider);
      JS.Key ("colorProvider");
      DocumentColorOptions'Write (S, V.colorProvider);
      JS.Key ("documentFormattingProvider");
      DocumentFormattingOptions'Write (S, V.documentFormattingProvider);
      JS.Key ("documentRangeFormattingProvider");
      DocumentRangeFormattingOptions'Write (S, V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Write (S, V.documentOnTypeFormattingProvider);
      JS.Key ("renameProvider");
      Optional_RenameOptions'Write (S, V.renameProvider);
      JS.Key ("foldingRangeProvider");
      FoldingRangeOptions'Write (S, V.foldingRangeProvider);
      JS.Key ("executeCommandProvider");
      Optional_ExecuteCommandOptions'Write (S, V.executeCommandProvider);
      JS.Key ("selectionRangeProvider");
      SelectionRangeOptions'Write (S, V.selectionRangeProvider);
      JS.Key ("linkedEditingRangeProvider");
      LinkedEditingRangeOptions'Write (S, V.linkedEditingRangeProvider);
      JS.Key ("semanticTokensProvider");
      Optional_SemanticTokensOptions'Write (S, V.semanticTokensProvider);
      JS.Key ("monikerProvider");
      MonikerOptions'Write (S, V.monikerProvider);
      JS.Key ("workspaceSymbolProvider");
      WorkspaceSymbolOptions'Write (S, V.workspaceSymbolProvider);
      JS.Key ("workspace");
      Optional_workspace_Options'Write (S, V.workspace);
      JS.Key ("callHierarchyProvider");
      CallHierarchyOptions'Write (S, V.callHierarchyProvider);
      JS.Key ("alsShowDepsProvider");
      Optional_Boolean'Write (S, V.alsShowDepsProvider);
      JS.Key ("alsReferenceKinds");
      Optional_AlsReferenceKind_Set'Write (S, V.alsReferenceKinds);
      JS.Key ("alsCheckSyntaxProvider");
      Optional_Boolean'Write (S, V.alsCheckSyntaxProvider);
      JS.End_Object;
   end Write_ServerCapabilities;

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "capabilities" then
               ServerCapabilities'Read (S, V.capabilities);
            elsif Key = "serverInfo" then
               Optional_ProgramInfo'Read (S, V.serverInfo);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializeResult;

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Write (S, V.capabilities);
      JS.Key ("serverInfo");
      Optional_ProgramInfo'Write (S, V.serverInfo);
      JS.End_Object;
   end Write_InitializeResult;

   procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "" then
               null;
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InitializedParams;

   procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Write_InitializedParams;

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := MessageType'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_MessageType;

   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((MessageType'Pos (V)) + 1);
   end Write_MessageType;

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "type" then
               MessageType'Read (S, V.a_type);
            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowMessageParams;

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.a_type);
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.End_Object;
   end Write_ShowMessageParams;

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "type" then
               MessageType'Read (S, V.a_type);
            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);
            elsif Key = "actions" then
               MessageActionItem_Vector'Read (S, V.actions);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowMessageRequestParams;

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.a_type);
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.Key ("actions");
      MessageActionItem_Vector'Write (S, V.actions);
      JS.End_Object;
   end Write_ShowMessageRequestParams;

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "type" then
               MessageType'Read (S, V.a_type);
            elsif Key = "message" then
               LSP.Types.Read_String (S, V.message);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LogMessageParams;

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogMessageParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("type");
      MessageType'Write (S, V.a_type);
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.End_Object;
   end Write_LogMessageParams;

   procedure Read_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentChangeRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "syncKind" then
               TextDocumentSyncKind'Read (S, V.syncKind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentChangeRegistrationOptions;

   procedure Write_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentChangeRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("syncKind");
      TextDocumentSyncKind'Write (S, V.syncKind);
      JS.End_Object;
   end Write_TextDocumentChangeRegistrationOptions;

   procedure Read_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "includeText" then
               Optional_Boolean'Read (S, V.includeText);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentSaveRegistrationOptions;

   procedure Write_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("includeText");
      Optional_Boolean'Write (S, V.includeText);
      JS.End_Object;
   end Write_TextDocumentSaveRegistrationOptions;

   procedure Read_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "triggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.triggerCharacters);
            elsif Key = "allCommitCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.allCommitCharacters);
            elsif Key = "resolveProvider" then
               Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionRegistrationOptions;

   procedure Write_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("triggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("allCommitCharacters");
      Optional_Virtual_String_Vector'Write (S, V.allCommitCharacters);
      JS.Key ("resolveProvider");
      Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CompletionRegistrationOptions;

   procedure Read_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "triggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.triggerCharacters);
            elsif Key = "retriggerCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.retriggerCharacters);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelpRegistrationOptions;

   procedure Write_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("triggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.triggerCharacters);
      JS.Key ("retriggerCharacters");
      Optional_Virtual_String_Vector'Write (S, V.retriggerCharacters);
      JS.End_Object;
   end Write_SignatureHelpRegistrationOptions;

   procedure Read_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "resolveProvider" then
               Optional_Boolean'Read (S, V.resolveProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeLensRegistrationOptions;

   procedure Write_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("resolveProvider");
      Optional_Boolean'Write (S, V.resolveProvider);
      JS.End_Object;
   end Write_CodeLensRegistrationOptions;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "firstTriggerCharacter" then
               LSP.Types.Read_String (S, V.firstTriggerCharacter);
            elsif Key = "moreTriggerCharacter" then
               Optional_Virtual_String_Vector'Read (S, V.moreTriggerCharacter);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentOnTypeFormattingRegistrationOptions;

   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("firstTriggerCharacter");
      LSP.Types.Write_String (S, V.firstTriggerCharacter);
      JS.Key ("moreTriggerCharacter");
      Optional_Virtual_String_Vector'Write (S, V.moreTriggerCharacter);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingRegistrationOptions;

   procedure Read_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "commands" then
               LSP.Types.Read_String_Vector (S, V.commands);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ExecuteCommandRegistrationOptions;

   procedure Write_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("commands");
      LSP.Types.Write_String_Vector (S, V.commands);
      JS.End_Object;
   end Write_ExecuteCommandRegistrationOptions;

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "globPattern" then
               LSP.Types.Read_String (S, V.globPattern);
            elsif Key = "kind" then
               WatchKind_Set'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileSystemWatcher;

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("globPattern");
      LSP.Types.Write_String (S, V.globPattern);
      JS.Key ("kind");
      WatchKind_Set'Write (S, V.kind);
      JS.End_Object;
   end Write_FileSystemWatcher;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "watchers" then
               FileSystemWatcher_Vector'Read (S, V.watchers);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidChangeWatchedFilesRegistrationOptions;

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("watchers");
      FileSystemWatcher_Vector'Write (S, V.watchers);
      JS.End_Object;
   end Write_DidChangeWatchedFilesRegistrationOptions;

   procedure Read_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "This" then
               CodeActionOptions'Read (S, V.This);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeActionRegistrationOptions;

   procedure Write_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("This");
      CodeActionOptions'Write (S, V.This);
      JS.End_Object;
   end Write_CodeActionRegistrationOptions;

   procedure Read_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "documentSelector" then
               LSP.Messages.DocumentSelector'Read (S, V.documentSelector);
            elsif Key = "prepareProvider" then
               Optional_Boolean'Read (S, V.prepareProvider);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameRegistrationOptions;

   procedure Write_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentSelector");
      LSP.Messages.DocumentSelector'Write (S, V.documentSelector);
      JS.Key ("prepareProvider");
      Optional_Boolean'Write (S, V.prepareProvider);
      JS.End_Object;
   end Write_RenameRegistrationOptions;

   procedure Read_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Registration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "id" then
               LSP.Types.Read_String (S, V.id);
            elsif Key = "method" then
               LSP.Types.Read_String (S, V.method);
            elsif Key = "registerOptions" then
               Registration_Option'Read (S, V.registerOptions);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Registration;

   procedure Write_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Registration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("id");
      LSP.Types.Write_String (S, V.id);
      JS.Key ("method");
      LSP.Types.Write_String (S, V.method);
      JS.Key ("registerOptions");
      Registration_Option'Write (S, V.registerOptions);
      JS.End_Object;
   end Write_Registration;

   procedure Read_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegistrationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "registrations" then
               Registration_Vector'Read (S, V.registrations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RegistrationParams;

   procedure Write_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegistrationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("registrations");
      Registration_Vector'Write (S, V.registrations);
      JS.End_Object;
   end Write_RegistrationParams;

   procedure Read_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Unregistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "id" then
               LSP.Types.Read_String (S, V.id);
            elsif Key = "method" then
               LSP.Types.Read_String (S, V.method);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Unregistration;

   procedure Write_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Unregistration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("id");
      LSP.Types.Write_String (S, V.id);
      JS.Key ("method");
      LSP.Types.Write_String (S, V.method);
      JS.End_Object;
   end Write_Unregistration;

   procedure Read_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UnregistrationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "unregisterations" then
               Unregistration_Vector'Read (S, V.unregisterations);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_UnregistrationParams;

   procedure Write_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UnregistrationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("unregisterations");
      Unregistration_Vector'Write (S, V.unregisterations);
      JS.End_Object;
   end Write_UnregistrationParams;

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "settings" then
               LSP.Types.LSP_Any'Read (S, V.settings);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidChangeConfigurationParams;

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      LSP.Types.LSP_Any'Write (S, V.settings);
      JS.End_Object;
   end Write_DidChangeConfigurationParams;

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentItem'Read (S, V.textDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidOpenTextDocumentParams;

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidOpenTextDocumentParams;

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               Optional_Span'Read (S, V.span);
            elsif Key = "rangeLength" then
               LSP.Types.Optional_Number'Read (S, V.rangeLength);
            elsif Key = "text" then
               LSP.Types.Read_String (S, V.text);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextDocumentContentChangeEvent;

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Write (S, V.span);
      JS.Key ("rangeLength");
      LSP.Types.Optional_Number'Write (S, V.rangeLength);
      JS.Key ("text");
      LSP.Types.Write_String (S, V.text);
      JS.End_Object;
   end Write_TextDocumentContentChangeEvent;

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               VersionedTextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "contentChanges" then
               TextDocumentContentChangeEvent_Vector'Read (S, V.contentChanges);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidChangeTextDocumentParams;

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Write (S, V.contentChanges);
      JS.End_Object;
   end Write_DidChangeTextDocumentParams;

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveReason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := TextDocumentSaveReason'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_TextDocumentSaveReason;

   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveReason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((TextDocumentSaveReason'Pos (V)) + 1);
   end Write_TextDocumentSaveReason;

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "text" then
               Optional_Virtual_String'Read (S, V.text);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidSaveTextDocumentParams;

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("text");
      Optional_Virtual_String'Write (S, V.text);
      JS.End_Object;
   end Write_DidSaveTextDocumentParams;

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidCloseTextDocumentParams;

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidCloseTextDocumentParams;

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileChangeType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := FileChangeType'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_FileChangeType;

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileChangeType)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((FileChangeType'Pos (V)) + 1);
   end Write_FileChangeType;

   procedure Read_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "type" then
               FileChangeType'Read (S, V.a_type);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileEvent;

   procedure Write_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("type");
      FileChangeType'Write (S, V.a_type);
      JS.End_Object;
   end Write_FileEvent;

   procedure Read_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "changes" then
               FileEvent_Vector'Read (S, V.changes);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidChangeWatchedFilesParams;

   procedure Write_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("changes");
      FileEvent_Vector'Write (S, V.changes);
      JS.End_Object;
   end Write_DidChangeWatchedFilesParams;

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "version" then
               Optional_Number'Read (S, V.version);
            elsif Key = "diagnostics" then
               Diagnostic_Vector'Read (S, V.diagnostics);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_PublishDiagnosticsParams;

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("version");
      Optional_Number'Write (S, V.version);
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.End_Object;
   end Write_PublishDiagnosticsParams;

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := InsertTextFormat'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_InsertTextFormat;

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((InsertTextFormat'Pos (V)) + 1);
   end Write_InsertTextFormat;

   procedure Read_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertReplaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "newText" then
               LSP.Types.Read_String (S, V.newText);
            elsif Key = "insert" then
               Span'Read (S, V.insert);
            elsif Key = "replace" then
               Span'Read (S, V.replace);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_InsertReplaceEdit;

   procedure Write_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertReplaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("newText");
      LSP.Types.Write_String (S, V.newText);
      JS.Key ("insert");
      Span'Write (S, V.insert);
      JS.Key ("replace");
      Span'Write (S, V.replace);
      JS.End_Object;
   end Write_InsertReplaceEdit;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               LSP.Types.Read_String (S, V.label);
            elsif Key = "kind" then
               Optional_CompletionItemKind'Read (S, V.kind);
            elsif Key = "tags" then
               Optional_CompletionItemTagSet'Read (S, V.tags);
            elsif Key = "detail" then
               Optional_Virtual_String'Read (S, V.detail);
            elsif Key = "documentation" then
               Optional_String_Or_MarkupContent'Read (S, V.documentation);
            elsif Key = "deprecated" then
               Optional_Boolean'Read (S, V.deprecated);
            elsif Key = "preselect" then
               Optional_Boolean'Read (S, V.preselect);
            elsif Key = "sortText" then
               Optional_Virtual_String'Read (S, V.sortText);
            elsif Key = "filterText" then
               Optional_Virtual_String'Read (S, V.filterText);
            elsif Key = "insertText" then
               Optional_Virtual_String'Read (S, V.insertText);
            elsif Key = "insertTextFormat" then
               Optional_InsertTextFormat'Read (S, V.insertTextFormat);
            elsif Key = "insertTextMode" then
               Optional_InsertTextMode'Read (S, V.insertTextMode);
            elsif Key = "textEdit" then
               Optional_TextEdit_Or_InsertReplaceEdit'Read (S, V.textEdit);
            elsif Key = "additionalTextEdits" then
               TextEdit_Vector'Read (S, V.additionalTextEdits);
            elsif Key = "commitCharacters" then
               Optional_Virtual_String_Vector'Read (S, V.commitCharacters);
            elsif Key = "command" then
               Optional_Command'Read (S, V.command);
            elsif Key = "data" then
               Optional_Location'Read (S, V.data);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionItem;

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write_String (S, V.label);
      JS.Key ("kind");
      Optional_CompletionItemKind'Write (S, V.kind);
      JS.Key ("tags");
      Optional_CompletionItemTagSet'Write (S, V.tags);
      JS.Key ("detail");
      Optional_Virtual_String'Write (S, V.detail);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.Key ("deprecated");
      Optional_Boolean'Write (S, V.deprecated);
      JS.Key ("preselect");
      Optional_Boolean'Write (S, V.preselect);
      JS.Key ("sortText");
      Optional_Virtual_String'Write (S, V.sortText);
      JS.Key ("filterText");
      Optional_Virtual_String'Write (S, V.filterText);
      JS.Key ("insertText");
      Optional_Virtual_String'Write (S, V.insertText);
      JS.Key ("insertTextFormat");
      Optional_InsertTextFormat'Write (S, V.insertTextFormat);
      JS.Key ("insertTextMode");
      Optional_InsertTextMode'Write (S, V.insertTextMode);
      JS.Key ("textEdit");
      Optional_TextEdit_Or_InsertReplaceEdit'Write (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Write (S, V.additionalTextEdits);
      JS.Key ("commitCharacters");
      Optional_Virtual_String_Vector'Write (S, V.commitCharacters);
      JS.Key ("command");
      Optional_Command'Write (S, V.command);
      JS.Key ("data");
      Optional_Location'Write (S, V.data);
      JS.End_Object;
   end Write_CompletionItem;

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "isIncomplete" then
               LSP.Types.Read_Boolean (JS, V.isIncomplete);
            elsif Key = "items" then
               CompletionItem_Vector'Read (S, V.items);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionList;

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, "isIncomplete", V.isIncomplete);
      JS.Key ("items");
      CompletionItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_CompletionList;

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "contents" then
               MarkupContent_Or_MarkedString_Vector'Read (S, V.contents);
            elsif Key = "range" then
               Optional_Span'Read (S, V.Span);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Hover;

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");
      MarkupContent_Or_MarkedString_Vector'Write (S, V.contents);
      JS.Key ("range");
      Optional_Span'Write (S, V.Span);
      JS.End_Object;
   end Write_Hover;

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               Parameter_Label'Read (S, V.label);
            elsif Key = "documentation" then
               Optional_String_Or_MarkupContent'Read (S, V.documentation);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ParameterInformation;

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Parameter_Label'Write (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.End_Object;
   end Write_ParameterInformation;

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               LSP.Types.Read_String (S, V.label);
            elsif Key = "documentation" then
               Optional_String_Or_MarkupContent'Read (S, V.documentation);
            elsif Key = "parameters" then
               ParameterInformation_Vector'Read (S, V.parameters);
            elsif Key = "activeParameter" then
               Optional_uinteger'Read (S, V.activeParameter);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureInformation;

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write_String (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Write (S, V.parameters);
      JS.Key ("activeParameter");
      Optional_uinteger'Write (S, V.activeParameter);
      JS.End_Object;
   end Write_SignatureInformation;

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "signatures" then
               SignatureInformation_Vector'Read (S, V.signatures);
            elsif Key = "activeSignature" then
               Optional_Number'Read (S, V.activeSignature);
            elsif Key = "activeParameter" then
               Optional_Number'Read (S, V.activeParameter);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelp;

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("signatures");
      SignatureInformation_Vector'Write (S, V.signatures);
      JS.Key ("activeSignature");
      Optional_Number'Write (S, V.activeSignature);
      JS.Key ("activeParameter");
      Optional_Number'Write (S, V.activeParameter);
      JS.End_Object;
   end Write_SignatureHelp;

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "includeDeclaration" then
               LSP.Types.Read_Boolean (JS, V.includeDeclaration);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReferenceContext;

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, "includeDeclaration", V.includeDeclaration);
      JS.End_Object;
   end Write_ReferenceContext;

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "context" then
               ReferenceContext'Read (S, V.context);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ReferenceParams;

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("context");
      ReferenceContext'Write (S, V.context);
      JS.End_Object;
   end Write_ReferenceParams;

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := DocumentHighlightKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_DocumentHighlightKind;

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlightKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((DocumentHighlightKind'Pos (V)) + 1);
   end Write_DocumentHighlightKind;

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "kind" then
               Optional_DocumentHighlightKind'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentHighlight;

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("kind");
      Optional_DocumentHighlightKind'Write (S, V.kind);
      JS.End_Object;
   end Write_DocumentHighlight;

   procedure Read_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Search_Kind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := Search_Kind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_Search_Kind;

   procedure Write_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Search_Kind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((Search_Kind'Pos (V)) + 1);
   end Write_Search_Kind;

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "query" then
               LSP.Types.Read_String (S, V.query);
            elsif Key = "case_sensitive" then
               Optional_Boolean'Read (S, V.case_sensitive);
            elsif Key = "whole_word" then
               Optional_Boolean'Read (S, V.whole_word);
            elsif Key = "negate" then
               Optional_Boolean'Read (S, V.negate);
            elsif Key = "kind" then
               Optional_Search_Kind'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentSymbolParams;

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("query");
      LSP.Types.Write_String (S, V.query);
      JS.Key ("case_sensitive");
      Optional_Boolean'Write (S, V.case_sensitive);
      JS.Key ("whole_word");
      Optional_Boolean'Write (S, V.whole_word);
      JS.Key ("negate");
      Optional_Boolean'Write (S, V.negate);
      JS.Key ("kind");
      Optional_Search_Kind'Write (S, V.kind);
      JS.End_Object;
   end Write_DocumentSymbolParams;

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "name" then
               LSP.Types.Read_String (S, V.name);
            elsif Key = "kind" then
               SymbolKind'Read (S, V.kind);
            elsif Key = "alsIsAdaProcedure" then
               Optional_Boolean'Read (S, V.alsIsAdaProcedure);
            elsif Key = "tags" then
               SymbolTagSet'Read (S, V.tags);
            elsif Key = "deprecated" then
               Optional_Boolean'Read (S, V.deprecated);
            elsif Key = "location" then
               LSP.Messages.Location'Read (S, V.location);
            elsif Key = "containerName" then
               Optional_Virtual_String'Read (S, V.containerName);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SymbolInformation;

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.Key ("kind");
      SymbolKind'Write (S, V.kind);
      JS.Key ("alsIsAdaProcedure");
      Optional_Boolean'Write (S, V.alsIsAdaProcedure);
      JS.Key ("tags");
      SymbolTagSet'Write (S, V.tags);
      JS.Key ("deprecated");
      Optional_Boolean'Write (S, V.deprecated);
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("containerName");
      Optional_Virtual_String'Write (S, V.containerName);
      JS.End_Object;
   end Write_SymbolInformation;

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "query" then
               LSP.Types.Read_String (S, V.query);
            elsif Key = "case_sensitive" then
               Optional_Boolean'Read (S, V.case_sensitive);
            elsif Key = "whole_word" then
               Optional_Boolean'Read (S, V.whole_word);
            elsif Key = "negate" then
               Optional_Boolean'Read (S, V.negate);
            elsif Key = "kind" then
               Optional_Search_Kind'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceSymbolParams;

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("query");
      LSP.Types.Write_String (S, V.query);
      JS.Key ("case_sensitive");
      Optional_Boolean'Write (S, V.case_sensitive);
      JS.Key ("whole_word");
      Optional_Boolean'Write (S, V.whole_word);
      JS.Key ("negate");
      Optional_Boolean'Write (S, V.negate);
      JS.Key ("kind");
      Optional_Search_Kind'Write (S, V.kind);
      JS.End_Object;
   end Write_WorkspaceSymbolParams;

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "diagnostics" then
               Diagnostic_Vector'Read (S, V.diagnostics);
            elsif Key = "only" then
               Optional_CodeActionKindSet'Read (S, V.only);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeActionContext;

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.Key ("only");
      Optional_CodeActionKindSet'Write (S, V.only);
      JS.End_Object;
   end Write_CodeActionContext;

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "context" then
               CodeActionContext'Read (S, V.context);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CodeActionParams;

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("context");
      CodeActionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CodeActionParams;

   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "tabSize" then
               LSP_Number'Read (S, V.tabSize);
            elsif Key = "insertSpaces" then
               LSP.Types.Read_Boolean (JS, V.insertSpaces);
            elsif Key = "trimTrailingWhitespace" then
               Optional_Boolean'Read (S, V.trimTrailingWhitespace);
            elsif Key = "insertFinalNewline" then
               Optional_Boolean'Read (S, V.insertFinalNewline);
            elsif Key = "trimFinalNewlines" then
               Optional_Boolean'Read (S, V.trimFinalNewlines);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FormattingOptions;

   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FormattingOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("tabSize");
      LSP_Number'Write (S, V.tabSize);
      Write_Boolean (JS, "insertSpaces", V.insertSpaces);
      JS.Key ("trimTrailingWhitespace");
      Optional_Boolean'Write (S, V.trimTrailingWhitespace);
      JS.Key ("insertFinalNewline");
      Optional_Boolean'Write (S, V.insertFinalNewline);
      JS.Key ("trimFinalNewlines");
      Optional_Boolean'Write (S, V.trimFinalNewlines);
      JS.End_Object;
   end Write_FormattingOptions;

   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "options" then
               FormattingOptions'Read (S, V.options);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentFormattingParams;

   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentFormattingParams;

   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "options" then
               FormattingOptions'Read (S, V.options);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentRangeFormattingParams;

   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentRangeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentRangeFormattingParams;

   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "ch" then
               LSP.Types.Read_String (S, V.ch);
            elsif Key = "options" then
               FormattingOptions'Read (S, V.options);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentOnTypeFormattingParams;

   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("ch");
      LSP.Types.Write_String (S, V.ch);
      JS.Key ("options");
      FormattingOptions'Write (S, V.options);
      JS.End_Object;
   end Write_DocumentOnTypeFormattingParams;

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "newName" then
               LSP.Types.Read_String (S, V.newName);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameParams;

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("newName");
      LSP.Types.Write_String (S, V.newName);
      JS.End_Object;
   end Write_RenameParams;

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               Optional_Virtual_String'Read (S, V.label);
            elsif Key = "edit" then
               WorkspaceEdit'Read (S, V.edit);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ApplyWorkspaceEditParams;

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Optional_Virtual_String'Write (S, V.label);
      JS.Key ("edit");
      WorkspaceEdit'Write (S, V.edit);
      JS.End_Object;
   end Write_ApplyWorkspaceEditParams;

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "applied" then
               LSP.Types.Read_Boolean (JS, V.applied);
            elsif Key = "failureReason" then
               Optional_Virtual_String'Read (S, V.failureReason);
            elsif Key = "failedChange" then
               Optional_uinteger'Read (S, V.failedChange);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ApplyWorkspaceEditResult;

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, "applied", V.applied);
      JS.Key ("failureReason");
      Optional_Virtual_String'Write (S, V.failureReason);
      JS.Key ("failedChange");
      Optional_uinteger'Write (S, V.failedChange);
      JS.End_Object;
   end Write_ApplyWorkspaceEditResult;

   procedure Read_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressBegin)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               LSP.Types.Read_String (S, V.kind);
            elsif Key = "title" then
               LSP.Types.Read_String (S, V.title);
            elsif Key = "cancellable" then
               Optional_Boolean'Read (S, V.cancellable);
            elsif Key = "message" then
               Optional_Virtual_String'Read (S, V.message);
            elsif Key = "percentage" then
               Optional_Number'Read (S, V.percentage);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkDoneProgressBegin;

   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressBegin)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write_String (S, V.kind);
      JS.Key ("title");
      LSP.Types.Write_String (S, V.title);
      JS.Key ("cancellable");
      Optional_Boolean'Write (S, V.cancellable);
      JS.Key ("message");
      Optional_Virtual_String'Write (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Write (S, V.percentage);
      JS.End_Object;
   end Write_WorkDoneProgressBegin;

   procedure Read_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressReport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               LSP.Types.Read_String (S, V.kind);
            elsif Key = "cancellable" then
               Optional_Boolean'Read (S, V.cancellable);
            elsif Key = "message" then
               Optional_Virtual_String'Read (S, V.message);
            elsif Key = "percentage" then
               Optional_Number'Read (S, V.percentage);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkDoneProgressReport;

   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressReport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write_String (S, V.kind);
      JS.Key ("cancellable");
      Optional_Boolean'Write (S, V.cancellable);
      JS.Key ("message");
      Optional_Virtual_String'Write (S, V.message);
      JS.Key ("percentage");
      Optional_Number'Write (S, V.percentage);
      JS.End_Object;
   end Write_WorkDoneProgressReport;

   procedure Read_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressEnd)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "kind" then
               LSP.Types.Read_String (S, V.kind);
            elsif Key = "message" then
               Optional_Virtual_String'Read (S, V.message);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkDoneProgressEnd;

   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressEnd)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      LSP.Types.Write_String (S, V.kind);
      JS.Key ("message");
      Optional_Virtual_String'Write (S, V.message);
      JS.End_Object;
   end Write_WorkDoneProgressEnd;

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "added" then
               WorkspaceFolder_Vector'Read (S, V.added);
            elsif Key = "removed" then
               WorkspaceFolder_Vector'Read (S, V.removed);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceFoldersChangeEvent;

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("added");
      WorkspaceFolder_Vector'Write (S, V.added);
      JS.Key ("removed");
      WorkspaceFolder_Vector'Write (S, V.removed);
      JS.End_Object;
   end Write_WorkspaceFoldersChangeEvent;

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "event" then
               WorkspaceFoldersChangeEvent'Read (S, V.event);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DidChangeWorkspaceFoldersParams;

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("event");
      WorkspaceFoldersChangeEvent'Write (S, V.event);
      JS.End_Object;
   end Write_DidChangeWorkspaceFoldersParams;

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "scopeUri" then
               Optional_Virtual_String'Read (S, V.scopeUri);
            elsif Key = "section" then
               Optional_Virtual_String'Read (S, V.section);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationItem;

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scopeUri");
      Optional_Virtual_String'Write (S, V.scopeUri);
      JS.Key ("section");
      Optional_Virtual_String'Write (S, V.section);
      JS.End_Object;
   end Write_ConfigurationItem;

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "items" then
               ConfigurationItem_Vector'Read (S, V.items);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ConfigurationParams;

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("items");
      ConfigurationItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_ConfigurationParams;

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := CompletionTriggerKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_CompletionTriggerKind;

   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((CompletionTriggerKind'Pos (V)) + 1);
   end Write_CompletionTriggerKind;

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "triggerKind" then
               CompletionTriggerKind'Read (S, V.triggerKind);
            elsif Key = "triggerCharacter" then
               Optional_Virtual_String'Read (S, V.triggerCharacter);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionContext;

   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerKind");
      CompletionTriggerKind'Write (S, V.triggerKind);
      JS.Key ("triggerCharacter");
      Optional_Virtual_String'Write (S, V.triggerCharacter);
      JS.End_Object;
   end Write_CompletionContext;

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "context" then
               Optional_CompletionContext'Read (S, V.context);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CompletionParams;

   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("context");
      Optional_CompletionContext'Write (S, V.context);
      JS.End_Object;
   end Write_CompletionParams;

   procedure Read_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Disable_Reason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "reason" then
               LSP.Types.Read_String (S, V.reason);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Disable_Reason;

   procedure Write_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Disable_Reason)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("reason");
      LSP.Types.Write_String (S, V.reason);
      JS.End_Object;
   end Write_Disable_Reason;

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "red" then
               LSP_Number'Read (S, V.red);
            elsif Key = "green" then
               LSP_Number'Read (S, V.green);
            elsif Key = "blue" then
               LSP_Number'Read (S, V.blue);
            elsif Key = "alpha" then
               LSP_Number'Read (S, V.alpha);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RGBA_Color;

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("red");
      LSP_Number'Write (S, V.red);
      JS.Key ("green");
      LSP_Number'Write (S, V.green);
      JS.Key ("blue");
      LSP_Number'Write (S, V.blue);
      JS.Key ("alpha");
      LSP_Number'Write (S, V.alpha);
      JS.End_Object;
   end Write_RGBA_Color;

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "color" then
               RGBA_Color'Read (S, V.color);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ColorInformation;

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.End_Object;
   end Write_ColorInformation;

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "color" then
               RGBA_Color'Read (S, V.color);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ColorPresentationParams;

   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_ColorPresentationParams;

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "label" then
               LSP.Types.Read_String (S, V.label);
            elsif Key = "textEdit" then
               Optional_TextEdit'Read (S, V.textEdit);
            elsif Key = "additionalTextEdits" then
               TextEdit_Vector'Read (S, V.additionalTextEdits);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ColorPresentation;

   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write_String (S, V.label);
      JS.Key ("textEdit");
      Optional_TextEdit'Write (S, V.textEdit);
      JS.Key ("additionalTextEdits");
      TextEdit_Vector'Write (S, V.additionalTextEdits);
      JS.End_Object;
   end Write_ColorPresentation;

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FoldingRangeParams;

   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_FoldingRangeParams;

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "startLine" then
               Line_Number'Read (S, V.startLine);
            elsif Key = "startCharacter" then
               Optional_Number'Read (S, V.startCharacter);
            elsif Key = "endLine" then
               Line_Number'Read (S, V.endLine);
            elsif Key = "endCharacter" then
               Optional_Number'Read (S, V.endCharacter);
            elsif Key = "kind" then
               Optional_Virtual_String'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FoldingRange;

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("startLine");
      Line_Number'Write (S, V.startLine);
      JS.Key ("startCharacter");
      Optional_Number'Write (S, V.startCharacter);
      JS.Key ("endLine");
      Line_Number'Write (S, V.endLine);
      JS.Key ("endCharacter");
      Optional_Number'Write (S, V.endCharacter);
      JS.Key ("kind");
      Optional_Virtual_String'Write (S, V.kind);
      JS.End_Object;
   end Write_FoldingRange;

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DocumentColorParams;

   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentColorParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DocumentColorParams;

   procedure Read_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := SignatureHelpTriggerKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_SignatureHelpTriggerKind;

   procedure Write_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpTriggerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((SignatureHelpTriggerKind'Pos (V)) + 1);
   end Write_SignatureHelpTriggerKind;

   procedure Read_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "triggerKind" then
               SignatureHelpTriggerKind'Read (S, V.triggerKind);
            elsif Key = "triggerCharacter" then
               Optional_Virtual_String'Read (S, V.triggerCharacter);
            elsif Key = "isRetrigger" then
               LSP.Types.Read_Boolean (JS, V.isRetrigger);
            elsif Key = "activeSignatureHelp" then
               Optional_SignatureHelp'Read (S, V.activeSignatureHelp);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelpContext;

   procedure Write_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("triggerKind");
      SignatureHelpTriggerKind'Write (S, V.triggerKind);
      JS.Key ("triggerCharacter");
      Optional_Virtual_String'Write (S, V.triggerCharacter);
      Write_Boolean (JS, "isRetrigger", V.isRetrigger);
      JS.Key ("activeSignatureHelp");
      Optional_SignatureHelp'Write (S, V.activeSignatureHelp);
      JS.End_Object;
   end Write_SignatureHelpContext;

   procedure Read_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "context" then
               Optional_SignatureHelpContext'Read (S, V.context);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SignatureHelpParams;

   procedure Write_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("context");
      Optional_SignatureHelpContext'Write (S, V.context);
      JS.End_Object;
   end Write_SignatureHelpParams;

   procedure Read_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NavigationRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "position" then
               LSP.Messages.Position'Read (S, V.position);
            elsif Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "alsDisplayMethodAncestryOnNavigation" then
               Optional_AlsDisplayMethodAncestryOnNavigationPolicy'Read (S, V.alsDisplayMethodAncestryOnNavigation);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_NavigationRequestParams;

   procedure Write_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NavigationRequestParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("alsDisplayMethodAncestryOnNavigation");
      Optional_AlsDisplayMethodAncestryOnNavigationPolicy'Write (S, V.alsDisplayMethodAncestryOnNavigation);
      JS.End_Object;
   end Write_NavigationRequestParams;

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "positions" then
               Position_Vector'Read (S, V.positions);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SelectionRangeParams;

   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("positions");
      Position_Vector'Write (S, V.positions);
      JS.End_Object;
   end Write_SelectionRangeParams;

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SelectionRange;

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_SelectionRange;

   procedure Read_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LinkedEditingRanges)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "ranges" then
               Span_Vector'Read (S, V.ranges);
            elsif Key = "wordPattern" then
               Optional_Virtual_String'Read (S, V.wordPattern);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LinkedEditingRanges;

   procedure Write_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LinkedEditingRanges)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("ranges");
      Span_Vector'Write (S, V.ranges);
      JS.Key ("wordPattern");
      Optional_Virtual_String'Write (S, V.wordPattern);
      JS.End_Object;
   end Write_LinkedEditingRanges;

   procedure Read_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "name" then
               LSP.Types.Read_String (S, V.name);
            elsif Key = "kind" then
               SymbolKind'Read (S, V.kind);
            elsif Key = "tags" then
               Optional_SymbolTagSet'Read (S, V.tags);
            elsif Key = "detail" then
               Optional_Virtual_String'Read (S, V.detail);
            elsif Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            elsif Key = "selectionRange" then
               LSP.Messages.Span'Read (S, V.selectionRange);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CallHierarchyItem;

   procedure Write_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.Key ("kind");
      SymbolKind'Write (S, V.kind);
      JS.Key ("tags");
      Optional_SymbolTagSet'Write (S, V.tags);
      JS.Key ("detail");
      Optional_Virtual_String'Write (S, V.detail);
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("selectionRange");
      LSP.Messages.Span'Write (S, V.selectionRange);
      JS.End_Object;
   end Write_CallHierarchyItem;

   procedure Read_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCallsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "item" then
               CallHierarchyItem'Read (S, V.item);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CallHierarchyIncomingCallsParams;

   procedure Write_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCallsParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("item");
      CallHierarchyItem'Write (S, V.item);
      JS.End_Object;
   end Write_CallHierarchyIncomingCallsParams;

   procedure Read_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCall)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "from" then
               CallHierarchyItem'Read (S, V.from);
            elsif Key = "fromRanges" then
               Span_Vector'Read (S, V.fromRanges);
            elsif Key = "kinds" then
               AlsReferenceKind_Vector'Read (S, V.kinds);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CallHierarchyIncomingCall;

   procedure Write_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCall)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("from");
      CallHierarchyItem'Write (S, V.from);
      JS.Key ("fromRanges");
      Span_Vector'Write (S, V.fromRanges);
      JS.Key ("kinds");
      AlsReferenceKind_Vector'Write (S, V.kinds);
      JS.End_Object;
   end Write_CallHierarchyIncomingCall;

   procedure Read_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyOutgoingCall)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "to" then
               CallHierarchyItem'Read (S, V.to);
            elsif Key = "fromRanges" then
               Span_Vector'Read (S, V.fromRanges);
            elsif Key = "kinds" then
               AlsReferenceKind_Vector'Read (S, V.kinds);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CallHierarchyOutgoingCall;

   procedure Write_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyOutgoingCall)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("to");
      CallHierarchyItem'Write (S, V.to);
      JS.Key ("fromRanges");
      Span_Vector'Write (S, V.fromRanges);
      JS.Key ("kinds");
      AlsReferenceKind_Vector'Write (S, V.kinds);
      JS.End_Object;
   end Write_CallHierarchyOutgoingCall;

   procedure Read_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensParams;

   procedure Write_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_SemanticTokensParams;

   procedure Read_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokens)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "resultId" then
               Optional_Virtual_String'Read (S, V.resultId);
            elsif Key = "data" then
               uinteger_Vector'Read (S, V.data);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokens;

   procedure Write_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokens)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("resultId");
      Optional_Virtual_String'Write (S, V.resultId);
      JS.Key ("data");
      uinteger_Vector'Write (S, V.data);
      JS.End_Object;
   end Write_SemanticTokens;

   procedure Read_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensPartialResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "data" then
               uinteger_Vector'Read (S, V.data);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensPartialResult;

   procedure Write_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensPartialResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("data");
      uinteger_Vector'Write (S, V.data);
      JS.End_Object;
   end Write_SemanticTokensPartialResult;

   procedure Read_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "previousResultId" then
               LSP.Types.Read_String (S, V.previousResultId);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensDeltaParams;

   procedure Write_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("previousResultId");
      LSP.Types.Write_String (S, V.previousResultId);
      JS.End_Object;
   end Write_SemanticTokensDeltaParams;

   procedure Read_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "start" then
               uinteger'Read (S, V.start);
            elsif Key = "deleteCount" then
               uinteger'Read (S, V.deleteCount);
            elsif Key = "data" then
               uinteger_Vector'Read (S, V.data);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensEdit;

   procedure Write_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      uinteger'Write (S, V.start);
      JS.Key ("deleteCount");
      uinteger'Write (S, V.deleteCount);
      JS.Key ("data");
      uinteger_Vector'Write (S, V.data);
      JS.End_Object;
   end Write_SemanticTokensEdit;

   procedure Read_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDelta)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "resultId" then
               Optional_Virtual_String'Read (S, V.resultId);
            elsif Key = "edits" then
               SemanticTokensEdit_Vector'Read (S, V.edits);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensDelta;

   procedure Write_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDelta)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("resultId");
      Optional_Virtual_String'Write (S, V.resultId);
      JS.Key ("edits");
      SemanticTokensEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_SemanticTokensDelta;

   procedure Read_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaPartialResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "edits" then
               SemanticTokensEdit_Vector'Read (S, V.edits);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensDeltaPartialResult;

   procedure Write_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaPartialResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("edits");
      SemanticTokensEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_SemanticTokensDeltaPartialResult;

   procedure Read_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "workDoneToken" then
               Optional_ProgressToken'Read (S, V.workDoneToken);
            elsif Key = "partialResultToken" then
               Optional_ProgressToken'Read (S, V.partialResultToken);
            elsif Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "range" then
               LSP.Messages.Span'Read (S, V.span);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SemanticTokensRangeParams;

   procedure Write_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRangeParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.workDoneToken);
      JS.Key ("partialResultToken");
      Optional_ProgressToken'Write (S, V.partialResultToken);
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_SemanticTokensRangeParams;

   procedure Read_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UniquenessLevel)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "document" then
         V := document;
      elsif Text = "project" then
         V := project;
      elsif Text = "group" then
         V := group;
      elsif Text = "scheme" then
         V := scheme;
      elsif Text = "global" then
         V := global;
      else
         V := UniquenessLevel'First;
      end if;
   end Read_UniquenessLevel;

   procedure Write_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UniquenessLevel)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : UniquenessLevel)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : UniquenessLevel)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when document =>
               return "document";
            when project =>
               return "project";
            when group =>
               return "group";
            when scheme =>
               return "scheme";
            when global =>
               return "global";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_UniquenessLevel;

   procedure Read_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MonikerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      if Text = "import" then
         V := import;
      elsif Text = "export" then
         V := export;
      elsif Text = "local" then
         V := local;
      else
         V := MonikerKind'First;
      end if;
   end Read_MonikerKind;

   procedure Write_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MonikerKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      function To_Virtual_String
        (Value : MonikerKind)
         return VSS.Strings.Virtual_String;

      function To_Virtual_String
        (Value : MonikerKind)
         return VSS.Strings.Virtual_String is
      begin
         case Value is
            when import =>
               return "import";
            when export =>
               return "export";
            when local =>
               return "local";
         end case;
      end To_Virtual_String;

   begin
      JS.Write_String (To_Virtual_String (V));
   end Write_MonikerKind;

   procedure Read_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Moniker)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "scheme" then
               LSP.Types.Read_String (S, V.scheme);
            elsif Key = "identifier" then
               LSP.Types.Read_String (S, V.identifier);
            elsif Key = "unique" then
               UniquenessLevel'Read (S, V.unique);
            elsif Key = "kind" then
               Optional_MonikerKind'Read (S, V.kind);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Moniker;

   procedure Write_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Moniker)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scheme");
      LSP.Types.Write_String (S, V.scheme);
      JS.Key ("identifier");
      LSP.Types.Write_String (S, V.identifier);
      JS.Key ("unique");
      UniquenessLevel'Write (S, V.unique);
      JS.Key ("kind");
      Optional_MonikerKind'Write (S, V.kind);
      JS.End_Object;
   end Write_Moniker;

   procedure Read_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "external" then
               Optional_Boolean'Read (S, V.external);
            elsif Key = "takeFocus" then
               Optional_Boolean'Read (S, V.takeFocus);
            elsif Key = "selection" then
               Optional_Span'Read (S, V.selection);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowDocumentParams;

   procedure Write_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("external");
      Optional_Boolean'Write (S, V.external);
      JS.Key ("takeFocus");
      Optional_Boolean'Write (S, V.takeFocus);
      JS.Key ("selection");
      Optional_Span'Write (S, V.selection);
      JS.End_Object;
   end Write_ShowDocumentParams;

   procedure Read_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "success" then
               LSP.Types.Read_Boolean (JS, V.success);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ShowDocumentResult;

   procedure Write_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, "success", V.success);
      JS.End_Object;
   end Write_ShowDocumentResult;

   procedure Read_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileCreate)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_String (S, V.uri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileCreate;

   procedure Write_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileCreate)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_String (S, V.uri);
      JS.End_Object;
   end Write_FileCreate;

   procedure Read_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CreateFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "files" then
               FileCreate_Vector'Read (S, V.files);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_CreateFilesParams;

   procedure Write_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CreateFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("files");
      FileCreate_Vector'Write (S, V.files);
      JS.End_Object;
   end Write_CreateFilesParams;

   procedure Read_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileRename)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "oldUri" then
               LSP.Types.Read_String (S, V.oldUri);
            elsif Key = "newUri" then
               LSP.Types.Read_String (S, V.newUri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileRename;

   procedure Write_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileRename)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("oldUri");
      LSP.Types.Write_String (S, V.oldUri);
      JS.Key ("newUri");
      LSP.Types.Write_String (S, V.newUri);
      JS.End_Object;
   end Write_FileRename;

   procedure Read_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "files" then
               FileRename_Vector'Read (S, V.files);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_RenameFilesParams;

   procedure Write_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("files");
      FileRename_Vector'Write (S, V.files);
      JS.End_Object;
   end Write_RenameFilesParams;

   procedure Read_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileDelete)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_String (S, V.uri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_FileDelete;

   procedure Write_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileDelete)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_String (S, V.uri);
      JS.End_Object;
   end Write_FileDelete;

   procedure Read_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeleteFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "files" then
               FileDelete_Vector'Read (S, V.files);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_DeleteFilesParams;

   procedure Write_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeleteFilesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("files");
      FileDelete_Vector'Write (S, V.files);
      JS.End_Object;
   end Write_DeleteFilesParams;

   procedure Read_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogTraceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "message" then
               LSP.Types.Read_String (S, V.message);
            elsif Key = "verbose" then
               Optional_Virtual_String'Read (S, V.verbose);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_LogTraceParams;

   procedure Write_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogTraceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("message");
      LSP.Types.Write_String (S, V.message);
      JS.Key ("verbose");
      Optional_Virtual_String'Write (S, V.verbose);
      JS.End_Object;
   end Write_LogTraceParams;

   procedure Read_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SetTraceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "value" then
               TraceValue'Read (S, V.value);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_SetTraceParams;

   procedure Write_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetTraceParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("value");
      TraceValue'Write (S, V.value);
      JS.End_Object;
   end Write_SetTraceParams;

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "location" then
               Location'Read (S, V.loc);
            elsif Key = "name" then
               LSP.Types.Read_String (S, V.name);
            elsif Key = "refs" then
               Location_Vector'Read (S, V.refs);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_Subprogram_And_References;

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Write (S, V.loc);
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.Key ("refs");
      Location_Vector'Write (S, V.refs);
      JS.End_Object;
   end Write_ALS_Subprogram_And_References;

   procedure Read_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Source_Dir_Description)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "name" then
               LSP.Types.Read_String (S, V.name);
            elsif Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_Source_Dir_Description;

   procedure Write_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Source_Dir_Description)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write_String (S, V.name);
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.End_Object;
   end Write_ALS_Source_Dir_Description;

   procedure Read_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := ALS_ShowDependenciesKind'Val (JS.R.Number_Value.Integer_Value - 1);
      JS.R.Read_Next;
   end Read_ALS_ShowDependenciesKind;

   procedure Write_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesKind)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer ((ALS_ShowDependenciesKind'Pos (V)) + 1);
   end Write_ALS_ShowDependenciesKind;

   procedure Read_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "textDocument" then
               TextDocumentIdentifier'Read (S, V.textDocument);
            elsif Key = "kind" then
               ALS_ShowDependenciesKind'Read (S, V.kind);
            elsif Key = "showImplicit" then
               LSP.Types.Read_Boolean (JS, V.showImplicit);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_ShowDependenciesParams;

   procedure Write_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("kind");
      ALS_ShowDependenciesKind'Write (S, V.kind);
      Write_Boolean (JS, "showImplicit", V.showImplicit);
      JS.End_Object;
   end Write_ALS_ShowDependenciesParams;

   procedure Read_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Unit_Description)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "uri" then
               LSP.Types.Read_LSP_URI (S, V.uri);
            elsif Key = "projectUri" then
               LSP.Types.Read_LSP_URI (S, V.projectUri);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_Unit_Description;

   procedure Write_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Unit_Description)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write_LSP_URI (S, V.uri);
      JS.Key ("projectUri");
      LSP.Types.Write_LSP_URI (S, V.projectUri);
      JS.End_Object;
   end Write_ALS_Unit_Description;

end LSP.Message_IO;
