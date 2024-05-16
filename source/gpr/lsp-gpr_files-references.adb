------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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
--
--  This package provides GPR references API.

with Ada.Containers.Vectors;

with LSP.Text_Documents.Langkit_Documents;

with VSS.Strings.Conversions;

package body LSP.GPR_Files.References is

   use type GPC.Token_Kind;

   package Identifier_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => GPC.Token_Reference);

   Project_Identifier_Id : constant Project_Id := +"project";
   --  Identifier used to reference attributes, types, variables & packages
   --  of the project itself.

   function Previous_Token
     (Ref  : GPC.Token_Reference;
      File : LSP.GPR_Files.File_Access) return GPC.Token_Reference;
   --  go to previous token if any or stay at first

   function Next_Token
     (Ref  : GPC.Token_Reference;
      File : LSP.GPR_Files.File_Access) return GPC.Token_Reference;
   --  go to next token if any or stay at last

   function Token_Reference
     (Ref : Reference) return Gpr_Parser.Common.Token_Reference;
   --  Token_Reference adapter.

   --------------------
   -- Previous_Token --
   --------------------

   function Previous_Token
     (Ref  : GPC.Token_Reference;
      File : LSP.GPR_Files.File_Access) return GPC.Token_Reference
   is
   begin
      if Ref /= GPC.No_Token then
         declare
            Result : constant GPC.Token_Reference :=
                       GPC.Previous (Ref, True);
         begin
            if Result /= GPC.No_Token then
               return Result;
            end if;
         end;
      end if;
      return File.Unit.First_Token;
   end Previous_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Ref  : GPC.Token_Reference;
      File : LSP.GPR_Files.File_Access) return GPC.Token_Reference is
   begin
      if Ref /= GPC.No_Token then
         declare
            Result : constant GPC.Token_Reference := GPC.Next (Ref, True);
         begin
            if Result /= GPC.No_Token then
               return Result;
            end if;
         end;
      end if;
      return File.Unit.Last_Token;
   end Next_Token;

   ---------------------
   -- Token_Reference --
   ---------------------

   function Token_Reference
     (Ref : Reference) return Gpr_Parser.Common.Token_Reference is
   begin
      return Ref.Token;
   end Token_Reference;

   --------------------------
   -- Identifier_Reference --
   --------------------------

   function Identifier_Reference
     (File     : LSP.GPR_Files.File_Access;
      Current_Package : GPR2.Package_Id;
      Token           : Gpr_Parser.Common.Token_Reference)
         return Reference is

      Project_Provided : Boolean := False;
      --  True if reference qualified name contains project's name

      Package_Provided : Boolean := False;
      --  True if reference qualified name contains a package's name

      Referenced_Project : LSP.GPR_Files.File_Access := File;
      --  Project referenced at 'Location'

      Referenced_Package : GPR2.Package_Id := GPR2.Project_Level_Scope;
      --  Package referenced at 'Location'

      Identifiers : Identifier_Vectors.Vector;
      --  Identifier list found before 'Location'

      Tick_Found : Boolean := False;
      --  True if "'" is at the left of position

      In_Type_Reference : Boolean := False;
      --  True if ":" is at the left of reference

      Previous_Token_Kind : GPC.Token_Kind;
      --  token kind just before 'Location'

      Next_Token_Kind : GPC.Token_Kind;
      --  token kind just after 'Location'

      procedure Initialize (Last_Token : GPC.Token_Reference);
      --  Initialize 'Identifiers' & 'Tick_Found'

      procedure Remove_Project;
      --  remove from Identifiers project part
      --  set Project & Prj_Path 'Ref' fields & 'Referenced_Project'

      procedure Remove_Package;
      --  remove from Identifiers package part and set 'Ref' Pack field

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Last_Token : GPC.Token_Reference) is
         Token    : GPC.Token_Reference := Last_Token;
         First    : Boolean := True;
      begin
         Previous_Token_Kind :=
           GPC.Kind (GPC.Data (Previous_Token (Token, File)));
         Next_Token_Kind := GPC.Kind (GPC.Data (Next_Token (Token, File)));

         while Token /= GPC.No_Token
           and then Token.Data.Kind = GPC.Gpr_Identifier loop
            Identifiers.Prepend (Token);
            Token := Token.Previous (True);
            if Token /= GPC.No_Token then
               if Token.Data.Kind = GPC.Gpr_Tick then
                  if First then
                     Tick_Found := True;
                  else
                     --  Tick only supported just before 'Location'

                     Identifiers.Clear;
                     return;
                  end if;
               elsif Token.Data.Kind = GPC.Gpr_Identifier then
                  --  An identifier followed by an identifier is not allowed.

                  Identifiers.Clear;
                  return;
               elsif Token.Data.Kind = GPC.Gpr_Colon
                 or else Token.Data.Kind = GPC.Gpr_Type
               then
                  --  Let 'Initialize' return a type reference

                  In_Type_Reference := True;

                  return;
               elsif Token.Data.Kind /= GPC.Gpr_Dot then
                  --  Let 'Initialize' return a possible reference.

                  return;

               end if;

               --  Loop on previous token

               First := False;
               Token := Token.Previous (True);
            else
               --  A GPR file cannot start with a reference

               Identifiers.Clear;
               return;
            end if;
         end loop;

         --  'Location' not within a valid reference.

         Identifiers.Clear;
      end Initialize;

      --------------------
      -- Remove_Project --
      --------------------

      procedure Remove_Project is
         Project : constant Project_Id :=
                     +To_Lower_String (Identifiers.First_Element);
      begin
         if Project = Project_Identifier_Id or else Project = File.Name then
            Project_Provided := True;
            Identifiers.Delete_First;
         elsif File.Imported.Contains (Project) then
            Identifiers.Delete_First;
            Project_Provided := True;
            Referenced_Project := LSP.GPR_Files.Parse
              (File.File_Provider,
               File.Name_To_File_Map.Element (Project));
            return;
         elsif File.Extended = Project then
            Identifiers.Delete_First;
            Project_Provided := True;
            Referenced_Project := LSP.GPR_Files.Parse
              (File.File_Provider,
               File.Extended_Path);
            return;
         end if;
         Referenced_Project := File;
      end Remove_Project;

      --------------------
      -- Remove_Package --
      --------------------

      procedure Remove_Package is

         Id : constant GPR2.Package_Id :=
                +Optional_Name_Type
                  (To_Lower_String (Identifiers.First_Element));
      begin
         if Referenced_Project.Packages.Contains (Id) then
            Referenced_Package := Id;
            Package_Provided := True;
            Identifiers.Delete_First;
         end if;
      end Remove_Package;

      use type Ada.Containers.Count_Type;
   begin

      if Token = GPC.No_Token
        or else Token.Data.Kind /= GPC.Gpr_Identifier
      then
         return No_Reference;
      end if;

      Initialize (Token);

      if Identifiers.Length > 0 then
         Remove_Project;
      end if;

      if Identifiers.Length = 0 then
         if Project_Provided then
            return (Kind               => Project_Ref,
                    Token              => Referenced_Project.Name_Token,
                    Project            => Referenced_Project.Name,
                    In_Type_Reference => In_Type_Reference);
         end if;
         return No_Reference;
      end if;

      if Identifiers.Length > 0 then
         Remove_Package;
      end if;

      if Identifiers.Length = 0 then
         if Package_Provided then
            declare
               P : constant LSP.GPR_Files.Package_Maps.Cursor :=
                     Referenced_Project.Packages.
                       Find (Referenced_Package);
               D : Package_Definition;
            begin
               if LSP.GPR_Files.Package_Maps.Has_Element (P) then
                  D := LSP.GPR_Files.Package_Maps.Element (P);
                  return (Kind               => Package_Ref,
                          Token              => D.First,
                          Project => Referenced_Project.Name,
                          In_Type_Reference  => False,
                          Pack               => Referenced_Package);
               else
                  return (Kind               => Package_Ref,
                          Token              => Token,
                          Project => Referenced_Project.Name,
                          In_Type_Reference  => False,
                          Pack               => Referenced_Package);
               end if;
            end;
         end if;
         return No_Reference;
      end if;

      if Previous_Token_Kind = GPC.Gpr_For then
         Tick_Found := True;
         Referenced_Package := Current_Package;
      end if;

      if Identifiers.Length = 1 then
         if Tick_Found then
            declare
               Attribute : constant GPR2.Attribute_Id :=
                             +Optional_Name_Type
                               (To_String (Identifiers.First_Element));
               Index     : constant Index_Type :=
                             LSP.GPR_Files.Index
                               (Self            => File.all,
                                Attribute_Token => Identifiers.First_Element,
                                Current_Package => Referenced_Package);
            begin
               if Referenced_Package /= GPR2.Project_Level_Scope then
                  declare
                     P : constant LSP.GPR_Files.Package_Maps.Cursor :=
                           Referenced_Project.Packages.
                             Find (Referenced_Package);
                     D : Package_Definition;
                     A : LSP.GPR_Files.Attribute_Maps.Cursor;
                     M : Attribute_Index_Maps.Map;
                     C : LSP.GPR_Files.Attribute_Index_Maps.Cursor;
                  begin
                     if LSP.GPR_Files.Package_Maps.Has_Element (P) then
                        D := LSP.GPR_Files.Package_Maps.Element (P);
                        A := D.Attributes.Find (Attribute);
                        if LSP.GPR_Files.Attribute_Maps.Has_Element (A) then
                           M := LSP.GPR_Files.Attribute_Maps.Element (A);
                           C := M.Find (Index);
                           if LSP.GPR_Files.Attribute_Index_Maps.Has_Element (C)
                           then
                              return (Kind              => Attribute_Ref,
                                      Token             => LSP.GPR_Files.
                                        Attribute_Index_Maps.Element (C).Token,
                                      Project           =>
                                        Referenced_Project.Name,
                                      Pack              => Referenced_Package,
                                      Attribute         => Attribute,
                                      Index             => Index,
                                      In_Type_Reference => False);
                           end if;
                           C := M.First;
                           if LSP.GPR_Files.Attribute_Index_Maps.Has_Element (C)
                           then
                              return (Kind              => Attribute_Ref,
                                      Token             => LSP.GPR_Files.
                                        Attribute_Index_Maps.Element (C).Token,
                                      Project           =>
                                        Referenced_Project.Name,
                                      Pack              => Referenced_Package,
                                      Attribute         => Attribute,
                                      Index             => Index,
                                      In_Type_Reference => False);
                           end if;
                        end if;
                     end if;
                  end;
               else
                  declare
                     A : LSP.GPR_Files.Attribute_Maps.Cursor;
                     M : Attribute_Index_Maps.Map;
                     C : LSP.GPR_Files.Attribute_Index_Maps.Cursor;
                  begin
                     A := Referenced_Project.
                       Project_Level_Scope_Defs.Attributes.Find (Attribute);
                     if LSP.GPR_Files.Attribute_Maps.Has_Element (A) then
                        M := LSP.GPR_Files.Attribute_Maps.Element (A);
                        C := M.Find (Index);
                        if C.Has_Element then
                           return (Kind              => Attribute_Ref,
                                   Token             => C.Element.Token,
                                   Project           =>
                                     Referenced_Project.Name,
                                   Pack              => Referenced_Package,
                                   Attribute         => Attribute,
                                   Index             => Index,
                                   In_Type_Reference => False);
                        end if;
                        C := M.First;
                        if C.Has_Element then
                           return (Kind              => Attribute_Ref,
                                   Token             => C.Element.Token,
                                   Project           =>
                                     Referenced_Project.Name,
                                   Pack              => Referenced_Package,
                                   Attribute         => Attribute,
                                   Index             => Index,
                                   In_Type_Reference => False);
                        end if;
                     end if;
                  end;
               end if;
               return (Kind              => Attribute_Ref,
                       Token             => Token,
                       Project           =>
                         Referenced_Project.Name,
                       Pack              => Referenced_Package,
                       Attribute         => Attribute,
                       Index             => Index,
                       In_Type_Reference => False);
            end;
         else
            declare
               Typ      : constant Type_Id :=
                            +To_String (Identifiers.First_Element);
               Variable : constant LSP.GPR_Files.Variable_Id :=
                            +To_String (Identifiers.First_Element);
            begin
               if Referenced_Project.Types.Contains (Typ) then
                  declare
                     C : constant LSP.GPR_Files.Type_Maps.Cursor :=
                           Referenced_Project.Types.Find (Typ);
                  begin
                     if LSP.GPR_Files.Type_Maps.Has_Element (C) then
                        return (Kind               => Type_Ref,
                                Token              =>
                                  LSP.GPR_Files.Type_Maps.Element (C).Token,
                                Project            =>
                                  Referenced_Project.Name,
                                In_Type_Reference => In_Type_Reference,
                                Typ                => Typ);
                     end if;
                  end;
               elsif Next_Token_Kind not in GPC.Gpr_Assign | GPC.Gpr_Colon then
                  if not Project_Provided and then not Package_Provided then
                     if Current_Package /= GPR2.Project_Level_Scope then

                        --  In this case the variable can be defined in
                        --  current package or at project level.
                        --  Use the declaration at package level if any.
                        declare
                           P : constant LSP.GPR_Files.Package_Maps.Cursor :=
                                 Referenced_Project.Packages.Find
                                   (Current_Package);
                           D : Package_Definition;
                           V : LSP.GPR_Files.Variable_Maps.Cursor;
                        begin
                           if LSP.GPR_Files.Package_Maps.Has_Element (P) then
                              D := LSP.GPR_Files.Package_Maps.Element (P);
                              V := D.Variables.Find (Variable);
                              if LSP.GPR_Files.Variable_Maps.
                                Has_Element (V)
                              then
                                 return (Kind              => Variable_Ref,
                                         Token             =>
                                           LSP.GPR_Files.Variable_Maps.
                                             Element (V).Token,
                                         Project           =>
                                           Referenced_Project.Name,
                                         Pack              => Current_Package,
                                         Variable          => Variable,
                                         In_Type_Reference => False);
                              end if;
                           end if;
                        end;
                     end if;
                     declare
                        V : constant LSP.GPR_Files.Variable_Maps.Cursor :=
                              Referenced_Project.Project_Level_Scope_Defs.
                                Variables.Find (Variable);
                     begin
                        if LSP.GPR_Files.Variable_Maps.Has_Element (V) then
                           return (Kind              => Variable_Ref,
                                   Token             =>
                                     LSP.GPR_Files.Variable_Maps.
                                       Element (V).Token,
                                   Project           =>
                                     Referenced_Project.Name,
                                   Pack              =>
                                     GPR2.Project_Level_Scope,
                                   Variable          => Variable,
                                   In_Type_Reference => False);
                        end if;
                     end;
                  elsif Referenced_Package /= GPR2.Project_Level_Scope then
                     declare
                        P : constant LSP.GPR_Files.Package_Maps.Cursor :=
                              Referenced_Project.Packages.
                                Find (Referenced_Package);
                        D : Package_Definition;
                        V : LSP.GPR_Files.Variable_Maps.Cursor;
                     begin
                        if LSP.GPR_Files.Package_Maps.Has_Element (P) then
                           D := LSP.GPR_Files.Package_Maps.Element (P);
                           V := D.Variables.Find (Variable);
                           if LSP.GPR_Files.Variable_Maps.
                             Has_Element (V)
                           then
                              return (Kind              => Variable_Ref,
                                      Token             =>
                                        LSP.GPR_Files.Variable_Maps.
                                          Element (V).Token,
                                      Project           =>
                                        Referenced_Project.Name,
                                      Pack              => Referenced_Package,
                                      Variable          => Variable,
                                      In_Type_Reference => False);
                           end if;
                        end if;
                     end;
                  else
                     declare
                        V : constant LSP.GPR_Files.Variable_Maps.Cursor :=
                              Referenced_Project.Project_Level_Scope_Defs.
                                Variables.Find (Variable);
                     begin
                        if LSP.GPR_Files.Variable_Maps.Has_Element (V) then
                           return (Kind              => Variable_Ref,
                                   Token             =>
                                     LSP.GPR_Files.Variable_Maps.
                                       Element (V).Token,
                                   Project           =>
                                     Referenced_Project.Name,
                                   Pack              =>
                                     GPR2.Project_Level_Scope,
                                   Variable          => Variable,
                                   In_Type_Reference => False);
                        end if;
                     end;
                  end if;
               else
                  return (Kind              => Variable_Ref,
                          Token             => Identifiers.First_Element,
                          Project           => File.Name,
                          Pack              => Current_Package,
                          Variable          => Variable,
                          In_Type_Reference => False);
               end if;
            end;
         end if;
      end if;
      return No_Reference;
   end Identifier_Reference;

   ---------------------
   -- Token_Reference --
   ---------------------

   function Token_Reference
     (File     : LSP.GPR_Files.File_Access;
      Position : LSP.Structures.Position)
      return Gpr_Parser.Common.Token_Reference is

      package LKD renames LSP.Text_Documents.Langkit_Documents;

      Location : constant Gpr_Parser.Slocs.Source_Location :=
                   LSP.GPR_Files.To_Langkit_Location
                     (LKD.To_Source_Location
                        (Line_Text => File.Get_Line
                           (LKD.To_Source_Line (Position.line)),
                         Position  => Position));

      Token : constant GPC.Token_Reference :=
                LSP.GPR_Files.Token (File.all, Location);

      function String_Reference return Gpr_Parser.Common.Token_Reference;
      --  Resolves reference when 'Position' on a string token

      ----------------------
      -- String_Reference --
      ----------------------

      function String_Reference return Gpr_Parser.Common.Token_Reference is

         function Is_Referenced_Project return Boolean;
         --  True if 'Position' at a string referencing an imported, extended
         --  or aggregated project

         ---------------------------
         -- Is_Referenced_Project --
         ---------------------------

         function Is_Referenced_Project return Boolean is
            Finished : Boolean := False;
            Reference_Project : Boolean := False;
            String_Allowed : Boolean := True;
            With_Allowed : Boolean := False;
            Comma_Allowed : Boolean := False;
            Par_Open_Allowed : Boolean := False;
            Extends_Allowed : Boolean := False;
            All_Allowed : Boolean := False;

            Token : GPC.Token_Reference :=
                      LSP.GPR_Files.Token (File.all, Location);
         begin
            while not Finished and then Token /= GPC.No_Token loop
               case Token.Data.Kind is
                  when GPC.Gpr_String =>
                     if String_Allowed then
                        String_Allowed := False;
                        Comma_Allowed := True;
                        With_Allowed := True;
                        Par_Open_Allowed := True;
                        Extends_Allowed := True;
                        All_Allowed := True;
                     else
                        Finished := True;
                     end if;
                  when GPC.Gpr_Comma =>
                     if Comma_Allowed then
                        String_Allowed := True;
                        Comma_Allowed := False;
                        With_Allowed := False;
                        Par_Open_Allowed := False;
                        Extends_Allowed := False;
                        All_Allowed := False;
                     else
                        Finished := True;
                     end if;
                  when GPC.Gpr_With =>
                     Finished := True;
                     if With_Allowed then
                        Reference_Project := True;
                     end if;
                  when GPC.Gpr_All =>
                     Finished := True;
                     if All_Allowed then
                        Reference_Project := True;
                     end if;
                  when GPC.Gpr_Extends =>
                     Finished := True;
                     if Extends_Allowed then
                        Reference_Project := True;
                     end if;
                  when GPC.Gpr_Par_Open =>
                     Finished := True;
                     if Par_Open_Allowed then
                        Token := Previous_Token (Token, File);
                        if GPC.Kind (GPC.Data (Token)) = GPC.Gpr_Use then
                           Token := Previous_Token (Token, File);
                        end if;
                        if GPC.Kind (GPC.Data (Token)) = GPC.Gpr_Identifier
                          and then To_Lower_String (Token) = "project_files"
                        then
                           Reference_Project := True;
                        end if;
                     end if;
                  when others =>
                     Finished := True;
               end case;
               Token := Previous_Token (Token, File);
            end loop;
            return Reference_Project;
         end Is_Referenced_Project;
      begin

         if Token /= GPC.No_Token
           and then Token.Data.Kind = GPC.Gpr_String
           and then Is_Referenced_Project
         then
            declare
               Path     : constant GPR2.Path_Name.Object :=
                            File.Get_Referenced_GPR (Token);
               Referenced : File_Access;
            begin
               if Path.Is_Defined and then Path.Exists then
                  Referenced := Parse (File.File_Provider, Path);
                  return Referenced.Name_Token;
               end if;
            end;
         end if;
         return GPC.No_Token;
      end String_Reference;

   begin
      if Token /= GPC.No_Token then
         case Token.Data.Kind is
            when GPC.Gpr_Identifier =>
               return Token_Reference
                 (Identifier_Reference
                    (File, File.Get_Package (Position), Token));
            when GPC.Gpr_String =>
               return String_Reference;
            when others =>
               null;
         end case;
      end if;
      return GPC.No_Token;
   end Token_Reference;

   ----------
   -- Kind --
   ----------

   function Kind (Ref : Reference) return Ref_Kind is
   begin
      return Ref.Kind;
   end Kind;

   ---------------------
   -- Referenced_File --
   ---------------------

   function Referenced_File
     (File      : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.File_Access is
   begin
      if Reference.Kind /= No_Ref then
         declare
            Cursor : constant Name_To_File_Maps.Cursor :=
                          File.Name_To_File_Map.Find (Reference.Project);
            Path   : GPR2.Path_Name.Object;
         begin
            if Cursor.Has_Element then
               Path := Cursor.Element;
               if Path.Exists then
                  return File.File_Provider.Get_Parsed_File (Path);
               end if;
            end if;
         end;
      end if;
      return File;
   end Referenced_File;

   ------------------------
   -- Referenced_Package --
   ------------------------

   function Referenced_Package
     (Reference : LSP.GPR_Files.References.Reference)
      return GPR2.Package_Id is
   begin
      if Has_Package (Reference) then
         return Reference.Pack;
      else
         return GPR2.Project_Level_Scope;
      end if;
   end Referenced_Package;

   function Get_Referenced_File
     (File : LSP.GPR_Files.File_Access;
      Reference : LSP.GPR_Files.References.Reference)
      return LSP.GPR_Files.File_Access is
   begin
      if Reference /= No_Reference then
         declare
            Cursor : constant LSP.GPR_Files.Name_To_File_Maps.Cursor
              := File.Name_To_File_Map.Find (Reference.Project);
            Path : constant GPR2.Path_Name.Object :=
                     (if Cursor.Has_Element
                      then Cursor.Element
                      else GPR2.Path_Name.Undefined);
         begin
            if Path.Is_Defined and then Path.Exists then
               return File.File_Provider.Get_Parsed_File (Path);
            end if;
         end;
      end if;
      return File;
   end Get_Referenced_File;

   --------------------------
   -- Referenced_Attribute --
   --------------------------

   function Referenced_Attribute
     (Reference : LSP.GPR_Files.References.Reference)
      return Attribute_Definition is
   begin
      if Is_Attribute_Reference (Reference) then
         declare
            Name     : constant GPR2.Q_Attribute_Id :=
                         (Reference.Pack, Reference.Attribute);
            Attr_Def : Attribute_Definition;
         begin
            Attr_Def.Name := Name;
            Attr_Def.At_Pos := Reference.Index.At_Pos;
            if Reference.Index.Is_Others then
               Attr_Def.Index := GPR2.Project.Attribute_Index.I_Others;
            elsif Reference.Index /= No_Index then
               declare
                  Value : constant GPR2.Value_Type :=
                            VSS.Strings.Conversions.To_UTF_8_String
                              (Reference.Index.Text);
                  Case_Sensitive : constant Boolean :=
                                     (if PRA.Exists (Name)
                                      then PRA.Is_Case_Sensitive
                                        (Index_Value => Value,
                                         Index_Type  =>
                                           PRA.Get (Name).Index_Type)
                                      else False);
               begin
                  Attr_Def.Index := GPR2.Project.Attribute_Index.Create
                    (Value          => Value,
                     Case_Sensitive => Case_Sensitive);
               end;

            end if;

            return Attr_Def;
         end;
      else
         return No_Attribute_Definition;
      end if;
   end Referenced_Attribute;

end LSP.GPR_Files.References;
