------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Wide_Wide_Characters.Handling;
with GNAT.Regpat;

package body URIs is

   package body Conversions is

      UNC : constant GNAT.Regpat.Pattern_Matcher :=  --  \\host\share\path
        GNAT.Regpat.Compile ("^\\\\([^\\]*)\\");

      Slash : constant String :=
        GNAT.Regpat.Quote ((1 => GNAT.OS_Lib.Directory_Separator));

      Segment : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile (Slash);

      ---------------
      -- From_File --
      ---------------

      function From_File (Full_Path : String) return URI_String is

         use type GNAT.Regpat.Match_Location;

         procedure Add_All_Paths
           (URI  : in out URIs.URI;
            Path : String);
         --  Add each directory of Path to URI.Path

         -------------------
         -- Add_All_Paths --
         -------------------

         procedure Add_All_Paths
           (URI  : in out URIs.URI;
            Path : String)
         is
            Found : GNAT.Regpat.Match_Array (0 .. 0);
         begin
            GNAT.Regpat.Match (Segment, Path, Found);

            if Found (0) = GNAT.Regpat.No_Match then
               URI.Add_Path_Segment (Path);

               return;

            elsif Found (0).First > Path'First then
               URI.Add_Path_Segment (Path (Path'First .. Found (0).First - 1));
            end if;

            Add_All_Paths (URI, Path (Found (0).Last + 1 .. Path'Last));
         end Add_All_Paths;

         URI   : URIs.URI;
         Found : GNAT.Regpat.Match_Array (0 .. 1);
      begin
         GNAT.Regpat.Match (UNC, Full_Path, Found);
         --  Check if we have path in form of UNC:  \\host\share\path

         if Found (0) = GNAT.Regpat.No_Match then
            Add_All_Paths (URI, Full_Path);

         else  --  UNC form, extract Host
            declare
               Host : constant String :=
                 Full_Path (Found (1).First .. Found (1).Last);
            begin
               URI.Set_Host (Host);
               Add_All_Paths
                 (URI, Full_Path (Found (1).Last + 1 .. Full_Path'Last));
            end;
         end if;

         URI.Set_Scheme ("file");

         return URI.To_String;
      end From_File;

      -------------
      -- To_File --
      -------------

      function To_File (URI : URI_String; Normalize : Boolean) return String is

         procedure Append_Path (Path : String);
         --  Append Path to Result

         Result : Ada.Strings.Unbounded.Unbounded_String;

         -----------------
         -- Append_Path --
         -----------------

         procedure Append_Path (Path : String) is
            use Ada.Strings.Unbounded;
         begin
            --  We skip empty path segments, for URI like "file:///a//b/"
            --  converts to "/a/b"
            if Path /= "" then
               if Result = Null_Unbounded_String then
                  --  Special case for the first element of the PATH:
                  --  under Windows it should be "<drive_letter>:", on
                  --  other systems it should be "/"
                  if GNAT.OS_Lib.Directory_Separator = '\' then
                     Result := To_Unbounded_String (Path);
                  else
                     Result := To_Unbounded_String ('/' & Path);
                  end if;
               else
                  Result := Result & GNAT.OS_Lib.Directory_Separator & Path;
               end if;
            end if;
         end Append_Path;

         Value  : URIs.URI;
         Ok     : Boolean;
      begin
         Value.Parse (URI, Ok);

         if Ok and then Scheme (Value) = "file" then
            if Host (Value) not in "" | "localhost" then
               Append_Path ("\\" & Host (Value));
            end if;

            for J in Value.Each_Path_Segment loop
               Append_Path (J.To_String);
            end loop;
         else
            raise Constraint_Error with "Invalid URI: " & URI;
         end if;

         if Normalize then
            return GNAT.OS_Lib.Normalize_Pathname
              (Ada.Strings.Unbounded.To_String (Result));
         else
            return Ada.Strings.Unbounded.To_String (Result);
         end if;
      end To_File;

   end Conversions;

   package Iterators is
      type Iterator is new Iterator_Interfaces.Reversible_Iterator with record
         Parent : access constant URI;
      end record;

      overriding function First (Self : Iterator) return Path_Cursor;
      overriding function Last (Self : Iterator) return Path_Cursor;

      overriding function Next
        (Self     : Iterator;
         Position : Path_Cursor) return Path_Cursor;

      overriding function Previous
        (Self     : Iterator;
         Position : Path_Cursor) return Path_Cursor;
   end Iterators;

   package body Iterators is

      overriding function First (Self : Iterator) return Path_Cursor is
      begin
         return (Value => Self.Parent.Path.First);
      end First;

      overriding function Last (Self : Iterator) return Path_Cursor is
      begin
         return (Value => Self.Parent.Path.Last);
      end Last;

      overriding function Next
        (Self     : Iterator;
         Position : Path_Cursor) return Path_Cursor
      is
         pragma Unreferenced (Self);
      begin
         return (Value => Unbounded_String_Lists.Next (Position.Value));
      end Next;

      overriding function Previous
        (Self     : Iterator;
         Position : Path_Cursor) return Path_Cursor
      is
         pragma Unreferenced (Self);
      begin
         return (Value => Unbounded_String_Lists.Previous (Position.Value));
      end Previous;

   end Iterators;

   subtype Unreserved is Character
     with Static_Predicate => Unreserved
       in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' |  '_' | '~';

   subtype Sub_Delims is Character
     with Static_Predicate => Sub_Delims
       in '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=';

   ----------------------
   -- Add_Path_Segment --
   ----------------------

   procedure Add_Path_Segment
     (Self : in out URI'Class; Value : UTF_8_String) is
   begin
      Self.Path.Append (Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Add_Path_Segment;

   -----------------------
   -- Each_Path_Segment --
   -----------------------

   function Each_Path_Segment
     (Self : URI'Class)
      return Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Iterators.Iterator'(Parent => Self'Unchecked_Access);
   end Each_Path_Segment;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Path_Cursor) return Boolean is
   begin
      return Unbounded_String_Lists.Has_Element (Position.Value);
   end Has_Element;

   ----------
   -- Host --
   ----------

   function Host (Self : URI'Class) return UTF_8_String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Host);
   end Host;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self    : out URI'Class;
      Text    : String;
      Success : out Boolean)
   is
      use Ada.Strings.Unbounded;
      use Ada.Characters.Handling;

      function Parse_Authority
        (Userinfo : out Unbounded_String;
         Host     : out Unbounded_String;
         Port     : out Natural) return Boolean;

      function Parse_Scheme (Scheme : out Unbounded_String)  return Boolean;
      function Parse_User (Userinfo : out Unbounded_String) return Boolean;
      function Parse_Host (Host : out Unbounded_String) return Boolean;
      function Parse_IP_Literal (Host : out Unbounded_String) return Boolean;
      function Parse_Reg_Name (Host : out Unbounded_String) return Boolean;
      function Parse_Port (Port : out Natural) return Boolean;
      function Parse_Segment (Segment : out Unbounded_String) return Boolean;
      function Parse_Path_Abempty
        (Path : out Unbounded_String_Lists.List) return Boolean;

      Index    : Positive := Text'First;

      ---------------------
      -- Parse_Authority --
      ---------------------

      function Parse_Authority
        (Userinfo : out Unbounded_String;
         Host     : out Unbounded_String;
         Port     : out Natural) return Boolean
      is
         Has_Userinfo : Boolean;
         pragma Unreferenced (Has_Userinfo);
         Has_Host     : Boolean;
         Has_Port     : Boolean;
         pragma Unreferenced (Has_Port);
      begin
         Port := 0;

         if Index + 1 <= Text'Last
           and then Text (Index .. Index + 1) = "//"
         then
            Index := Index + 2;
         else
            return False;
         end if;

         Has_Userinfo := Parse_User (Userinfo);
         Has_Host := Parse_Host (Host);

         if Has_Host then
            Has_Port := Parse_Port (Port);
         end if;

         return Has_Host;
      end Parse_Authority;

      ----------------
      -- Parse_Host --
      ----------------

      function Parse_Host (Host : out Unbounded_String) return Boolean is
      begin
         return Parse_IP_Literal (Host) or else Parse_Reg_Name (Host);
      end Parse_Host;

      ----------------------
      -- Parse_IP_Literal --
      ----------------------

      function Parse_IP_Literal (Host : out Unbounded_String) return Boolean is
         Result : Unbounded_String;
      begin
         if Index > Text'Last or else Text (Index) /= '[' then
            return False;
         end if;

         Append (Result, '[');

         for J in Index + 1 .. Text'Last loop
            case Text (J) is
               when Unreserved | Sub_Delims | ':' =>
                  Append (Result, Text (J));
               when ']' =>
                  Append (Result, Text (J));
                  Host := Result;
                  Index := J + 1;
                  return True;
               when others =>
                  return False;
            end case;
         end loop;

         return False;
      end Parse_IP_Literal;

      ------------------------
      -- Parse_Path_Abempty --
      ------------------------

      function Parse_Path_Abempty
        (Path : out Unbounded_String_Lists.List) return Boolean is
      begin
         while Index <= Text'Last
           and then Text (Index) = '/'
         loop
            declare
               Segment : Unbounded_String;
            begin
               Index := Index + 1;

               if Parse_Segment (Segment) then
                  Path.Append (Segment);
               else
                  return False;
               end if;
            end;
         end loop;

         return True;
      end Parse_Path_Abempty;

      ----------------
      -- Parse_Port --
      ----------------

      function Parse_Port (Port : out Natural) return Boolean is
         Result : Unbounded_String;
      begin
         if Index > Text'Last or else Text (Index) /= ':' then
            Port := 0;
            return False;
         end if;

         for J in Index + 1 .. Text'Last loop
            case Text (J) is
               when '0' .. '9' =>
                  Append (Result, Text (J));
               when others =>
                  Index := J;
                  exit;
            end case;
         end loop;

         if Length (Result) = 0 then
            Port := 0;
         else
            Port := Natural'Value (To_String (Result));
         end if;

         return True;
      end Parse_Port;

      --------------------
      -- Parse_Reg_Name --
      --------------------

      function Parse_Reg_Name (Host : out Unbounded_String) return Boolean is
         Result : Unbounded_String;
         J      : Positive := Index;
      begin
         Index := Text'Last + 1;

         while J <= Text'Last loop
            case Text (J) is
               when Unreserved | Sub_Delims =>
                  Append (Result, Text (J));
                  J := J + 1;
               when '%' =>
                  if J + 2 > Text'Last
                    or else not Is_Hexadecimal_Digit (Text (J + 1))
                    or else not Is_Hexadecimal_Digit (Text (J + 2))
                  then
                     return False;
                  end if;

                  Append
                    (Result,
                     Character'Val
                       (Natural'Value ("16#" & Text (J + 1 .. J + 2) & "#")));

                  J := J + 3;
               when others =>
                  Index := J;
                  exit;
            end case;
         end loop;

         Host := Result;
         return True;
      end Parse_Reg_Name;

      ------------------
      -- Parse_Scheme --
      ------------------

      function Parse_Scheme (Scheme : out Unbounded_String)  return Boolean is
         Result : Unbounded_String;
      begin
         if Index > Text'Last
           or else Text (Index) not in 'a' .. 'z' | 'A' .. 'Z'
         then
            return False;
         end if;

         for J in Index .. Text'Last loop
            case Text (J) is
               when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '-' | '.' =>
                  Append (Result, Text (J));
               when ':' =>
                  Scheme := Result;
                  Index := J + 1;
                  return True;
               when others =>
                  return False;
            end case;
         end loop;

         return False;
      end Parse_Scheme;

      -------------------
      -- Parse_Segment --
      -------------------

      function Parse_Segment (Segment : out Unbounded_String) return Boolean is
         Result : Unbounded_String;
         J      : Positive := Index;
      begin
         while J <= Text'Last loop
            case Text (J) is
               when Unreserved | Sub_Delims | ':' | '@' =>
                  Append (Result, Text (J));
                  J := J + 1;
               when '%' =>
                  if J + 2 > Text'Last
                    or else not Is_Hexadecimal_Digit (Text (J + 1))
                    or else not Is_Hexadecimal_Digit (Text (J + 2))
                  then
                     return False;
                  end if;

                  Append
                    (Result,
                     Character'Val
                       (Natural'Value ("16#" & Text (J + 1 .. J + 2) & "#")));

                  J := J + 3;
               when others =>
                  Index := J;
                  Segment := Result;
                  return True;
            end case;
         end loop;

         Index := Text'Last + 1;
         Segment := Result;
         return True;
      end Parse_Segment;

      ----------------
      -- Parse_User --
      ----------------

      function Parse_User (Userinfo : out Unbounded_String) return Boolean is
         Result : Unbounded_String;
         J      : Positive := Index;
      begin
         while J <= Text'Last loop
            case Text (J) is
               when Unreserved | Sub_Delims | ':' =>
                  Append (Result, Text (J));
                  J := J + 1;
               when '%' =>
                  if J + 2 > Text'Last
                    or else not Is_Hexadecimal_Digit (Text (J + 1))
                    or else not Is_Hexadecimal_Digit (Text (J + 2))
                  then
                     return False;
                  end if;

                  Append
                    (Result,
                     Character'Val
                       (Natural'Value ("16#" & Text (J + 1 .. J + 2) & "#")));

                  J := J + 3;
               when '@' =>
                  Index := J + 1;
                  Userinfo := Result;
                  return True;

               when others =>
                  return False;
            end case;
         end loop;

         return False;
      end Parse_User;

      Has_Scheme : Boolean;
      Scheme     : Ada.Strings.Unbounded.Unbounded_String;
      User_Info  : Ada.Strings.Unbounded.Unbounded_String;
      Host       : Ada.Strings.Unbounded.Unbounded_String;
      Port       : Natural := 0;
      Path       : Unbounded_String_Lists.List;
   begin
      Has_Scheme := Parse_Scheme (Scheme);

      if Has_Scheme
        and then Parse_Authority (User_Info, Host, Port)
      then
         Success := Parse_Path_Abempty (Path) and then Index > Text'Last;
      else
         Success := False;
      end if;

   --  query and fragment parts are not implemented yet

      if Success then
         Self.Set_Scheme (To_String (Scheme));
         Self.User_Info := User_Info;
         Self.Set_Host (To_String (Host));
         Self.Port := Port;
         Self.Path := Path;
      end if;
   end Parse;

   ------------
   -- Scheme --
   ------------

   function Scheme (Self : URI'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Scheme);
   end Scheme;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host (Self : in out URI'Class; Value : UTF_8_String) is
      Wide : constant Wide_Wide_String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode (Value);
      Lower : constant Wide_Wide_String :=
        Ada.Wide_Wide_Characters.Handling.To_Lower (Wide);
      UTF_8 : constant UTF_8_String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Lower);
   begin
      Self.Host := Ada.Strings.Unbounded.To_Unbounded_String (UTF_8);
   end Set_Host;

   ----------------
   -- Set_Scheme --
   ----------------

   procedure Set_Scheme (Self : in out URI'Class; Value : String) is
   begin
      Self.Scheme := Ada.Strings.Unbounded.To_Unbounded_String
        (Ada.Characters.Handling.To_Lower (Value));
   end Set_Scheme;

   ---------------
   -- To_String --
   ---------------

   function To_String (Position : Path_Cursor) return String is
      Value : constant Ada.Strings.Unbounded.Unbounded_String :=
        Unbounded_String_Lists.Element (Position.Value);
   begin
      return Ada.Strings.Unbounded.To_String (Value);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : URI'Class) return URI_String is

      procedure Append (Char : Character);
      procedure Append (Text : String);
      procedure Append_Hex (Char : Character);

      Result : Ada.Strings.Unbounded.Unbounded_String;

      ------------
      -- Append --
      ------------

      procedure Append (Char : Character) is
      begin
         Ada.Strings.Unbounded.Append (Result, Char);
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (Text : String) is
      begin
         for J of Text loop
            case J is
               when Unreserved =>
                  Append (J);
               when others =>
                  Append_Hex (J);
            end case;
         end loop;
      end Append;

      ----------------
      -- Append_Hex --
      ----------------

      procedure Append_Hex (Char : Character) is
         Pos   : constant Natural := Character'Pos (Char);
         Map   : constant String := "0123456789ABCDEF";
         Upper : constant Character := Map (1 + Pos / 16);
         Lower : constant Character := Map (1 + Pos mod 16);
      begin
         Append ('%');
         Append (Upper);
         Append (Lower);
      end Append_Hex;

      User : constant String :=
        Ada.Strings.Unbounded.To_String (Self.User_Info);
      Host : constant String := Ada.Strings.Unbounded.To_String (Self.Host);
      Port : String := Natural'Image (Self.Port);
   begin
      Ada.Strings.Unbounded.Append (Result, Self.Scheme);
      Ada.Strings.Unbounded.Append (Result, "://");

      if User /= "" then
         Append (User);
         Append ('@');
      end if;

      if Host'Length > 0 and then Host (1) = '[' then
         Ada.Strings.Unbounded.Append (Result, Host);
      else
         Append (Host);
      end if;

      if Self.Port > 0 then
         Port (1) := ':';
         Ada.Strings.Unbounded.Append (Result, Port);
      end if;

      for Segment in Self.Each_Path_Segment loop
         Append ('/');

         Append (Segment.To_String);
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end To_String;

end URIs;
