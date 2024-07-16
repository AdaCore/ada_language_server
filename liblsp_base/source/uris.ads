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
--
--  URI (Uniform Resource Identifier) described in RFC 3986.
--  This package provides URI type corresponding methods.
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;

with GNAT.OS_Lib;

package URIs is

   subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;

   --  A URI is a sequence of characters from a very limited set: the letters
   --  of the basic Latin alphabet, digits, and a few special characters.
   subtype URI_String is String
     with Dynamic_Predicate =>
       (for all Char of URI_String =>
          Char in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
            | '-' | '.' | '_' | '~'
            | ':' | '/' | '?' | '#' | '[' | ']' | '@'
            | '!' | '$' | '&' | ''' | '(' | ')'
            | '*' | '+' | ',' | ';' | '=' | '%');

   type URI is tagged limited private;
   --  URI is an Uniform Resource Identifier.

   function Scheme (Self : URI'Class) return String;
   --  Return scheme part of the URI. Case-insensitive, so always in lowercase

   procedure Set_Scheme
     (Self  : in out URI'Class;
      Value : String)
     with Pre =>
       Value'Length > 1
       and then Value (Value'First) in 'a' .. 'z' | 'A' .. 'Z'
       and then (for all Char of Value => Char in
                 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.');

   function Host (Self : URI'Class) return UTF_8_String;
   --  Return host of authority from the URI.
   --  Case-insensitive, so always in lowercase

   procedure Set_Host
     (Self  : in out URI'Class;
      Value : UTF_8_String);

   type Path_Cursor is tagged private;
   --  Cursor to iterate over URI path segments

   function Has_Element (Position : Path_Cursor) return Boolean;

   function To_String (Position : Path_Cursor) return String;
   --  Return segment value without a separator

   package Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Path_Cursor, Has_Element);

   function Each_Path_Segment
     (Self : URI'Class) return Iterator_Interfaces.Reversible_Iterator'Class;
   --  Iterate over each segment

   procedure Add_Path_Segment
     (Self  : in out URI'Class;
      Value : UTF_8_String);
   --  Append next path segment to URI's path

   function To_String (Self : URI'Class) return URI_String;
   --  Convert URI to string form

   procedure Parse
     (Self    : out URI'Class;
      Text    : String;
      Success : out Boolean);
   --  Parse given text of uri into the URI object.

   package Conversions is

      function From_File (Full_Path : String) return URI_String
        with Pre => GNAT.OS_Lib.Is_Absolute_Path (Full_Path);
      --  Convert from file to URI in form of file://path
      --  Argument should be a absolute path (not relative one).
      --
      --  On Windows:
      --  "z:\ar" converted to "file:///z%3A/ar"
      --  "\\VBOXSVR\tmp\ar" converted to "file://vboxsvr/tmp/ar"
      --
      --  On Linux:
      --  "/tmp/ar" converted to "file:///tmp/ar"

      function To_File (URI : URI_String; Normalize : Boolean) return String;
      --  Convert from file:// URI to file full path. If Normalize = True
      --  then convert result is independ on letter case and symlinks.
   end Conversions;

private

   package Unbounded_String_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   type URI is tagged limited record
      Scheme    : Ada.Strings.Unbounded.Unbounded_String;
      User_Info : Ada.Strings.Unbounded.Unbounded_String;
      Host      : Ada.Strings.Unbounded.Unbounded_String;
      Port      : Natural := 0;
      Path      : Unbounded_String_Lists.List;
   end record;

   type Path_Cursor is tagged record
      Value : Unbounded_String_Lists.Cursor;
   end record;

end URIs;
