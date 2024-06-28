------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;

package body LSP.GPR_External_Tools is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRD renames GPR2.Project.Registry.Pack;

   use GPR2;
   use GPR2.Project.Registry.Attribute;
   use GPR2.Project.Registry.Attribute.Description;
   use GPR2.Project.Registry.Pack;
   use GPR2.Project.Registry.Pack.Description;

   ---------------------------------
   -- True / False attribute type --
   ---------------------------------

   function Init_Boolean_Type return PRA.Attribute_Type;
   function Init_False_Default_Value return Default_Value;

   -----------------------
   -- Init_Boolean_Type --
   -----------------------

   function Init_Boolean_Type return Attribute_Type is
      Result : Attribute_Type;
   begin
      Result.Include ("True");
      Result.Include ("False");
      return Result;
   end Init_Boolean_Type;

   ------------------------------
   -- Init_False_Default_Value --
   ------------------------------

   function Init_False_Default_Value return Default_Value is
      Result : Default_Value (D_Value);
   begin
      Result.Values.Insert (Any_Index, "False");
      return Result;
   end Init_False_Default_Value;

   Boolean_Type_Def    : constant Attribute_Type := Init_Boolean_Type;
   False_Default_Value : constant Default_Value := Init_False_Default_Value;

   --------------------------------
   -- Pass / Fail attribute type --
   --------------------------------

   function Init_Pass_Fail_Type return PRA.Attribute_Type;
   function Init_Fail_Default_Value return Default_Value;

   -------------------------
   -- Init_Pass_Fail_Type --
   -------------------------

   function Init_Pass_Fail_Type return Attribute_Type is
      Result : Attribute_Type;
   begin
      Result.Include ("Pass");
      Result.Include ("Fail");
      return Result;
   end Init_Pass_Fail_Type;

   -----------------------------
   -- Init_Fail_Default_Value --
   -----------------------------

   function Init_Fail_Default_Value return Default_Value is
      Result : Default_Value (D_Value);
   begin
      Result.Values.Insert (Any_Index, "Fail");
      return Result;
   end Init_Fail_Default_Value;

   Pass_Fail_Type_Def  : constant Attribute_Type := Init_Pass_Fail_Type;
   Fail_Default_Value : constant Default_Value := Init_Fail_Default_Value;

   procedure Add_Package
     (Name        : Package_Id;
      Description : String;
      Projects    : Projects_Kind := PRD.Everywhere);

   procedure Add_Attribute
     (Package_Name          : Package_Id;
      Attribute_Name        : Optional_Attribute_Id;
      Description           : String;
      Index_Type            : Index_Value_Type;
      Value                 : Value_Kind;
      Value_Case_Sensitive  : Boolean;
      Is_Allowed_In         : Allowed_In                 := PRA.Everywhere;
      Type_Def              : Attribute_Type             :=
        PRA.No_Attribute_Type;
      Is_Builtin            : Boolean                    := False;
      Index_Optional        : Boolean                    := False;
      Empty_Value           : Empty_Value_Status         := Allow;
      Default               : Default_Value              := No_Default_Value;
      Has_Default_In        : Allowed_In                 := Nowhere;
      Is_Toolchain_Config   : Boolean                    := False;
      Config_Concatenable   : Boolean                    := False;
      Inherit_From_Extended : Inherit_From_Extended_Type := Inherited;
      Is_Set                : Boolean                    := False);

   procedure Import_IDE_Attributes;

   procedure Import_Ant_Support_Attributes;
   procedure Import_Make_Support_Attributes;

   procedure Import_GNATsas_Attributes;
   procedure Import_GNATcheck_Attributes;
   procedure Import_Codepeer_Attributes;
   procedure Import_GNATcov_Attributes;
   procedure Import_GNATdist_Attributes;
   procedure Import_GNATdoc_Attributes;
   procedure Import_GNATemulator_Attributes;
   procedure Import_GNATtest_Attributes;
   procedure Import_GNATpp_Attributes;
   procedure Import_GNATprove_Attributes;
   procedure Import_QGen_Attributes;
   procedure Import_GNATstack_Attributes;
   procedure Import_GNATstub_Attributes;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Package_Name          : Package_Id;
      Attribute_Name        : Optional_Attribute_Id;
      Description           : String;
      Index_Type            : Index_Value_Type;
      Value                 : Value_Kind;
      Value_Case_Sensitive  : Boolean;
      Is_Allowed_In         : Allowed_In                 := PRA.Everywhere;
      Type_Def              : Attribute_Type             := No_Attribute_Type;
      Is_Builtin            : Boolean                    := False;
      Index_Optional        : Boolean                    := False;
      Empty_Value           : Empty_Value_Status         := Allow;
      Default               : Default_Value              := No_Default_Value;
      Has_Default_In        : Allowed_In                 := Nowhere;
      Is_Toolchain_Config   : Boolean                    := False;
      Config_Concatenable   : Boolean                    := False;
      Inherit_From_Extended : Inherit_From_Extended_Type := Inherited;
      Is_Set                : Boolean                    := False) is

      Name : constant Q_Attribute_Id := (Package_Name, Attribute_Name);
   begin
      if not Exists (Name) then
         Add
           (Name                  => Name,
            Index_Type            => Index_Type,
            Value                 => Value,
            Value_Case_Sensitive  => Value_Case_Sensitive,
            Is_Allowed_In         => Is_Allowed_In,
            Type_Def              => Type_Def,
            Is_Builtin            => Is_Builtin,
            Index_Optional        => Index_Optional,
            Empty_Value           => Empty_Value,
            Default               => Default,
            Has_Default_In        => Has_Default_In,
            Is_Toolchain_Config   => Is_Toolchain_Config,
            Config_Concatenable   => Config_Concatenable,
            Inherit_From_Extended => Inherit_From_Extended,
            Is_Set                => Is_Set);
      end if;
      if Get_Attribute_Description (Name)'Length = 0
        and then Description'Length > 0
      then
         Set_Attribute_Description (Name, Description);
      end if;
   end Add_Attribute;

   -----------------
   -- Add_Package --
   -----------------

   procedure Add_Package
     (Name        : Package_Id;
      Description : String;
      Projects    : Projects_Kind := PRD.Everywhere) is
   begin
      if not Exists (Name) then
         Add (Name, Projects);
      end if;
      if Get_Package_Description (Name)'Length = 0
        and then Description'Length > 0
      then
         Set_Package_Description (Name, Description);
      end if;
   end Add_Package;

   ---------------------------
   -- Import_IDE_Attributes --
   ---------------------------

   procedure Import_IDE_Attributes is
   begin
      Add_Package
        (Name        => +"IDE",
         Description => "This package specifies the options used by " &
           "'gnatstudio' IDE.");

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Connection_Tool",
         Description           => "Executable used to interface with a " &
           "remote target when debugging. GNAT Studio currently supports " &
           "OpenOCD, st-util or pyOCD. You can leave this attribute empty " &
           "if you are using a custom tool spawned outside of GNAT Studio'.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Connection_Config_File",
         Description           => "File used to configure the " &
           "IDE'Connection_Tool. Used only when OpenOCD is set.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Program_Host",
         Description           => "Name or IP address of the embedded " &
           "target. This field should be left blank if you are not working " &
           "on an embedded application.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Communication_Protocol",
         Description           => "Protocol used to connect to the embedded " &
           "target. This field should be left blank if you are not working " &
           "on an embedded application.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Compiler_Command",
         Description           => "The command to compile the source files " &
           "for a given language.",
         Index_Type            => PRA.Language_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Xref_Database",
         Description           => "Location of the xref database.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Gnatlist",
         Description           => "The 'gnatls' command used to find where " &
           "the Ada run time files are installed (including optional " &
           "arguments, e.g. gnatls --RTS=sjlj).",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Gnat",
         Description           => "The gnat driver used to run the various " &
           "commands associated with the GNAT toolchain.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Debugger_Command",
         Description           => "The command line to use when debugging " &
           "applications (including optional arguments). Only 'gdb' and its " &
           "variants are currently supported.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"VCS_Kind",
         Description           => "Name of the version control system that " &
           "you are using. When set to 'auto', GNAT Studio will try to " &
           "detect the underlying version control system. Set this " &
           "attribute to 'none' if you want to disable VCS for this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"VCS_Log_Check",
         Description           => "Application run on the log file/revision " &
           "history just before commiting a file. If it returns anything " &
           "other than 0, the commit will not be performed. The only " &
           "parameter to this script is the name of the log file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"VCS_File_Check",
         Description           => "Application run on the source file just " &
           "before commiting a file. If it returns anything other than 0, " &
           "the commit will not be performed. The only parameter to this " &
           "script is the name of the source file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"VCS_Repository_Root",
         Description           => "The repository root path.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"VCS_Patch_Root",
         Description           => "The root directory to use for building " &
           "patch file. The root project directory is used if this value is " &
           "not defined.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"IDE",
         Attribute_Name        => +"Artifacts_Dir",
         Description           => "The directory in which the files " &
           "generated by 'gnatstudio' for this project (cross-references " &
           "database, locations etc.) are stored by default. Defaults to " &
           "Object_Dir if not specified.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_IDE_Attributes;

   -----------------------------------
   -- Import_Ant_Support_Attributes --
   -----------------------------------

   procedure Import_Ant_Support_Attributes is
   begin
      Add_Package
        (Name        => +"Ant",
         Description => "This package specifies the options used when " &
           "calling the 'ant' tool from 'gnatstudio' IDE.");

      Add_Attribute
        (Package_Name          => +"Ant",
         Attribute_Name        => +"Antfile",
         Description           => "Ant build file to use for this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Ant",
         Attribute_Name        => +"Ant",
         Description           => "",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Ant",
         Attribute_Name        => +"Switches",
         Description           => "",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_Ant_Support_Attributes;

   ------------------------------------
   -- Import_Make_Support_Attributes --
   ------------------------------------

   procedure Import_Make_Support_Attributes is
   begin
      Add_Package
        (Name        => +"Make",
         Description => "This package specifies the options used when " &
           "calling the 'make' tool from 'gnatstudio' IDE.");

      Add_Attribute
        (Package_Name          => +"Make",
         Attribute_Name        => +"Makefile",
         Description           => "Makefile to use for this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Make",
         Attribute_Name        => +"Switches",
         Description           => "",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_Make_Support_Attributes;

   -------------------------------
   -- Import_GNATsas_Attributes --
   -------------------------------

   procedure Import_GNATsas_Attributes is
   begin
      Add_Package
        (Name        => +"Analyzer",
         Description => "This package specifies the options used when " &
           "calling the 'gnatsas' tool.");

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Output_Dir",
         Description           => "GNATSAS output directory to use for this " &
           "project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Review_File",
         Description           => "GNATSAS review file to use for this " &
           "project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Message_Patterns",
         Description           => "Alternate MessagePatterns.xml file used " &
           "for this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Additional_Patterns",
         Description           => "Extra MessagePatterns.xml file to use in " &
           "addition to the default patterns file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Excluded_Source_Files",
         Description           => "List of project source file " &
           "(as base names) which should be excluded from GNATSAS's analysis.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Pending_Status",
         Description           => "Custom message review status for the " &
           "'pending' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Not_A_Bug_Status",
         Description           => "Custom message review status for the " &
           "'not a bug' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Analyzer",
         Attribute_Name        => +"Bug_Status",
         Description           => "Custom message review status for the " &
           "'bug' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

   end Import_GNATsas_Attributes;

   ---------------------------------
   -- Import_GNATcheck_Attributes --
   ---------------------------------

   procedure Import_GNATcheck_Attributes is
   begin
      Add_Package
        (Name        => +"Check",
         Description => "This package specifies the options used when " &
           "calling the checking tool 'gnatcheck'. Its attribute " &
           "Default_Switches has the same semantics as for the package " &
           "Builder. The first string should always be -rules to specify " &
           "that all the other options belong to the -rules section of " &
           "the parameters to 'gnatcheck'.");

      Add_Attribute
        (Package_Name          => +"Check",
         Attribute_Name        => +"Default_Switches",
         Description           => "Index is a language name. Value is a " &
           "list of switches to be used when invoking 'gnatcheck' for a " &
           "source of the language, if there is no applicable attribute " &
           "Switches.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Check",
         Attribute_Name        => +"Switches",
         Description           => "Index is a source file name. Value is " &
           "the list of switches to be used when invoking 'gnatcheck' for " &
           "the source.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATcheck_Attributes;

   --------------------------------
   -- Import_Codepeer_Attributes --
   --------------------------------

   procedure Import_Codepeer_Attributes is
   begin
      Add_Package
        (Name        => +"CodePeer",
         Description => "This package specifies the options used when " &
           "calling the tool 'codepeer' or calling 'gnatcheck' with " &
           "'--simple-project' switch.");

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"File_Patterns",
         Description           => "If you want to override ada default file " &
           "extensions (ada, ads, adb, spc & bdy), use this attribute " &
           "which includes a list of file patterns where you can specify " &
           "the following meta characters: * : matches any string of 0 " &
           "or more characters, ? : matches any character, " &
           " [list of chars] : matches any character listed, [char-char] " &
           ": matches any character in given range, [^list of chars] : " &
           "matches any character not listed. These patterns are case " &
           "insensitive.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Output_Directory",
         Description           => "CodePeer output directory to use for " &
           "this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Database_Directory",
         Description           => "CodePeer database directory to use for " &
           "this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Server_URL",
         Description           => "URL used to connect to the CodePeer " &
           "server to access the CodePeer database remotely.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"CWE",
         Description           => "Include CWE ids in output.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => False,
         Default               => False_Default_Value,
         Has_Default_In        => PRA.Everywhere,
         Type_Def              => Boolean_Type_Def);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Message_Patterns",
         Description           => "Alternate MessagePatterns.xml file used " &
           "for this project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Additional_Patterns",
         Description           => "Extra MessagePatterns.xml file to use in " &
           "addition to the default patterns file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Excluded_Source_Files",
         Description           => "List of project source file (as base " &
           "names) which should be excluded from CodePeer's analysis.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Pending_Status",
         Description           => "Custom message review status for the " &
           "'pending' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Not_A_Bug_Status",
         Description           => "Custom message review status for the " &
           "'not a bug' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"CodePeer",
         Attribute_Name        => +"Bug_Status",
         Description           => "Custom message review status for the " &
           "'bug' category.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

   end Import_Codepeer_Attributes;

   -------------------------------
   -- Import_GNATcov_Attributes --
   -------------------------------

   procedure Import_GNATcov_Attributes is
   begin
      Add_Package
        (Name        => +"Coverage",
         Description => "This package specifies the options used when " &
           "calling the 'gnatcov' tool. It has an unique Switches " &
           "attribute that can be indexed.");

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Switches",
         Description           => "This attribute is a simple variable that " &
           "contains a string list value representing the switches to be " &
           "passed to GNATcoverage tool. Each attribute specification " &
           "requires an index indicating what 'gnatcov' operation the " &
           "switches apply to.",
         Index_Type            => PRA.String_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Units",
         Description           => "List of monitored units.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Units_List",
         Description           => "File indicating the monitored units.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Excluded_Units",
         Description           => "List of excluded units.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Excluded_Units_List",
         Description           => "File indicating the excluded units.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Ignored_Units",
         Description           => "List of ignored units.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Ignored_Units_List",
         Description           => "File indicating the ignored units.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Routines",
         Description           => "List of routines.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Routines_List",
         Description           => "File containing the list of routine.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Excluded_Routines",
         Description           => "List of excluded routines.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Coverage",
         Attribute_Name        => +"Excluded_Routines_List",
         Description           => "File containing the excluded routines.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);
   end Import_GNATcov_Attributes;

   --------------------------------
   -- Import_GNATdist_Attributes --
   --------------------------------

   procedure Import_GNATdist_Attributes is
   begin
      Add_Package
        (Name        => +"DSA",
         Description => "This package specifies the options used when " &
           "calling the tool 'gnatdist' or 'po_gnatdist'.");

      Add_Attribute
        (Package_Name          => +"DSA",
         Attribute_Name        => +"Configuration_File",
         Description           => "DSA configuration file to use for this " &
           "project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATdist_Attributes;

   -------------------------------
   -- Import_GNATdoc_Attributes --
   -------------------------------

   procedure Import_GNATdoc_Attributes is
   begin
      Add_Package
        (Name        => +"Documentation",
         Description => "This package specifies the options used when " &
           "calling the tool 'gnatdoc'.");

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Output_Dir",
         Description           => "In which subdirectory to lookup " &
           "alternative resources for doc generation.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Doc_Pattern",
         Description           => "Regular expression identifying " &
           "documentation comments.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Documentation_Dir",
         Description           => "The documentation is generated by " &
           "default into a directory called gnatdoc, created under the " &
           "object directory of the root project.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Ignored_Subprojects",
         Description           => "By default GNATdoc recursively processes " &
           "all the projects on which your root project depends.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Image_Dir",
         Description           => "The directory containing images is " &
           "specified by the string attribute Image_Dir of the " &
           "Documentation package.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"HTML_Custom_Dir",
         Description           => "GNATdoc uses a set of static resources " &
           "and templates files to control the final rendering. Modifying " &
           "these static resources and templates you can control the " &
           "rendering of the generated documentation. The files used for " &
           "generating the documentation can be found under " &
           "'<install_dir>/share/gnatstudio/gnatdoc/html'. If you need a " &
           "different layout from the proposed one, you can override those " &
           "files and provides new files.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Documentation",
         Attribute_Name        => +"Custom_Tags_Definition",
         Description           => "Set of tags processed by GNATdoc can be " &
           "extended by providing custom tags handlers.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATdoc_Attributes;

   ------------------------------------
   -- Import_GNATemulator_Attributes --
   ------------------------------------

   procedure Import_GNATemulator_Attributes is
   begin
      Add_Package
        (Name        => +"Emulator",
         Description => "This package specifies the options used when " &
           "calling the GNATemulator '<target>-gnatemu' tool.");

      Add_Attribute
        (Package_Name          => +"Emulator",
         Attribute_Name        => +"Debug_Port",
         Description           => "Port used by GNATemulator to debug",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Emulator",
         Attribute_Name        => +"Board",
         Description           => "If GNATemulator provides multiple " &
           "emulations for the target platform, use this option to select a " &
           "specific board. Use `gnatemu --help` to get the list of boards.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Emulator",
         Attribute_Name        => +"Switches",
         Description           => "A list of switches processed before the " &
           "command line switches",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

   end Import_GNATemulator_Attributes;

   --------------------------------
   -- Import_GNATtest_Attributes --
   --------------------------------

   procedure Import_GNATtest_Attributes is
   begin
      Add_Package
        (Name        => +"GNATtest",
         Description => "This package specifies the options used when " &
           "calling the 'gnattest' tool.");

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Harness_Dir",
         Description           => "Used to specify the directory in which " &
           "to place harness packages and project file for the test driver. " &
           "If it's a relative path, it is considered relative to the " &
           "object directory of the project file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Tests_Dir",
         Description           => "All test packages are placed in this " &
           "directory. If it's a relative path, it is considered relative " &
           "to the object directory of the project file. When all sources " &
           "from all projects are taken recursively from all projects, " &
           "directories with the given relative path are created for each " &
           "project in their object directories and test packages are " &
           "placed accordingly.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Tests_Root",
         Description           => "Test files are put in a same directory " &
           "hierarchy as the sources with this directory as the root " &
           "directory. If it's a relative path, it is considered relative " &
           "to the object directory of the project file. When projects are " &
           "considered recursively, directory hierarchies of tested sources " &
           "are recreated for each project in their object directories and " &
           "test packages are placed accordingly.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Subdir",
         Description           => "Test packages are placed in a " &
           "subdirectory of the corresponding source directory, with the " &
           "specified name. Thus, each set of unit tests are placed in a " &
           "subdirectory of the code under test. If the sources are in " &
           "separate directories, each source directory will have a test " &
           "subdirectory",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Stubs_dir",
         Description           => "The hierarchy of directories containing " &
           "stubbed units is recreated in the specified directory, with " &
           "stubs placed in directories corresponding to projects they are " &
           "derived from. If it's a relative path, it is considered " &
           "relative to the object directory of the project file. When " &
           "projects are considered recursively, directory hierarchies of " &
           "stubs are recreated for each project in their object " &
           "directories and test packages are placed accordingly.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Additional_Tests",
         Description           => "Sources described in given project are " &
           "considered potential additional manual tests to be added to the " &
           "test suite.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Skeletons_Default",
         Description           => "Default behavior of generated skeletons.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => False,
         Default               => Fail_Default_Value,
         Has_Default_In        => PRA.Everywhere,
         Type_Def              => Pass_Fail_Type_Def);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"GNATtest_Switches",
         Description           => "Custom switches for 'gnattest'",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"GNATtest_Mapping_File",
         Description           => "Map tests to subprograms.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Default_Stub_Exclusion_List",
         Description           => "Used to specify the file with list of " &
           "units whose bodies should not be stubbed, otherwise specified " &
           "by '--exclude-from-stubbing=filename'.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"GNATtest",
         Attribute_Name        => +"Stub_Exclusion_Listt",
         Description           => "Used to specify the file with list of " &
           "units whose bodies should not be stubbed when testing 'unit'.",
         Index_Type            => PRA.Unit_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

   end Import_GNATtest_Attributes;

   ------------------------------
   -- Import_GNATpp_Attributes --
   ------------------------------

   procedure Import_GNATpp_Attributes is
   begin
      Add_Package
        (Name        => +"Pretty_Printer",
         Description => "This package specifies the options used when " &
           "calling the 'gnatpp' tool. It has an unique Default_Switches " &
           "attribute that should be indexed using ""Ada"".");

      Add_Attribute
        (Package_Name          => +"Pretty_Printer",
         Attribute_Name        => +"Default_Switches",
         Description           => "This attribute is a simple variable that " &
           "contains a string list value representing the switches to be " &
           "passed to 'gnatpp' tool. It requires the ""Ada"" index",
         Index_Type            => PRA.Language_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Pretty_Printer",
         Attribute_Name        => +"Switches",
         Description           => "Index is a source file name. Value is " &
           "the list of switches to be used when invoking gnatpp for the " &
           "source.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATpp_Attributes;

   ---------------------------------
   -- Import_GNATprove_Attributes --
   ---------------------------------

   procedure Import_GNATprove_Attributes is
   begin
      Add_Package
        (Name        => +"Prove",
         Description => "This package specifies the options used when " &
           "calling 'gnatprove' tool.");

      Add_Attribute
        (Package_Name          => +"Prove",
         Attribute_Name        => +"Proof_Switches",
         Description           => "Defines additional command line switches " &
           "that are used for the invokation of GNATprove. Only the " &
           "following switches are allowed for file-specific switches: " &
           "'--steps', '--timeout', '--memlimit', '--proof', '--prover', " &
           "'--level', '--mode', '--counterexamples', '--no-inlining', " &
           "'--no-loop-unrolling'",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Prove",
         Attribute_Name        => +"Switches",
         Description           => "This deprecated attribute is the same as " &
           "Proof_Switches (""Ada"").",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Prove",
         Attribute_Name        => +"Proof_Dir",
         Description           => "Defines the directory where are stored " &
           "the files concerning the state of the proof of a project. This " &
           "directory contains a sub-directory sessions with one " &
           "directory per source package analyzed for proof. Each of " &
           "these package directories contains a Why3 session file. If a " &
           "manual prover is used to prove some VCs, then a " &
           "sub-directory called by the name of the prover is created " &
           "next to sessions, with the same organization of " &
           "sub-directories. Each of these package directories contains " &
           "manual proof files. Common proof files to be used across " &
           "various proofs can be stored at the toplevel of the " &
           "prover-specific directory.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATprove_Attributes;

   ----------------------------
   -- Import_QGen_Attributes --
   ----------------------------

   procedure Import_QGen_Attributes is
   begin
      Add_Package
        (Name        => +"QGen",
         Description => "This package specifies the options used when " &
           "calling the QGen tool.");

      Add_Attribute
        (Package_Name          => +"QGen",
         Attribute_Name        => +"Output_Dir",
         Description           => "The default location for QGen generated " &
           "file.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"QGen",
         Attribute_Name        => +"Target",
         Description           => "",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"QGen",
         Attribute_Name        => +"Debug_Args",
         Description           => "Arguments to supply to gdb `run` when " &
           "starting the QGen debugger",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_QGen_Attributes;

   ---------------------------------
   -- Import_GNATstack_Attributes --
   ---------------------------------

   procedure Import_GNATstack_Attributes is
   begin
      Add_Package
        (Name        => +"Stack",
         Description => "This package specifies the options used when " &
           "calling the 'gnatstack' tool. It has an unique Switches " &
           "attribute.");

      Add_Attribute
        (Package_Name          => +"Stack",
         Attribute_Name        => +"Switches",
         Description           => "This attribute is a simple variable that " &
           "contains a string list value representing the switches to be " &
           "passed to 'gnatstack' tool.",
         Index_Type            => PRA.No_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

   end Import_GNATstack_Attributes;

   --------------------------------
   -- Import_GNATstub_Attributes --
   --------------------------------

   procedure Import_GNATstub_Attributes is
   begin
      Add_Package
        (Name        => +"Stub",
         Description => "This package specifies the options used when " &
           "calling the 'gnatstub' tool. It has an unique Default_Switches " &
           "attribute that should be indexed using ""Ada"".");

      Add_Attribute
        (Package_Name          => +"Stub",
         Attribute_Name        => +"Default_Switches",
         Description           => "This attribute is a simple variable that " &
           "contains a string list value representing the switches to be " &
           "passed to 'gnatstub' tool. It requires the ""Ada"" index",
         Index_Type            => PRA.Language_Index,
         Value                 => List,
         Value_Case_Sensitive  => True);

      Add_Attribute
        (Package_Name          => +"Stub",
         Attribute_Name        => +"Switches",
         Description           => "Index is a source file name. Value is " &
           "the list of switches to be used when invoking gnatstub for the " &
           "source.",
         Index_Type            => PRA.No_Index,
         Value                 => Single,
         Value_Case_Sensitive  => True);

   end Import_GNATstub_Attributes;

   ------------------------------------------
   -- Initialize_Extra_Packages_Attributes --
   ------------------------------------------

   procedure Initialize_Extra_Packages_Attributes is
   begin

      --  packages/attributes for AdaCore IDE
      Import_IDE_Attributes;
      Import_Ant_Support_Attributes;
      Import_Make_Support_Attributes;

      --  package for extra AdaCore tools

      Import_GNATsas_Attributes;
      Import_GNATcheck_Attributes;
      Import_Codepeer_Attributes;
      Import_GNATcov_Attributes;
      Import_GNATdist_Attributes;
      Import_GNATdoc_Attributes;
      Import_GNATemulator_Attributes;
      Import_GNATtest_Attributes;
      Import_GNATpp_Attributes;
      Import_GNATprove_Attributes;
      Import_QGen_Attributes;
      Import_GNATstack_Attributes;
      Import_GNATstub_Attributes;

   end Initialize_Extra_Packages_Attributes;

end LSP.GPR_External_Tools;
