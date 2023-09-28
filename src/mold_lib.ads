-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

with Custom_Text_Filters;
with Mold_Lib_Config;

package Mold_Lib is

   package Log renames Simple_Logging;

   subtype Filters_Array is Custom_Text_Filters.Filters_Array;
   subtype Filters_Access is Custom_Text_Filters.Filters_Access;

   type Undefined_Alerts is (None, Warning, Error);
   type Undefined_Variable_Actions is (Ignore, Empty);

   Optional_Replacement_Prefix  : constant Character := '?';
   Mandatory_Replacement_Prefix : constant Character := '#';
   Mold_File_Extension          : constant String    := "mold";
   Inclusion_Prefix             : constant String    := "include:";
   Include_File_Extension       : constant String    := "molt";
   Defined_Setting_Prefix       : constant String    := "mold-";

   type Settings_Type is record
      Replacement_In_File_Names   : aliased Boolean;
      Delete_Source_Files         : aliased Boolean;
      Overwrite_Destination_Files : aliased Boolean;
      Enable_Defined_Settings     : aliased Boolean;
      Undefined_Variable_Action   : aliased Undefined_Variable_Actions;
      Undefined_Variable_Alert    : aliased Undefined_Alerts;
      Undefined_Filter_Alert      : aliased Undefined_Alerts;
      Abort_On_Error              : aliased Boolean;
   end record;
   type Settings_Access is access all Settings_Type;

   type Text_Filter is access function (S : String) return String;
   type Filter_Array is array (0 .. 9) of Text_Filter;
   type Filter_Array_Access is access Filter_Array;

   --!pp off
   Default_Settings : aliased Settings_Type :=
   (
      Replacement_In_File_Names   => True,
      Delete_Source_Files         => True,
      Overwrite_Destination_Files => False,
      Enable_Defined_Settings     => True,
      Undefined_Variable_Action   => Ignore,
      Undefined_Variable_Alert    => Error,
      Undefined_Filter_Alert      => Warning,
      Abort_On_Error              => True
   );
   --!pp on

   --!pp off
   type Results_Fields is
   (
      Files_Processed,
      Files_Renamed,
      Files_Overwritten,
      Variables_Defined,
      Variables_Found,
      Variables_Undefined,
      Variables_Replaced,
      Variables_Ignored,
      Variables_Emptied,
      Filters_Found,
      Filters_Applied,
      Replacement_Warnings,
      Replacement_Errors
   );
   --!pp on

   type Results_Type is array (Results_Fields) of Natural;
   type Results_Access is access all Results_Type;

   function Name return String is (Mold_Lib_Config.Crate_Name);
   --  Return create name.

   function Version return String is (Mold_Lib_Config.Crate_Version);
   --  Return crate version.

   --!pp off
   function Apply
   (
      Source      : String          := ".";
      Output_Dir  : String          := "";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Filters     : Filters_Access  := null;
      Results     : Results_Access  := null;
      Log_Level   : Log.Levels      := Log.Info
   )
   return Natural;
   --!pp on
   --
   --  For a complete description of Mold, please visit:
   --
   --                    https://rocher.github.io/mold
   --
   --  Given Source, a file or directory, a Definition file with a set of
   --  variables defined in it, this function applies variable replacement
   --  and template inclusion in Source file. Or, recursively, in all '.mold'
   --  files in the current directory and subdirectories when Source is a
   --  directory. Optionally, Output_Dir can specify a different output
   --  directory, Settings can be a customized set  of settings other than
   --  Default_Settings, and Results can be a pointer to a Results_Type object
   --  if detailed information about the process is required.
   --
   --  Return the number of errors detected, including those detected during
   --  the replacement process. If Abort_On_Error is False, the number of
   --  errors can be arbitrarily big.
   --  ------------------------------------------------------------------------

end Mold_Lib;
