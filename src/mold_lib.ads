-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------
--!pp off
--
--  Please visit  https://rocher.github.io/mold  for a complete reference.
--

with Text_Filters_Custom;
with Simple_Logging;

package Mold_Lib is

   package Log renames Simple_Logging;

   type On_Undefined_Handling is (Ignore, Warning, Error);
   --
   --  Specifies how to handle undefined variables, undefined text filters or
   --  invalid date formats encountered during the variable substitution
   --  process. 'Ignore' means that nothing happens and the variable remains
   --  the same (e.g. '{{My_Var}}'); 'Warning' issues a warning and replaces
   --  the variable name with the empty string; 'Error' emits an error and
   --  stops the substitution process. Default is 'Error'.

   type Settings_Type is record
      Replacement_In_Filenames    : aliased Boolean;
      Replacement_In_Variables    : aliased Boolean;
      Delete_Source_Files         : aliased Boolean;
      Overwrite_Destination_Files : aliased Boolean;
      Enable_Defined_Settings     : aliased Boolean;
      On_Undefined                : aliased On_Undefined_Handling;
   end record;
   type Settings_Access is access all Settings_Type;
   --  Settings to configure the behavior of mold. Refer to the documentation
   --  for more information.

   Default_Settings : constant Settings_Type := (
      Replacement_In_Filenames    => True,
      Replacement_In_Variables    => True,
      Delete_Source_Files         => False,
      Overwrite_Destination_Files => True,
      Enable_Defined_Settings     => True,
      On_Undefined                => Error
   );

   type Results_Fields is (
      Files_Processed,
      Files_Renamed,
      Files_Overwritten,
      Files_Deleted,
      Variables_Defined,
      Variables_Found,
      Variables_Undefined,
      Variables_Replaced,
      Variables_Ignored,
      Variables_Emptied,
      Warnings
   );
   type Results_Type   is array (Results_Fields) of Natural;
   type Results_Access is access all Results_Type;
   --  Set of results returned by mold, when requested.

   subtype Filters_Array  is Text_Filters_Custom.Filters_Array;
   subtype Filters_Access is Text_Filters_Custom.Filters_Access;
   --
   --  Custom text filters are pointers to functions with the following
   --  specification,
   --
   --     function (S : String) return String;
   --
   --  that can be applied during variable substitution to provide additional
   --  text transformations. There are several predefined text filters
   --  covering a wide range of use cases. In case you need to define your
   --  custom text filter, you can provide up to ten functions with the above
   --  specification. The type Filter_Access is a pointer to an array of ten
   --  (0 .. 9) pointers to functions.

   function Name return String;

   function Version return String;

   function Apply (
      Source     : String          := ".";
      Output_Dir : String          := "";
      Toml_File  : String          := "mold.toml";
      Settings   : Settings_Access := null;
      Filters    : Filters_Access  := null;
      Results    : Results_Access  := null;
      Log_Level  : Log.Levels      := Log.Info
   )  return Boolean;
   --  Given Source, a file or directory, a TOML file with a set of variables
   --  defined in it, this function applies variable replacement and template
   --  inclusion in Source file. Or, recursively, in all '.mold' files in the
   --  current directory and subdirectories when Source is a directory.
   --  Optionally, Output_Dir can specify a different output directory,
   --  Settings can be a customized set  of settings other than
   --  Default_Settings, and Results can be a pointer to a Results_Type object
   --  if detailed information about the process is required.
   --
   --  Return True if the process ends successfully (no errors detected).

end Mold_Lib;
