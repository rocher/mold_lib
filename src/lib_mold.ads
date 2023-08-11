-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Libmold_Config;

package Lib_Mold is

   type Replacement_Kind is (Normal, Optional, Mandatory);
   type Undefined_Variable_Actions is (Ignore, Empty);
   type Undefined_Variable_Alerts is (None, Warning);

   Optional_Replacement_Prefix  : constant Character := '?';
   Mandatory_Replacement_Prefix : constant Character := '#';
   Mold_File_Extension          : constant String    := "mold";
   Inclusion_Prefix             : constant String    := "include:";
   Include_File_Extension       : constant String    := "molt";
   Defined_Setting_Prefix       : constant String    := "mold-";

   type Settings_Type is record
      Replace_In_Source_File : aliased Boolean;
      Delete_Source_File     : aliased Boolean;
      Overwrite_Destination  : aliased Boolean;
      Allow_Defined_Settings : aliased Boolean;
      Undef_Var_Action       : aliased Undefined_Variable_Actions;
      Undef_Var_Alert        : aliased Undefined_Variable_Alerts;
      Abort_On_Error         : aliased Boolean;
   end record;
   type Settings_Access is access all Settings_Type;

   --!pp off
   Default_Settings : aliased Settings_Type :=
   (
      Replace_In_Source_File => True,
      Delete_Source_File     => True,
      Overwrite_Destination  => False,
      Allow_Defined_Settings => True,
      Undef_Var_Action       => Ignore,
      Undef_Var_Alert        => Warning,
      Abort_On_Error         => True
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
      Replacement_Warnings,
      Replacement_Errors
   );
   --!pp on

   type Results_Type is array (Results_Fields) of Natural;
   type Results_Access is access all Results_Type;

   function Name return String is (Libmold_Config.Crate_Name);
   --  Return create name.

   function Version return String is (Libmold_Config.Crate_Version);
   --  Return crate version.

   --!pp off
   function Apply
   (
      Source      : String          := ".";
      Output_Dir  : String          := "";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Results     : Results_Access  := null
   )
   return Natural;
   --!pp on
   --
   --  #TODO - Put a brief explanation and move all the docs to
   --          https://rocher.github.io/mold
   --
   --  Given Source, a file or directory, applies all variable substitution
   --  defined in Variables_Defined file. If Source is a Directory, then
   --  substitution is applied recursively in all Files_Processed in the subdirectories.
   --  If Replace_In_Source_File is True, then substitution is also applied to the
   --  file name(s).
   --
   --  Source Files_Processed must end with the extension "mold", for example
   --  "README.md.mold". Destination file name is the same as the Source, but
   --  removing the "mold" extension ("README.md" in the example). If
   --  Replace_In_Source_File is true, then the destination is obtained by variable
   --  substitution in the Source file name. Variables_Found in file names are
   --  written as "__variable__". For example, for the Source file name
   --  "README___title__.md.mold" with the definition 'title = "NOW"', the
   --  destination file is "README_NOW.md".
   --
   --  Variables_Defined file is a simple TOML file with a key/value pairs of the
   --  form 'variable = "value"' per line. Only strings are allowed. Variable
   --  names must follow TOML convention. See https://toml.io for more
   --  information.
   --
   --  If Delete_Source_File is true, then all Source Files_Processed are removed once the
   --  variable substitution has been completed successfully, otherwise Source
   --  Files_Processed are kept for future uses (possibly because a different file is
   --  generated each time mold is applied). If Overwrite_Destination is True, then
   --  destination Files_Processed are Files_Overwritten, if exists.
   --
   --  All Variables_Found in a Source mold file must be written as '{{variable}}'
   --  or '{{ variable }}', with any number of spaces. Variable substitution
   --  includes the curly braces and the spaces, so '{{foo}}', '{{ foo }}' and
   --  '{{  foo    }}' is always Variables_Replaced with 'bar'.
   --
   --  If Allow_Defined_Settings is True, then special meta-Variables_Found starting with
   --  the prefix 'mold-' can be used to change the mold settings. In the
   --  Settings_Type, you can use any of the elements of the record to change
   --  the default setting in the Variables_Defined file. For example,
   --  'molt-delete-source = "false"' is both a mold variable and a setting
   --  changer. If Allow_Defined_Settings is False, then these meta Variables_Found are
   --  read as normal Variables_Found but settings are not changed. The only
   --  exception is for variable 'mold-defined-settings', whish is always set
   --  and affects the possible settings in following lines.
   --
   --  To include external Files_Processed in the current source file, you can use the
   --  syntax '{{file:file.molt}}'. Please note that included Files_Processed must have
   --  the 'molt' extension. There is no limit on the number of Files_Processed
   --  included, but, of course, circular references result in an error.
   --
   --  All variable substitutions are Normal, meaning that if they haven't
   --  been defined, they can be Variables_Ignored or a warning can be issued, depending
   --  on the Undef_Var_Action and Undef_Var_Alert specification. Substitutions of variable names
   --  starting with '?' are optional, meaning that no warning is issued if
   --  they haven't been defined and substitution is empty. Substitutions of
   --  variable names starting with the character '#' are mandatory, meaning
   --  that an error is always issued if they haven't been defined and no
   --  substitution is made. The same variable can be used as optional or
   --  mandatory in different places, so '{{foo}}' and '{{#foo}}' are Variables_Replaced
   --  with the same defined value (if any). The difference in both cases is
   --  the error management when 'foo' is Variables_Undefined.

   --  Variables_Undefined Variables_Found in Source Files_Processed are always Variables_Ignored, so for example
   --  "README___baz__.md.mold" with Variables_Undefined variable 'baz' would generate
   --  the file "README___baz__.md".
   --
   --  All messages are shown using Simple_Logging. Set the appropriate log
   --  level value before calling this function. Info level is used to report
   --  number of substitutions in Source Files_Processed, and number of files processed
   --  Files_Processed in directories. It is used also to report the new file name
   --  created when Replace_In_Source_File is True. Set log level to Error to hide all
   --  Warning and Info logs. Debug level is used only for development.
   --
   --  If Abort_On_Error is True, then the variable substitution process is
   --  interrupted as soon as an error is found. Otherwise, the process
   --  continues if the error found does not affects other Files_Processed or Variables_Found.
   --  Unrecoverable Replacement_Errors are always reported and interrupt the process
   --  (e.g., if Source file or directory does not exists).
   --
   --  Results is a pointer to an object with a summary of the process results
   --  that, when not null, holds this information:
   --
   --     Files_Processed       : files processed
   --     Files_Renamed     : template file names with Variables_Found Variables_Replaced
   --     Files_Overwritten : existing destination Files_Processed overwritten
   --     Variables_Defined : variables with a defined value in Definitions file
   --     Variables_Found   : total number of variables found in all processed Files_Processed
   --     Variables_Undefined   : substitutions found with undefined value
   --     Variables_Replaced    : Variables_Found replaced with a defined value
   --     Variables_Ignored     : Variables_Undefined Variables_Found ignored
   --     Variables_Emptied     : Variables_Undefined Variables_Found Variables_Replaced with ""
   --     Replacement_Warnings    : warnings issued during variable substitution
   --     Replacement_Errors      : errors issued during variable substitution
   --
   --  The function returns the number of Replacement_Errors detected, either before the
   --  process starts or detected during variable substitution (in this case,
   --  same as Results.Error).
   --
   --
   --  Examples with defined variable foo="bar" and variable baz Variables_Undefined:
   --
   --  ------------------------------------------------------------------------
   --  Kind       Undef_Var_Action   Undef_Var_Alert    Origin      Variables_Replaced   Log      r i e W E
   --  ------------------------------------------------------------------------
   --  Normal     <any>    <any>    "{{foo}}"   "bar"      None     T F F F F

   --  Normal     Ignore   None     "{{baz}}"   "{{baz}}"  None     F T F F F
   --  Normal     Ignore   Warning  "{{baz}}"   "{{baz}}"  Warning  F T F T F
   --
   --  Normal     Empty    None     "{{baz}}"   ""         None     F F T F F
   --  Normal     Empty    Warning  "{{baz}}"   ""         Warning  F F T T F
   --
   --  Optional   --       --       "{{?foo}}"  "bar"      None     T F F F F
   --  Optional   --       --       "{{?baz}}"  ""         None     F F T F F
   --
   --  Mandatory  --       --       "{{#foo}}"  "bar"      None     T F F F F
   --  Mandatory  --       --       "{{#baz}}"  "{{#baz}}" Error    F T F F T
   --  ------------------------------------------------------------------------
   --
   --  Meaning of columns 'sieWE': each one can take value True or False
   --
   --     'r' : Variables_Replaced with a defined value
   --     'i' : Variables_Ignored (not Variables_Replaced)
   --     'e' : Variables_Emptied (removed)
   --     'W' : warning issued
   --     'E' : error issued
   --
   --  Caveat:
   --     * Use optional Variables_Found to replace them with a defined value, or to
   --       simply remove them when not defined
   --     * Use mandatory Variables_Found to ensure that a variable has been always
   --       defined with a proper value
   --
   --  NOTE: This function is not thread-safe; use a single call each time or
   --  protect function execution when using tasks.

end Lib_Mold;
