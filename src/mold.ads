-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold is

   type Substitution_Kind is (Normal, Optional, Mandatory);
   type Undef_Var_Action is (Ignore, Empty);
   type Undef_Var_Alert is (None, Warning);

   Optional_Substitution_Prefix  : constant Character := '?';
   Mandatory_Substitution_Prefix : constant Character := '#';
   Mold_File_Extension           : constant String    := "mold";

   type Settings_Type is record
      Source_Template : aliased Boolean;
      Delete_Source   : aliased Boolean;
      Overwrite       : aliased Boolean;
      Action          : aliased Undef_Var_Action;
      Alert           : aliased Undef_Var_Alert;
      Abort_On_Error  : aliased Boolean;
   end record;
   type Settings_Access is access all Settings_Type;

   Default_Settings : aliased Settings_Type :=
   --!pp off
   (
      Source_Template => True,
      Delete_Source   => True,
      Overwrite       => False,
      Action          => Ignore,
      Alert           => Warning,
      Abort_On_Error  => True
   );
   --!pp on

   type Field_Type is
   --!pp off
   (
      Files,
      Renamed,
      Overwritten,
      Variables,
      Defined,
      Undefined,
      Substituted,
      Ignored,
      Emptied,
      Warnings,
      Errors
   );
   --!pp on

   type Results_Type is array (Field_Type) of Natural;
   type Results_Access is access all Results_Type;

   function Apply
   --!pp off
   (
      Source      : String          := ".";
      Definitions : String          := "mold.toml";
      Settings    : Settings_Access := null;
      Results     : Results_Access  := null
   )
   --!pp on

      return Natural;
   --
   --  Given Source, a file or directory, applies all variable substitution
   --  defined in Definitions file. If Source is a Directory, then
   --  substitution is applied recursively in all files in the subdirectories.
   --  If Source_Template is True, then substitution is also applied to the
   --  file name(s).
   --
   --  Source files must end with the extension "mold", for example
   --  "README.md.mold". Destination file name is the same as the Source, but
   --  removing the "mold" extension ("README.md" in the example). If
   --  Source_Template is true, then the destination is obtained by variable
   --  substitution in the Source file name. Variables in file names are
   --  written as "__variable__". For example, for the Source file name
   --  "README___title__.md.mold" with the definition 'title = "NOW"', the
   --  destination file is "README_NOW.md".
   --
   --  Definitions file is a simple TOML file with a key/value pairs of the
   --  form 'variable = "value"' per line. Only strings are allowed. Variable
   --  names must follow TOML convention. See https://toml.io for more
   --  information.
   --
   --  If Delete_Source is true, then all Source files are removed once the
   --  variable substitution has been completed successfully, otherwise Source
   --  files are kept for future uses (possibly because a different file is
   --  generated each time mold is applied). If Overwrite is True, then
   --  destination files are overwritten, if exists.
   --
   --  All variables in a Source mold file must be written as '{{variable}}'
   --  or '{{ variable }}', with any number of spaces. Variable substitution
   --  includes the curly braces and the spaces, so '{{foo}}', '{{ foo }}' and
   --  '{{  foo    }}' is always substituted with 'bar'.
   --
   --  All variable substitution are Normal, meaning that if they haven't been
   --  defined, they can be ignored or a warning can be issued, depending on
   --  the Action and Alert specification. Substitutions of variable names
   --  starting with '?' are optional, meaning that no warning is issued if
   --  they haven't been defined and substitution is empty. Substitutions of
   --  variable names starting with the character '#' are mandatory, meaning
   --  that an error is always issued if they haven't been defined and no
   --  substitution is made. The same variable can be used as optional or
   --  mandatory in different places, so '{{foo}}' and '{{#foo}}' are replaced
   --  with the same defined value (if any). The difference in both cases is
   --  the treatment if 'foo' is undefined.

   --  Undefined variables in Source files are always ignored, so for example
   --  "README___baz__.md.mold" with undefined variable 'baz' would generate
   --  the file "README___baz__.md".
   --
   --  All messages are shown using Simple_Logging. Set the appropriate log
   --  level value before calling this function. Info level is used to report
   --  number of substitutions in Source files, and number of files processed
   --  files in directories. It is used also to report the new file name
   --  created when Source_Template is True. Set log level to Error to hide
   --  all Warning and Info logs. Debug level is used only for development.
   --
   --  If Abort_On_Error is True, then the variable substitution process is
   --  interrupted as soon as an error is found. Otherwise, the process
   --  continues if the error found does not affects other files or variables.
   --  Unrecoverable errors are always reported and interrupt the process
   --  (e.g., if Source file or directory does not exists).
   --
   --  Results is a pointer to an object with a summary of the process
   --  results that, when not null, holds this information:
   --
   --     Files       : files processed
   --     Renamed     : template file names with variables replaced
   --     Overwritten : existing destination files overwritten
   --     Variables   : total number of variables found in all processed files
   --     Defined     : variables with a defined value in Definitions file
   --     Undefined   : substitutions found with undefined value
   --     Replaced    : variables replaced with a defined value
   --     Ignored     : undefined variables ignored
   --     Emptied     : undefined variables replaced with ""
   --     Warnings    : warnings issued during variable substitution
   --     Errors      : errors issued during variable substitution
   --
   --  The function returns the number of Errors detected, either before the
   --  process starts or detected during variable substitution (in this case,
   --  same as Results.Error).
   --
   --
   --  Examples with defined variable foo="bar" and variable baz undefined:
   --
   --  ------------------------------------------------------------------------
   --  Kind       Action   Alert    Origin      Replaced   Log      r i e W E
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
   --     'r' : replaced with a defined value
   --     'i' : ignored (not replaced)
   --     'e' : emptied (removed)
   --     'W' : warning issued
   --     'E' : error issued
   --
   --  Caveat:
   --     * Use optional variables to replace them with a defined value, or to
   --       simply remove them when not defined
   --     * Use mandatory variables to ensure that a variable has been always
   --       defined with a proper value
   --
   --  NOTE: This function is not thread-safe; use a single call each time or
   --  protect function execution when using tasks.

end Mold;
