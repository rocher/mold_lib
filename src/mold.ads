-------------------------------------------------------------------------------
--
--  MOLD - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package Mold is

   type Variable_Level is (Optional, Mandatory);
   type Undefined_Variable_Action is (Ignore, Empty);
   type Undefined_Variable_Alert is (None, Warning);

   Mandatory_Variable_Prefix : constant Character := '#';

   --  Examples with defined variable foo="bar"; variable baz is undefined:
   --
   --  Level      Result   Alert    Origin      Replaced   Error Type
   --  -----------------------------------------------------------------------
   --  Optional   <any>    <any>    "{{foo}}"   "bar"      None

   --  Optional   Ignore   None     "{{baz}}"   "{{baz}}"  None
   --  Optional   Ignore   Warning  "{{baz}}"   "{{baz}}"  Warning <-- DEFAULT
   --
   --  Optional   Empty    None     "{{baz}}"   ""         None
   --  Optional   Empty    Warning  "{{baz}}"   ""         Warning
   --
   --  Mandatory  <any>    <any>    "{{#foo}}"  "bar"      None
   --  Mandatory  <any>    <any>    "{{#baz}}"  <none>     Error

   function Run
   --!pp off
   (
      Repository  : String;
      Destination : String;
      Vars_File   : String                    := "mold.toml";
      Branch      : String                    := "main";
      Action      : Undefined_Variable_Action := Ignore;
      Alert       : Undefined_Variable_Alert  := Warning;
      Log_Level   : Simple_Logging.Levels     := Simple_Logging.Info
   )
   --!pp on
   return Natural;
   --  If the Destination directory does not exists, clone the Branch of the
   --  Repository into the Destination directory; then perform the variable
   --  replacements specified in the TOML Vars_File, in all "*.mold" files of
   --  the Destination directory. For undefined variables, the behavior is
   --  specified with Action and Alert level (default is Ignore and Warning).
   --
   --  Parameters:
   --
   --    * Repository: URL of a valid and public repository
   --            e.g.: "git@github.com:alice-adventures/Alice.git"
   --            If Repository = "", then Destination must exists
   --
   --    * Destination: Valid directory PATH (Use Ada.Directories if needed)
   --             e.g.: "alice"
   --
   --    * Vars_File: Valid filename PATH to an existing TOML file
   --           e.g.: "./subs.toml"
   --
   --    * Branch: Valid branch of the Repository
   --        e.g.: "main"
   --
   --    * Action: Action to perform when a variable has not been defined
   --
   --    * Alert: Alert level when an optional variable is undefined
   --
   --    * Log_Level: Log level used by the subprograms of libmold
   --
   --  Format of Vars_File is a "plain" TOML (no arrays), e.g.:
   --
   --    foo = "bar"
   --    baz = "zoo"
   --
   --  In case or Error, the execution of the function is aborted and no files
   --  already modified are restored: it is the responsibility of the user to
   --  restore (partially) modified files with "git restore ." from the
   --  command line.
   --
   --  Possible errors:
   --
   --     * Git repository not found
   --     * Invalid Branch
   --     * Destination directory cannot be created
   --     * Vars_File does not exists
   --     * Invalid or empty Vars_File
   --     * Undefined Mandatory variable
   --     * Destination directory has no write permissions
   --
   --  Return the number of replaced files.

end Mold;
