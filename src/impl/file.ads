-------------------------------------------------------------------------------
--
--  Lib_Mold - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Definitions;
with Lib_Mold;

package File is

   package Mold renames Lib_Mold;

   procedure Set_Running_Directory (Name : String);
   --  Set the directory from which the process has been invoked when
   --  operating on directory trees. This directory is used to search for
   --  include files when an include file is not found in the current working
   --  directory of the file being processed.

   --!pp off
   function Replace
   (
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Definitions.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Results    :          Mold.Results_Access := null
   )
   return Natural; -- *TODO - Consider returning a Boolean
   --!pp on
   --
   --  In the given Source file name, replace all occurrences of mold
   --  variables with the value defined in the Variables map. Behaves
   --  according to the Settings given and updates the Results object.
   --
   --  Parameter source is the name of file with extension "mold", e.g.
   --  "README.md.mold". The name of the generated file is the same without
   --  the "mold" extension, "README.md", even when there are no variables to
   --  replace.
   --
   --  Return the number of errors detected.  *TODO - review
   --  ------------------------------------------------------------------------

end File;
