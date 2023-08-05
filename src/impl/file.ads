-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Mold;
with Replace;

package File is

   procedure Set_Root_Directory (Name : String);
   --  Set the directory from which the process has been invoked when
   --  operating on directory trees. This directory is used to search include
   --  files if not found in the current working (sub-)directory.

   --!pp off
   function Replace
   (
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Replace.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Results    :          Mold.Results_Access := null
   )
   return Natural;
   --!pp on
   --
   --  Replace all occurrences of "{{variable}}" with the value defined in
   --  Variables in the given file Name, according to the Settings when an
   --  undefined variables is found.
   --
   --  Parameter Name is the name of file with extension "mold", e.g.
   --  "README.md.mold". The name of the generated file is the same without
   --  the "mold" extension, "README.md", even when there are no variables to
   --  replace.
   --
   --  Return the number of errors detected.
   --  ------------------------------------------------------------------------

end File;
