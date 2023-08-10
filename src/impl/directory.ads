-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Definitions;
with Mold;

package Directory is

   --!pp off
   function Replace
   (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Definitions.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Results    :          Mold.Results_Access := null
   )
   return Natural;
   --!pp on
   --
   --  Recursively apply variable substitution to all files with extension
   --  "mold" in all sub-directories, starting at directory Name. Return the
   --  number of errors detected.
   --  ------------------------------------------------------------------------

end Directory;
