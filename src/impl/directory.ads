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

package Directory is

   package Mold renames Lib_Mold;

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
   return Natural; -- *TODO - consider returning a Boolean
   --!pp on
   --
   --  Recursively apply variable replacement to all mold files (with
   --  extension "mold") in all sub-directories, starting at directory Source.
   --  Return the number of errors detected.
   --  ------------------------------------------------------------------------

end Directory;
