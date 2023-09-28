-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Definitions;
with Mold_Lib;

package Directory is

   package Mold renames Mold_Lib;

   --!pp off
   function Replace
   (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Definitions.Variables_Access;
      Settings   : not null Mold.Settings_Access;
      Filters    :          Mold.Filters_Access := null;
      Results    :          Mold.Results_Access := null
   )
   return Natural;
   --!pp on
   --
   --  Recursively apply variable replacement to all mold files (with
   --  extension "mold") in all sub-directories, starting at directory Source.
   --
   --  Return the number of errors detected, including those detected during
   --  the replacement process. If Abort_On_Error is False, the number of
   --  errors can be arbitrarily big.
   --  ------------------------------------------------------------------------

end Directory;
