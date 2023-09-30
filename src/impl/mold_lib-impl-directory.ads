-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------
--!pp off

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Mold_Lib.Impl.Definitions;

package Mold_Lib.Impl.Directory is

   function Replace
   (
      Sub_Dir    :          String;
      Source     : not null String_Access;
      Output_Dir : not null String_Access;
      Variables  : not null Definitions.Variables_Access;
      Settings   : not null Settings_Access;
      Filters    :          Filters_Access := null;
      Results    :          Results_Access := null
   )
   return Natural;
   --  Recursively apply variable replacement to all mold files (with
   --  extension "mold") in all sub-directories, starting at directory Source.
   --
   --  Return the number of errors detected, including those detected during
   --  the replacement process. If Abort_On_Error is False, the number of
   --  errors can be arbitrarily big.

end Mold_Lib.Impl.Directory;
