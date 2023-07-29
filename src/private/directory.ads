-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;
with Subs;

package Directory is

   function Replace
   --!pp off
   (
      Name      : String;
      Variables : Subs.Variables_Access;
      Settings  : Mold.Settings_Access;
      Results   : Mold.Results_Access
   )
   --!pp on

      return Natural;
   --
   --  Recursively apply variable substitution to all files with extension
   --  "mold" in all sub-directories, starting at directory Name. Return the
   --  number of errors detected.
   --  ------------------------------------------------------------------------

end Directory;
