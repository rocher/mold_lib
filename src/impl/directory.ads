-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;
with Replace;

package Directory is

   function Replace
   --!pp off
   (
      Name      : aliased  String;
      Variables : not null Replace.Variables_Access;
      Settings  : not null Mold.Settings_Access;
      Results   :          Mold.Results_Access := null
   )
   --!pp on
   return Natural;
   --
   --  Recursively apply variable substitution to all files with extension
   --  "mold" in all sub-directories, starting at directory Name. Return the
   --  number of errors detected.
   --  ------------------------------------------------------------------------

end Directory;
