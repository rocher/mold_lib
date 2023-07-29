-------------------------------------------------------------------------------
--
--  Mold - Meta-variable Operations for Lean Development (lib)
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Mold;
with Subs;

package File is

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
