-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.File is

   function Replace
     (Source, Output_Dir : not null String_Access) return Natural;
   --  In the given Source file name, replace all occurrences of mold
   --  variables with the value defined in the Variables map. Behaves
   --  according to the Settings given and updates the Results object.
   --
   --  Parameter source is the name of file with extension "mold", e.g.
   --  "README.md.mold". The name of the generated file is the same without
   --  the "mold" extension, "README.md", even when there are no variables to
   --  replace.
   --
   --  Return the number of errors detected, including those detected during
   --  the replacement process. If Abort_On_Error is False, the number of
   --  errors can be arbitrarily big.

end Mold_Lib.Impl.File;