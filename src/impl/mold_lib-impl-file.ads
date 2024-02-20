-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023, 2024 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.File is

  function Replace
   (Source, Output_Dir : not null String_Access) return Boolean;
  --  In the given Source file name, replace all occurrences of mold
  --  variables with the value defined in the Variables map. Behaves
  --  according to the Settings given and updates the Results object.
  --
  --  Parameter source is the name of file with extension "mold", e.g.
  --  "README.md.mold". The name of the generated file is the same without
  --  the "mold" extension, "README.md", even when there are no variables to
  --  replace.
  --
  --  Return True if the process end successfully (no error detected).

end Mold_Lib.Impl.File;
