-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.Line is

   function Replace
     (Line : String; Number : Natural; Output : IO.File_Type) return String;
   --  Replace all defined variables that appear in Line number Number of
   --  source file, applying all text filters. Return Line with all variable
   --  substitutions made. Some predefined, paragraph-level text filters write
   --  directly to the Output file, and then return the empty String.

end Mold_Lib.Impl.Line;
