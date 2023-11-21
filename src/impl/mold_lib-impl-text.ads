-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.Text is

   type Entity_Kind is (file, memory, variable);

   --!pp off
   function Replace (
      Text    :     String;
      Entity  :     Entity_Kind;
      Line    :     Natural;
      Name    :     String;
      Success : out Boolean;
      Output  :     IO.File_Access := null  --  RFU
   ) return String;
   --!pp on
   --  Replace all defined variables that appear in the Text of the given
   --  Entity, a line of a file or a variable. Applies all text filters
   --  present in the variable substitution.
   --
   --  Parameters Entity, Name and Line are used to identify the Entity to
   --  which the Text belongs. If the Text comes from a line of a source file,
   --  then Entity = file and Line is the line number (Name is not used). When
   --  Entity = variable, then Name is the name of the variable (Line is not
   --  used). This information is used to create a descriptive warning or
   --  error message, in case some is found.
   --
   --  Return the new text with all variable substitutions made. Some
   --  predefined paragraph-level text filters are directly written to the
   --  Output file, and then return the empty String.

end Mold_Lib.Impl.Text;
