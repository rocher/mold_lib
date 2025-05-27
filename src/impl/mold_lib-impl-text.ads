-------------------------------------------------------------------------------
--
--  Mold_Lib - Meta-variable Operations for Lean Development
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Mold_Lib.Impl.Text is

   type Entity_Kind is (file, variable);

   type Entity_Type (Kind : Entity_Kind) is record
      case Kind is
         when file =>
            --  File entity, used to identify a line in a source file.
            --  Line is the line number in the file.
            Line : Natural;

         when variable =>
            --  Variable entity, used to identify a variable.
            --  The Name is the variable's name.
            Name : Unbounded_String;
            --  Number of loops to avoid infinite loops in variable
            --  substitution. The Loops is used to limit the number of times a
            --  variable can be substituted in a single call to Replace.
            Loops : Natural := 0;
      end case;
   end record;

   --!pp off
   function Replace (
      Text        : String;
      Entity      : Entity_Type;
      Success     : out Boolean;
      Output      : IO.File_Access := null  --  RFU
   ) return String;
   --!pp on
   --  Replace all defined variables that appear in the Text of the given
   --  Entity, a line of a file or a variable. Applies all text filters
   --  present in the variable substitution.
   --
   --  Parameters Entity, Entity_Name and Line are used to identify the Entity
   --  to which the Text belongs. If the Text comes from a line of a source
   --  file, then Entity = file and Line is the line number (Entity_Name is
   --  not used). When Entity = variable, then Entity_Name is the name of the
   --  variable (Line is not used). This information is used to create a
   --  descriptive warning or error message, in case some is found.
   --
   --  Return the new text with all variable substitutions made. Some
   --  predefined paragraph-level text filters are directly written to the
   --  Output file, and then return the empty String.

end Mold_Lib.Impl.Text;
